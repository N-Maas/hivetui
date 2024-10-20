use hivetuilib::engine::{Engine, EventListener};
use hivetuilib_ai::{
    add_context_to_ratings,
    rater::{DecisionType, Rater},
    MinMaxAlgorithm, MinMaxError, Params, PruningKind, RateAndMap, RatingType, SlidingParams,
};
use hivetuilib_board::{
    hypothetical::Hypothetical, index_map::ArrayIndexMap, open_board::OpenIndex, prelude::*,
    structures::NeighborhoodStructure,
};

use crate::{
    pieces::{Piece, PieceType, Player},
    state::{HiveBoard, HiveContent, HiveContext, HiveGameState},
};

pub mod rate_game_state;
mod rate_moves;

// for testing purposes
pub use rate_game_state::{print_and_compare_rating, RatingWeights};

pub(crate) fn distance(i: OpenIndex, j: OpenIndex) -> u32 {
    ((isize::abs(i.x - j.x) + isize::abs((i.x - j.x) - (i.y - j.y)) + isize::abs(i.y - j.y)) / 2)
        as u32
}

fn neighbors_in_a_row<B>(field: Field<B>) -> bool
where
    B: Board<Content = HiveContent>,
    B::Structure: NeighborhoodStructure<B>,
{
    let mut prev_empty = false;
    let mut num_components = 0;
    let neighbor_cycle = field.neighbors().chain(field.neighbors().next());
    for f in neighbor_cycle {
        if prev_empty && !f.is_empty() {
            num_components += 1;
        }
        prev_empty = f.is_empty();
    }
    num_components <= 1
}

#[inline(always)]
fn blocks(target: Field<HiveBoard>, blocked: Field<HiveBoard>) -> bool {
    debug_assert!(!target.is_empty());
    debug_assert!(!blocked.is_empty());
    if target.content().len() > 1 || target.content().top().unwrap().p_type == PieceType::Queen {
        return false;
    }

    let mut hypothetical =
        Hypothetical::with_index_map(target.board(), ArrayIndexMap::<_, _, 1>::new());
    hypothetical[target].pop();
    let h_field = hypothetical.get_field_unchecked(blocked.index());
    if h_field
        .content()
        .top()
        .unwrap()
        .p_type
        .is_movable_generic(h_field)
    {
        // Now we need to verify the OHR holds when moving the blocked piece.
        // This is not always correct (and doesn't need to be, for the AI)
        let neighbor_count = h_field.neighbors().filter(|f| !f.is_empty()).count();
        neighbor_count <= 1
            || (neighbor_count <= 4
                // 4 or less neighbors and all neighbors are in a row => movable
                && neighbors_in_a_row(h_field))
            || h_field.content().len() > 1
    } else {
        false
    }
}

fn would_block(target: Field<HiveBoard>, blocked: Field<HiveBoard>) -> bool {
    const DUMMY_PIECE: Piece = Piece {
        player: Player::White,
        p_type: PieceType::Spider,
    };

    debug_assert!(target.is_empty());
    debug_assert!(!blocked.is_empty());
    if blocked.content().len() > 1 {
        return false;
    }

    let mut hypothetical =
        Hypothetical::with_index_map(target.board(), ArrayIndexMap::<_, _, 1>::new());
    hypothetical[target].push(DUMMY_PIECE);
    let h_field = hypothetical.get_field_unchecked(blocked.index());
    if !h_field
        .content()
        .top()
        .unwrap()
        .p_type
        .is_movable_generic(h_field)
    {
        true
    } else {
        // blocking via OHR happens exactly if there is no other neighbor
        target.neighbors().all(|f| f == blocked || f.is_empty())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Difficulty {
    Easy,
    QuiteEasy,
    Medium,
    Hard,
    VeryHard,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Character {
    Balanced,
    Aggressive,
    Defensive,
    Strategic,
}

type Algorithm = MinMaxAlgorithm<HiveGameState, HiveRater>;

pub struct HiveAI {
    alg: Algorithm,
    use_direct_move: bool,
}

impl HiveAI {
    pub fn new(level: Difficulty, character: Character) -> Self {
        let params = match level {
            Difficulty::VeryHard => {
                let sliding = SlidingParams::new(
                    vec![15, 10, 6, 6, 3, 3, 2, 2],
                    vec![50, 35, 22, 22, 16, 16, 16, 16],
                    vec![22, 18, 12, 8, 5, 5, 3, 3, 3, 3],
                    vec![17, 14, 12, 11, 9, 9, 7, 7, 7, 7],
                    vec![50, 10, 3, 3, 2, 2, 1, 1],
                    vec![3, 2, 1, 1, 1, 1, 1, 1],
                );
                Params::new(5, sliding, 5)
            }
            Difficulty::Hard => {
                let sliding = SlidingParams::new(
                    vec![8, 6, 4, 4],
                    vec![50, 30, 20, 18],
                    vec![15, 12, 8, 6, 5, 5],
                    vec![14, 10, 7, 7, 6, 6],
                    vec![50, 3, 2, 2],
                    vec![2, 1, 1, 1],
                );
                Params::new(3, sliding, 5)
            }
            Difficulty::Medium => {
                let sliding = SlidingParams::new(
                    vec![8, 5],
                    vec![40, 20],
                    vec![14, 10, 7, 6],
                    vec![14, 10, 7, 7],
                    vec![50, 3],
                    vec![1, 1],
                );
                Params::new(2, sliding, 4)
            }
            _ => {
                let sliding = SlidingParams::new(
                    vec![3, 2],
                    vec![20, 10],
                    vec![6, 3],
                    vec![8, 4],
                    vec![50, 2],
                    vec![1, 1],
                );
                Params::new(1, sliding, 4)
            }
        };
        let restrictions = match level {
            Difficulty::Easy => vec![
                AIRestriction::DoNotMoveQueen,
                AIRestriction::DoNotCrawlOnTop,
            ],
            Difficulty::QuiteEasy => vec![AIRestriction::DoNotCrawlOnTop],
            _ => vec![],
        };
        let weights = match character {
            Character::Balanced => RatingWeights::default(),
            Character::Aggressive => RatingWeights {
                own_queen: 0.85,
                enemy_queen: 1.2,
                own_movability: 1.1,
                enemy_movability: 0.9,
                own_pieces: 0.9,
                enemy_pieces: 0.9,
            },
            Character::Defensive => RatingWeights {
                own_queen: 1.2,
                enemy_queen: 0.85,
                own_movability: 0.9,
                enemy_movability: 1.1,
                own_pieces: 0.9,
                enemy_pieces: 0.9,
            },
            Character::Strategic => RatingWeights {
                own_queen: 1.0,
                enemy_queen: 0.9,
                own_movability: 1.1,
                enemy_movability: 1.1,
                own_pieces: 1.2,
                enemy_pieces: 1.2,
            },
        };
        let rater = HiveRater {
            restrictions,
            weights,
        };
        let alg = match level {
            Difficulty::VeryHard => Algorithm::with_pruning(params, rater, |input| {
                if input.total_depth <= 2 {
                    match input.current_depth {
                        0 => PruningKind::WithinBounds(5, 10, 30),
                        1 => PruningKind::WithinBounds(3, 6, 22),
                        _ => unreachable!(),
                    }
                } else if input.total_depth <= 4 {
                    match input.current_depth {
                        0 => PruningKind::WithinBounds(3, 7, 20),
                        1 => PruningKind::WithinBounds(2, 4, 16),
                        2 => PruningKind::WithinBounds(1, 3, 14),
                        3 => PruningKind::WithinBounds(1, 3, 14),
                        _ => unreachable!(),
                    }
                } else if input.total_depth <= 6 {
                    match input.current_depth {
                        0 => PruningKind::WithinBounds(2, 5, 16),
                        1 => PruningKind::WithinBounds(2, 2, 14),
                        2 => PruningKind::WithinBounds(1, 2, 12),
                        3 => PruningKind::WithinBounds(1, 2, 12),
                        4 => PruningKind::WithinBounds(1, 2, 12),
                        5 => PruningKind::WithinBounds(1, 2, 12),
                        _ => unreachable!(),
                    }
                } else {
                    unreachable!()
                }
            }),
            Difficulty::Hard => {
                Algorithm::with_pruning(params, rater, |input| match input.current_depth {
                    0 => PruningKind::WithinBounds(3, 7, 30),
                    1 => PruningKind::WithinBounds(2, 5, 18),
                    _ => unreachable!(),
                })
            }
            _ => Algorithm::new(params, rater),
        };
        Self {
            alg,
            use_direct_move: level == Difficulty::Easy,
        }
    }

    pub fn rater(&self) -> &HiveRater {
        self.alg.rate_and_map()
    }

    pub fn run_all_ratings<L: EventListener<HiveGameState>, F: Fn() -> bool>(
        &self,
        engine: &Engine<HiveGameState, L>,
        should_cancel: F,
    ) -> Option<Vec<(RatingType, Box<[usize]>, HiveContext)>> {
        if self.use_direct_move {
            let mut engine = Engine::new(2, engine.data().clone());
            let ratings = Rater::create_rating(&mut engine, self.rater());
            let ratings = add_context_to_ratings(&engine, ratings).unwrap();
            Some(ratings)
        } else {
            match self
                .alg
                .run_all_ratings_with_cancellation(engine, should_cancel)
            {
                Ok(ratings) => Some(ratings),
                Err(MinMaxError::Cancelled) => None,
                Err(e) => panic!("invalid engine state: {:?}", e),
            }
        }
    }

    pub fn run<L: EventListener<HiveGameState>, F: Fn() -> bool>(
        &self,
        engine: &Engine<HiveGameState, L>,
        should_cancel: F,
    ) -> Option<Box<[usize]>> {
        self.run_all_ratings(engine, should_cancel)
            .map(|ratings| ratings.into_iter().max_by_key(|&(r, _, _)| r).unwrap().1)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AIRestriction {
    DoNotMoveQueen,
    DoNotCrawlOnTop,
}

pub struct HiveRater {
    restrictions: Vec<AIRestriction>,
    weights: RatingWeights,
}

impl RateAndMap<HiveGameState> for HiveRater {
    fn apply_type_mapping(&self, context: &HiveContext) -> DecisionType {
        match context {
            HiveContext::BaseField(_) => DecisionType::HigherLevel,
            HiveContext::TargetField(_) => DecisionType::BottomLevel,
            HiveContext::Piece(_) => DecisionType::BottomLevel,
            HiveContext::SkipPlayer => DecisionType::BottomLevel,
        }
    }

    fn rate_moves(
        &self,
        rater: &mut Rater,
        curr_context: &[HiveContext],
        data: &HiveGameState,
        old_context: &[(HiveContext, usize)],
    ) {
        rate_moves::rate_moves(rater, curr_context, data, &self.restrictions, old_context);
    }

    fn rate_game_state(
        &self,
        data: &HiveGameState,
        _old_context: &[(HiveContext, usize)],
        player: usize,
    ) -> RatingType {
        rate_game_state::rate_game_state(data, &self.weights, player)
    }
}
