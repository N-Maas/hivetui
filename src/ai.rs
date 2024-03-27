use tgp::engine::{Engine, EventListener, GameEngine};
use tgp_ai::{
    add_context_to_ratings,
    rater::{DecisionType, Rater},
    MinMaxAlgorithm, MinMaxError, Params, PruningKind, RateAndMap, RatingType, SlidingParams,
};
use tgp_board::{
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
pub use rate_game_state::print_and_compare_rating;

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

type Algorithm = MinMaxAlgorithm<HiveGameState, HiveRater>;

pub struct HiveAI {
    alg: Algorithm,
    use_direct_move: bool,
}

impl HiveAI {
    pub fn new(level: Difficulty) -> Self {
        let params = match level {
            Difficulty::VeryHard => {
                let sliding = SlidingParams::new(
                    vec![15, 10, 6, 6, 3, 3, 2, 2],
                    vec![50, 35, 22, 22, 16, 16, 16, 16],
                    vec![24, 20, 10, 8, 5, 5, 3, 3, 3, 3],
                    vec![17, 14, 9, 9, 7, 7, 6, 6, 6, 6],
                    vec![50, 10, 3, 3, 2, 2, 1, 1],
                );
                Params::new(5, sliding)
            }
            Difficulty::Hard => {
                let sliding = SlidingParams::new(
                    vec![8, 6, 4, 4],
                    vec![50, 30, 20, 18],
                    vec![15, 12, 8, 6, 5, 5],
                    vec![14, 10, 7, 7, 6, 6],
                    vec![50, 3, 2, 2],
                );
                Params::new(3, sliding)
            }
            Difficulty::Medium => {
                let sliding = SlidingParams::new(
                    vec![8, 5],
                    vec![40, 20],
                    vec![12, 8, 6, 6],
                    vec![14, 9, 7, 7],
                    vec![50, 3],
                );
                Params::new(2, sliding)
            }
            _ => {
                // TODO: make this AI easier??
                let sliding = SlidingParams::new(
                    vec![3, 2],
                    vec![20, 10],
                    vec![6, 3],
                    vec![8, 4],
                    vec![50, 2],
                );
                Params::new(1, sliding)
            }
        };
        let alg = match level {
            Difficulty::VeryHard => Algorithm::with_pruning(params, HiveRater {}, |input| {
                if input.total_depth <= 2 {
                    match input.current_depth {
                        0 => PruningKind::WithinBounds(5, 9, 30),
                        1 => PruningKind::WithinBounds(3, 6, 22),
                        _ => unreachable!(),
                    }
                } else if input.total_depth <= 4 {
                    match input.current_depth {
                        0 => PruningKind::WithinBounds(3, 5, 20),
                        1 => PruningKind::WithinBounds(2, 4, 14),
                        2 => PruningKind::WithinBounds(1, 3, 12),
                        3 => PruningKind::WithinBounds(1, 3, 12),
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
                Algorithm::with_pruning(params, HiveRater {}, |input| match input.current_depth {
                    0 => PruningKind::WithinBounds(3, 7, 30),
                    1 => PruningKind::WithinBounds(2, 5, 18),
                    _ => unreachable!(),
                })
            }
            _ => Algorithm::new(params, HiveRater {}),
        };
        Self {
            alg,
            use_direct_move: level == Difficulty::Easy,
        }
    }

    pub fn run_all_ratings<L: EventListener<HiveGameState>, F: Fn() -> bool>(
        &self,
        engine: &Engine<HiveGameState, L>,
        should_cancel: F,
    ) -> Option<Vec<(RatingType, Box<[usize]>, HiveContext)>> {
        if self.use_direct_move {
            let mut engine = Engine::new(2, engine.data().clone());
            let ratings = Rater::create_rating(&mut engine, &HiveRater {});
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

pub struct HiveRater {
    // TODO: weights etc.
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
        rate_moves::rate_moves(rater, curr_context, data, old_context);
    }

    fn rate_game_state(
        &self,
        data: &HiveGameState,
        _old_context: &[(HiveContext, usize)],
        player: usize,
    ) -> RatingType {
        rate_game_state::rate_game_state(data, player)
    }
}
