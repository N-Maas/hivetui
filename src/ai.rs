use tgp::engine::{Engine, EventListener, GameEngine, GameState};
use tgp_ai::{
    rater::{DecisionType, Rater},
    MinMaxAlgorithm, Params, RateAndMap, RatingType,
};
use tgp_board::{
    hypothetical::Hypothetical, index_map::ArrayIndexMap, open_board::OpenIndex, prelude::*,
    structures::directions::DirectionEnumerable,
};

use crate::{
    pieces::{Piece, PieceType, Player},
    state::{HiveBoard, HiveContext, HiveGameState},
};

pub mod rate_game_state;
mod rate_moves;

// for testing purposes
pub use rate_game_state::print_and_compare_rating;

fn distance(i: OpenIndex, j: OpenIndex) -> u32 {
    ((isize::abs(i.x - j.x) + isize::abs((i.x - j.x) - (i.y - j.y)) + isize::abs(i.y - j.y)) / 2)
        as u32
}

fn blocks(target: Field<HiveBoard>, blocked: Field<HiveBoard>) -> bool {
    debug_assert!(!target.is_empty());
    debug_assert!(!blocked.is_empty());
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
                && h_field.neighbors_by_direction().all(|(d, f)| {
                    f.is_empty()
                        || !h_field.next(d.next_direction()).unwrap().is_empty()
                        || !h_field.next(d.prev_direction()).unwrap().is_empty()
                }))
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
}

pub struct HiveAI {
    alg: MinMaxAlgorithm<HiveGameState, HiveRater>,
    use_direct_move: bool,
}

impl HiveAI {
    pub fn new(level: Difficulty) -> Self {
        let depth = if level == Difficulty::Medium { 2 } else { 1 };
        let params = Params::new(depth, 4, 6, 20, 7);
        let alg = MinMaxAlgorithm::<HiveGameState, HiveRater>::new(params, HiveRater {});
        Self {
            alg,
            use_direct_move: level == Difficulty::Easy,
        }
    }

    pub fn apply<L: EventListener<HiveGameState>>(&self, engine: &mut Engine<HiveGameState, L>) {
        if self.use_direct_move {
            let rating = Rater::create_rating(engine, &HiveRater {});
            let (_, indizes) = rating
                .into_iter()
                .max_by(|(val1, _), (val2, _)| val1.cmp(val2))
                .unwrap();
            for &i in indizes.iter() {
                match engine.pull() {
                    GameState::PendingDecision(dec) => {
                        dec.select_option(i);
                    }
                    _ => unreachable!(),
                }
            }
        } else {
            self.alg.apply(engine);
        }
    }

    pub fn run_all_ratings<L: EventListener<HiveGameState>>(
        &self,
        engine: &Engine<HiveGameState, L>,
    ) -> Vec<(RatingType, Box<[usize]>)> {
        if self.use_direct_move {
            let mut engine = Engine::new(2, engine.data().clone());
            Rater::create_rating(&mut engine, &HiveRater {})
        } else {
            self.alg.run_all_ratings(engine).unwrap()
        }
    }

    pub fn run<L: EventListener<HiveGameState>>(
        &self,
        engine: &Engine<HiveGameState, L>,
    ) -> Box<[usize]> {
        if self.use_direct_move {
            let mut engine = Engine::new(2, engine.data().clone());
            Rater::create_rating(&mut engine, &HiveRater {})
                .into_iter()
                .max_by(|(a, _), (b, _)| a.cmp(b))
                .unwrap()
                .1
        } else {
            self.alg.run(engine).unwrap().1
        }
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
