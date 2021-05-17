use tgp_ai::{
    rater::{DecisionType, Rater},
    RateAndMap, RatingType,
};
use tgp_board::open_board::OpenIndex;

use crate::state::{HiveContext, HiveGameState};

mod rate_game_state;
mod rate_moves;

pub fn distance(i: OpenIndex, j: OpenIndex) -> u32 {
    ((isize::abs(i.x - j.x) + isize::abs((i.x - j.x) - (i.y - j.y)) + isize::abs(i.y - j.y)) / 2)
        as u32
}

pub struct HiveAI {
    // TODO: weights etc.
}

impl RateAndMap<HiveGameState> for HiveAI {
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
