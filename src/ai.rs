use tgp_ai::{
    rater::{DecisionType, Rater},
    RateAndMap, RatingType,
};

use crate::state::{HiveContext, HiveGameState};

mod rate_moves;

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
        old_context: &[(HiveContext, usize)],
        player: usize,
    ) -> RatingType {
        todo!()
    }
}
