use crate::ai::{Character, Difficulty, HiveAI};
use crate::{
    pieces::{PieceType, Player},
    state::{HiveContext, HiveGameState},
};
use hivetuilib::engine::Engine;
use hivetuilib_ai::RatingType;
use hivetuilib_board::open_board::OpenIndex;
use std::{collections::HashMap, thread, time::Duration};

use super::{start_worker_thread, MasterEndpoint};

#[derive(Debug)]
pub struct AIResult {
    pub player: Player,
    pub best_move: Box<[usize]>,
    pub all_ratings: Vec<(RatingType, Box<[usize]>, HiveContext)>,
    pub annotations: HashMap<OpenIndex, Vec<(usize, Option<PieceType>)>>,
}

#[derive(Debug)]
pub struct AIStart(pub Difficulty, pub Character, pub Box<HiveGameState>);

pub type AIEndpoint = MasterEndpoint<Box<AIResult>, AIStart>;

pub fn start_ai_worker_thread() -> AIEndpoint {
    start_worker_thread(|endpoint| loop {
        thread::sleep(Duration::from_millis(10));

        let Some(AIStart(level, character, state)) = endpoint.get_new_msg() else {
            continue;
        };

        let player = state.player();
        let ai = HiveAI::new(level, character);
        let engine = Engine::new(2, *state);
        let Some(mut ratings) = ai.run_all_ratings(&engine, || endpoint.has_msg()) else {
            continue;
        };
        ratings.sort_by_key(|(r, _, _)| -r);
        let (_, best_move, _) = ratings.first().unwrap().clone();

        endpoint.send_if_no_msg(Box::new(AIResult {
            player,
            best_move,
            all_ratings: ratings,
            annotations: Default::default(),
        }));
    })
}
