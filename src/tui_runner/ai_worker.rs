use std::{
    sync::{Arc, Mutex},
    thread,
    time::Duration,
};

use tgp::engine::Engine;

use crate::ai::{Difficulty, HiveAI};

use super::{AIExchange, AIMessage, AIResult};

pub fn start_ai_worker_thread(exchange_point: Arc<Mutex<AIExchange>>) {
    let ais = [
        HiveAI::new(Difficulty::Easy),
        HiveAI::new(Difficulty::QuiteEasy),
        HiveAI::new(Difficulty::Medium),
        HiveAI::new(Difficulty::Hard),
    ];
    thread::spawn(move || loop {
        thread::sleep(Duration::from_millis(10));

        let Some(AIMessage::Start(level, state)) = ({
            // careful: we need this construction to not accidentially prolong the life of the lock
            let mut exchange = exchange_point.lock().unwrap();
            exchange.for_worker.take()
        }) else {
            continue;
        };

        let player = state.player();
        let ai = &ais[usize::from(u8::from(level))];
        let engine = Engine::new(2, *state);
        let Some(ratings) = ai.run_all_ratings(&engine, || {
            exchange_point.lock().unwrap().for_worker.is_some()
        }) else {
            continue;
        };
        let (_, best_move, _) = ratings.iter().max_by_key(|(r, _, _)| r).unwrap();

        let mut exchange = exchange_point.lock().unwrap();
        let return_result = match exchange.for_worker.as_ref() {
            Some(AIMessage::Cancel | AIMessage::Start(_, _)) => false,
            None => true,
        };
        if return_result {
            exchange.for_runner = Some(Box::new(AIResult {
                player,
                best_move: best_move.clone(),
                all_ratings: ratings,
            }));
        }
    });
}
