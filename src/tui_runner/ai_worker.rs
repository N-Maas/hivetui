use std::{
    panic::catch_unwind,
    sync::{Arc, Mutex},
    thread,
    time::Duration,
};

use tgp::engine::Engine;

use super::{
    get_panic_data, setup_panic_reporting, AIExchange, AIMessageForRunner, AIMessageForWorker,
    AIResult,
};
use crate::ai::{Difficulty, HiveAI};

pub fn start_ai_worker_thread(exchange_point: Arc<Mutex<AIExchange>>) {
    let ais = [
        HiveAI::new(Difficulty::Easy),
        HiveAI::new(Difficulty::QuiteEasy),
        HiveAI::new(Difficulty::Medium),
        HiveAI::new(Difficulty::Hard),
    ];
    thread::spawn(move || loop {
        setup_panic_reporting();

        let result = catch_unwind(|| {
            thread::sleep(Duration::from_millis(10));

            let Some(AIMessageForWorker::Start(level, state)) = ({
                // careful: we need this construction to not accidentially prolong the life of the lock
                let mut exchange = exchange_point.lock().unwrap();
                exchange.for_worker.take()
            }) else {
                return;
            };

            let player = state.player();
            let ai = &ais[usize::from(u8::from(level))];
            let engine = Engine::new(2, *state);
            let Some(mut ratings) = ai.run_all_ratings(&engine, || {
                exchange_point.lock().unwrap().for_worker.is_some()
            }) else {
                return;
            };
            ratings.sort_by_key(|(r, _, _)| -r);
            let (_, best_move, _) = ratings.first().unwrap().clone();

            let mut exchange = exchange_point.lock().unwrap();
            let return_result = match exchange.for_worker.as_ref() {
                Some(AIMessageForWorker::Cancel | AIMessageForWorker::Start(_, _)) => false,
                None => true,
            };
            if return_result {
                exchange.for_runner = Some(AIMessageForRunner::Result(Box::new(AIResult {
                    player,
                    best_move: best_move,
                    all_ratings: ratings,
                    annotations: Default::default(),
                })));
            }
        });
        match result {
            Ok(val) => val, // just propagate I/O error
            Err(_) => {
                let (msg, trace) = get_panic_data();
                exchange_point.lock().unwrap().for_runner =
                    Some(AIMessageForRunner::Killed(msg, trace));
            }
        }
    });
}
