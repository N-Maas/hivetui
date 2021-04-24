use std::collections::BTreeMap;

use either::Either;
use tgp::engine::{Engine, GameEngine, GameState, PendingDecision};
use tgp_board::{prelude::*, search::HashIndexMap};

use display::print_annotated_board;
use pieces::PieceType;
use state::{HiveContext, HiveGameState};
use text_io::try_read;

pub mod display;
pub mod pieces;
pub mod state;

fn main() {
    let mut pieces = BTreeMap::new();
    pieces.insert(PieceType::Queen, 1);
    pieces.insert(PieceType::Ant, 3);
    pieces.insert(PieceType::Grasshopper, 3);
    pieces.insert(PieceType::Beetle, 2);
    pieces.insert(PieceType::Spider, 2);
    let mut engine = Engine::new(2, HiveGameState::new(pieces));
    let mut map = HashIndexMap::new();

    loop {
        map.clear();
        match engine.pull() {
            GameState::PendingEffect(pe) => {
                pe.next_effect();
            }
            GameState::PendingDecision(decision) => {
                let context = decision.context();
                let to_iter = match &context {
                    HiveContext::BaseField(context) => Either::Left(context.iter()),
                    HiveContext::TargetField(context) => Either::Left(context.iter()),
                    HiveContext::Piece(context) => Either::Right(context),
                    HiveContext::SkipPlayer => {
                        println!("Current player can not move, skip turn.");
                        decision.select_option(0);
                        continue;
                    }
                };
                match to_iter {
                    Either::Left(iter) => {
                        for (i, &index) in iter.enumerate() {
                            map.insert(index, i);
                        }
                        print_annotated_board(&decision.data(), &map, false);
                    }
                    Either::Right(context) => {
                        let mapped = context
                            .iter()
                            .enumerate()
                            .map(|(i, &(p, count))| {
                                format!("[{}] {} ({})", i, p.to_string(), count)
                            })
                            .collect::<Vec<_>>();
                        println!("Choose piece: {}", mapped.join(", "));
                    }
                }
                choose(decision);
            }
            GameState::Finished(_) => {
                break;
            }
        }
    }

    map.clear();
    print_annotated_board(&engine.data(), &map, false);
    println!("{}", engine.data().result().unwrap());
}

fn choose(decision: PendingDecision<HiveGameState>) {
    loop {
        let input: Result<usize, _> = try_read!();
        match input {
            Ok(index) => {
                if index < decision.option_count() {
                    decision.select_option(index);
                    return;
                } else {
                    println!("Invalid index, please try again.");
                }
            }
            Err(_) => {
                if decision.is_follow_up_decision() {
                    let follow_up = decision.into_follow_up_decision().unwrap();
                    follow_up.retract();
                    println!("Canceled.");
                    return;
                } else {
                    println!("Please enter a number.");
                }
            }
        }
    }
}
