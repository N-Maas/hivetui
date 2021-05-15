use std::collections::BTreeMap;

use either::Either;
use tgp::engine::{Engine, GameEngine, GameState};
use tgp_board::{index_map::HashIndexMap, prelude::*};

use display::print_annotated_board;
use pieces::PieceType;
use state::{HiveContext, HiveGameState};
use text_io::try_read;

mod ai;
mod display;
mod pieces;
mod state;

fn main() {
    let mut pieces = BTreeMap::new();
    pieces.insert(PieceType::Queen, 1);
    pieces.insert(PieceType::Ant, 3);
    pieces.insert(PieceType::Grasshopper, 3);
    pieces.insert(PieceType::Beetle, 2);
    pieces.insert(PieceType::Spider, 2);
    let mut engine = Engine::new_logging(2, HiveGameState::new(pieces));
    let mut map = HashIndexMap::new();
    let mut print = true;

    loop {
        map.clear();
        match engine.pull() {
            GameState::PendingEffect(pe) => {
                pe.next_effect();
            }
            GameState::PendingDecision(mut decision) => {
                let context = decision.context();
                let (to_iter, is_top_level) = match &context {
                    HiveContext::BaseField(context) => (Either::Left(context.iter()), true),
                    HiveContext::TargetField(context) => (Either::Left(context.iter()), false),
                    HiveContext::Piece(context) => (Either::Right(context), false),
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
                        if print {
                            print_annotated_board(&decision.data(), &map, false);
                            if is_top_level {
                                println!("([u]ndo, [r]edo)");
                            }
                        } else {
                            print = true;
                        }
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
                match get_input(is_top_level) {
                    Input::Choose(index) => {
                        decision.select_option(index);
                    }
                    Input::Undo => {
                        if !decision.undo_last_decision() {
                            println!("There is nothing to undo currently.");
                            print = false;
                        }
                    }
                    Input::Redo => {
                        if !decision.redo_decision() {
                            println!("There is nothing to redo.");
                            print = false;
                        }
                    }
                    Input::Cancel => {
                        decision
                            .into_follow_up_decision()
                            .expect("Error, invalid state.")
                            .retract();
                    }
                }
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

enum Input {
    Choose(usize),
    Undo,
    Redo,
    Cancel,
}

fn get_input(is_top_level: bool) -> Input {
    loop {
        let input: Result<String, _> = try_read!();
        match input {
            Ok(mut val) => {
                let to_num = val.parse::<usize>();
                match to_num {
                    Ok(index) => {
                        return Input::Choose(index);
                    }
                    Err(_) => {
                        if is_top_level {
                            val.make_ascii_lowercase();
                            if val.starts_with('u') {
                                return Input::Undo;
                            } else if val.starts_with('r') {
                                return Input::Redo;
                            } else {
                                println!("Please enter a number.");
                            }
                        } else {
                            return Input::Cancel;
                        }
                    }
                }
            }
            Err(_) => {
                println!("Invalid input, please try again.");
            }
        }
    }
}
