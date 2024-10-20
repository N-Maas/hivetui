use std::{collections::BTreeMap, thread::sleep, time::Duration};

use either::Either;
use hivetuilib::engine::{Engine, EventListener, GameState, PendingDecision};
use hivetuilib_board::{index_map::HashIndexMap, prelude::*};

use crate::ai::Character;
use crate::display::print_annotated_board;
use crate::pieces::PieceType;
use crate::state::{HiveContext, HiveGameState};
use text_io::try_read;

use crate::{
    ai::{print_and_compare_rating, Difficulty, HiveAI},
    display::{print_ai_ratings, print_move_ratings},
    pieces::Player,
};

pub fn run_in_cli(pieces: BTreeMap<PieceType, u32>) {
    let mut ais = [
        HiveAI::new(Difficulty::Easy, Character::Balanced),
        HiveAI::new(Difficulty::Easy, Character::Balanced),
    ];
    for &player in [Player::White, Player::Black].iter() {
        println!("Choose level for {} AI: [0] [1] [2] [3] [4]", player);
        let level = loop {
            let input: Result<String, _> = try_read!();
            if let Ok(val) = input {
                if let Ok(index) = val.parse::<usize>() {
                    if index < 5 {
                        break index;
                    }
                }
            }
            println!("Please enter a number between 0 and 4.");
        };
        let level = match level {
            0 => Difficulty::Easy,
            1 => Difficulty::QuiteEasy,
            2 => Difficulty::Medium,
            3 => Difficulty::Hard,
            4 => Difficulty::VeryHard,
            _ => unreachable!(),
        };
        ais[usize::from(player)] = HiveAI::new(level, Character::Balanced);
    }

    let mut engine = Engine::new_logging(2, HiveGameState::new(pieces));
    let mut map = HashIndexMap::new();
    let mut print = true;
    let mut undo = false;

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
                        if undo {
                            decision.undo_last_decision();
                        } else {
                            println!("Current player can not move, skip turn.");
                            decision.select_option(0);
                        }
                        continue;
                    }
                };
                undo = false;
                match to_iter {
                    Either::Left(iter) => {
                        for (i, &index) in iter.enumerate() {
                            map.insert(index, i);
                        }
                        if print {
                            print_annotated_board(decision.data(), &map, false, None, None);
                            if is_top_level {
                                println!("([u]ndo, [r]edo, [a]i)");
                            }
                        } else {
                            print = true;
                        }
                    }
                    Either::Right(context) => {
                        let mapped = context
                            .iter()
                            .enumerate()
                            .map(|(i, &(p, count))| format!("[{}] {} ({})", i, p, count))
                            .collect::<Vec<_>>();
                        println!("Choose piece: {}", mapped.join(", "));
                    }
                }
                match get_input(is_top_level) {
                    Input::Choose(index) => {
                        if index < decision.option_count() {
                            decision.select_option(index);
                        } else {
                            println!("Invalid number, please try again!");
                        }
                    }
                    Input::Undo => {
                        if !decision.undo_last_decision() {
                            println!("There is nothing to undo currently.");
                            print = false;
                        } else {
                            undo = true;
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
                    Input::StateRating => {
                        print_and_compare_rating(engine.data(), None);
                    }
                    Input::MoveRating => {
                        print_move_ratings(
                            engine.data(),
                            ais[usize::from(engine.data().player())].rater(),
                        );
                    }
                    Input::ShowAIRating => {
                        print_ai_ratings(engine.data(), &ais[usize::from(engine.data().player())]);
                    }
                    Input::AI => {
                        let ai = &ais[usize::from(engine.data().player())];
                        let path = ai.run(&engine, || false).unwrap();
                        pull_decision(&mut engine).select_option(path[0]);
                        let subdec = pull_decision(&mut engine);
                        let (from_field, to_field) = match subdec.context() {
                            HiveContext::TargetField(context) => {
                                (Some(*context.inner()), context[path[1]])
                            }
                            HiveContext::Piece(context) => (None, *context.inner()),
                            _ => unreachable!(),
                        };
                        map.clear();
                        print_annotated_board(
                            subdec.data(),
                            &map,
                            false,
                            from_field,
                            Some(to_field),
                        );
                        // keep height (minimize movement)
                        println!();
                        println!();
                        subdec.select_option(path[1]);
                        sleep(Duration::from_secs(2));
                    }
                    _ => {}
                }
            }
            GameState::Finished(mut f) => {
                map.clear();
                print_annotated_board(f.data(), &map, false, None, None);
                println!("{}", f.data().result().unwrap());

                println!("([u]ndo, [e]xit)");
                match get_input(true) {
                    Input::Undo => {
                        f.undo_last_decision();
                        undo = true;
                    }
                    Input::Exit => {
                        break;
                    }
                    _ => {}
                }
            }
        }
    }
}

fn pull_decision<L: EventListener<HiveGameState>>(
    engine: &mut Engine<HiveGameState, L>,
) -> PendingDecision<HiveGameState, L> {
    match engine.pull() {
        GameState::PendingDecision(dec) => dec,
        _ => unreachable!(),
    }
}

enum Input {
    Choose(usize),
    Undo,
    Redo,
    Cancel,
    MoveRating,
    StateRating,
    AI,
    ShowAIRating,
    Exit,
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
                            } else if val.starts_with('s') {
                                return Input::StateRating;
                            } else if val.starts_with('m') {
                                return Input::MoveRating;
                            } else if val.starts_with('a') {
                                return Input::AI;
                            } else if val.starts_with('h') {
                                return Input::ShowAIRating;
                            } else if val.starts_with('e') {
                                return Input::Exit;
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
