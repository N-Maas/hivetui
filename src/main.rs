use std::{collections::BTreeMap, str::FromStr};

use display::print_annotated_board;
use pieces::PieceType;
use state::HiveGameState;
use text_io::{read, try_read};
use tgp::board::search::HashIndexMap;
use tgp::board::IndexMap;

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
    let mut state = HiveGameState::new(pieces);
    let mut map = HashIndexMap::new();

    let final_msg = loop {
        map.clear();
        let movables = state.get_all_movables().collect::<Vec<_>>();
        let placables_offset = movables.len();
        let placables = state.get_all_placement_targets().collect::<Vec<_>>();
        for (i, &f) in movables.iter().enumerate() {
            map.insert(f.index(), i);
        }
        for (i, &f) in placables.iter().enumerate() {
            map.insert(f.index(), i + placables_offset);
        }
        print_annotated_board(&state, &map, false);
        if movables.is_empty() && placables.is_empty() {
            println!("Current player can not move, skip turn.");
            continue;
        }

        println!("Next move: ");
        let choice = get_choice(placables.len() + placables_offset);
        let result = if choice < placables_offset {
            map.clear();
            let moves = state
                .get_possible_moves(movables[choice])
                .collect::<Vec<_>>();
            for (i, &f) in moves.iter().enumerate() {
                map.insert(f.index(), i);
            }
            print_annotated_board(&state, &map, false);
            println!("Target: ");
            let move_choice = get_choice(moves.len());
            let (from, to) = (movables[choice].index(), moves[move_choice].index());
            state.move_piece(from, to)
        } else {
            let target = placables[choice - placables_offset].index();
            let available_pieces = state.get_available_pieces();
            let output = available_pieces
                .iter()
                .map(|p| format!("{} ({})", p.to_string(), state.pieces().0[p]))
                .collect::<Vec<_>>();
            println!("Choose piece: {}", output.join(", "));
            let p_type = loop {
                let input: String = read!();
                match PieceType::from_str(&input) {
                    Ok(p) => {
                        if available_pieces.contains(&p) {
                            break p;
                        } else {
                            println!(
                                "{} not available for placement, please try again.",
                                p.to_string()
                            );
                        }
                    }
                    Err(_) => {
                        println!("Please enter an available piece type.");
                    }
                }
            };
            state.place_piece(p_type, target)
        };
        match result {
            Some(s) => break s,
            None => {}
        }
    };
    map.clear();
    print_annotated_board(&state, &map, false);
    println!("{}", final_msg);
}

fn get_choice(num_indizes: usize) -> usize {
    loop {
        let input: Result<usize, _> = try_read!();
        match input {
            Ok(idx) => {
                if idx < num_indizes {
                    return idx;
                } else {
                    println!("Invalid index, please try again.");
                }
            }
            Err(_) => {
                println!("Please enter a number.");
            }
        }
    }
}
