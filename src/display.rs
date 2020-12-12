use std::fmt::Display;

use crate::{
    pieces::{Piece, Player},
    state::{HiveBoard, HiveGameState},
};

use tgp::board::{directions::HexaDirection, open_board::OpenIndex, Board, BoardToMap, IndexMap};
type HiveMap<T> = <HiveBoard as BoardToMap<T>>::Map;

pub fn print_annotated_board<T: Display>(state: &HiveGameState, map: &HiveMap<T>) {
    let board = state.board();
    let num_lines = 4 * board.num_rows() + 2 * board.num_cols() + 1;
    let mut lines = vec![String::new(); num_lines];
    lines[0].push_str("  ");
    for x in 0..board.num_cols() as isize {
        let buffer_y = (board.num_cols() as isize - x) / 2;
        for y in (-buffer_y..board.num_rows() as isize).rev() {
            let index = OpenIndex::from((board.lower_x() + x, board.lower_y() + y));
            let line = (2 * x + 4 * (board.num_rows() as isize - y - 1)) as usize;
            if x == 0 {
                draw_boarder(&mut lines[line..])
            }
            draw_field(
                &board,
                index,
                &mut lines[line..],
                map.get(index),
                x == 0,
                y + 1 == board.num_rows() as isize,
            );
        }
    }
    lines.pop();
    lines.pop();
    for line in lines {
        println!("{}", line);
    }
}

fn draw_boarder(lines: &mut [String]) {
    lines[1].push(' ');
    lines[4].push(' ');
}

fn draw_field<T: Display>(
    board: &HiveBoard,
    index: OpenIndex,
    lines: &mut [String],
    annot: Option<&T>,
    draw_left: bool,
    draw_top: bool,
) {
    assert!(lines.len() >= 5);
    if let Some(field) = board.get_field(index) {
        if draw_top {
            lines[0].push_str("_____");
        }
        if draw_left {
            for l in &mut lines[1..=2] {
                l.push('/');
            }
            for l in &mut lines[3..=4] {
                l.push('\\');
            }
        }

        match field.content().first() {
            Some(&Piece { player, p_type }) => {
                let fill = fill_char(player);
                if field.content().len() > 1 {
                    let Piece {
                        player: player_top,
                        p_type: p_top,
                    } = field.content()[1];
                    let fill_top = fill_char(player_top);
                    lines[1].push_str(&format!("{} {} {}", fill_top, p_top.to_string(), fill_top));
                    match annot {
                        Some(val) => {
                            let label = format!("[{}]", val);
                            lines[2].push_str(&format!("{}{:^5}{}", fill_top, label, fill_top));
                        }
                        None => {
                            for _ in 0..7 {
                                lines[2].push(fill_top);
                            }
                        }
                    }
                    lines[3].push_str(&format!("{}  {}  {}", fill, p_type.to_string(), fill));
                } else {
                    for _ in 0..5 {
                        lines[1].push(fill);
                    }
                    lines[2].push_str(&format!("{}  {}  {}", fill, p_type.to_string(), fill));
                    match annot {
                        Some(val) => {
                            let label = format!("[{}]", val);
                            lines[3].push_str(&format!("{}{:^5}{}", fill, label, fill));
                        }
                        None => {
                            for _ in 0..7 {
                                lines[3].push(fill);
                            }
                        }
                    }
                }
            }
            None => {
                for (i, &num) in [5, 7].iter().enumerate() {
                    for _ in 0..num {
                        lines[i + 1].push(' ');
                    }
                }
                match annot {
                    Some(val) => {
                        let label = format!("[{}]", val);
                        lines[3].push_str(&format!("{:^7}", label));
                    }
                    None => lines[3].push_str("       "),
                }
            }
        }

        for _ in 0..5 {
            lines[4].push('_');
        }
        for l in &mut lines[1..=2] {
            l.push('\\');
        }
        for l in &mut lines[3..=4] {
            l.push('/');
        }
    } else {
        if draw_left {
            for l in &mut lines[1..=4] {
                l.push(' ');
            }
        }
        for (i, &num) in [5, 7, 7].iter().enumerate() {
            for _ in 0..num {
                lines[i + 1].push(' ');
            }
        }

        if board.contains(index + HexaDirection::Down) {
            for _ in 0..5 {
                lines[4].push('_');
            }
        } else {
            lines[4].push_str("     ")
        }
        for l in &mut lines[1..=2] {
            l.push(if board.contains(index + HexaDirection::UpRight) {
                '\\'
            } else {
                ' '
            });
        }
        for l in &mut lines[3..=4] {
            l.push(if board.contains(index + HexaDirection::DownRight) {
                '/'
            } else {
                ' '
            });
        }
    }
}

fn fill_char(p: Player) -> char {
    match p {
        Player::White => '#',
        Player::Black => '-',
    }
}
