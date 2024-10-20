use std::fmt::Display;

use crate::{
    ai::{HiveAI, HiveRater},
    pieces::{Piece, Player},
    state::{HiveBoard, HiveContext, HiveGameState},
};

use hivetuilib::engine::Engine;
use hivetuilib_ai::{add_context_to_ratings, rater::Rater, RatingType};
use hivetuilib_board::open_board::OpenIndex;
use hivetuilib_board::{prelude::*, structures::directions::HexaDirection};
type HiveMap<T> = <HiveBoard as BoardToMap<T>>::Map;

pub fn print_move_ratings(
    state: &HiveGameState,
    rater: &HiveRater,
) -> Vec<(RatingType, Box<[usize]>, HiveContext)> {
    let mut engine = Engine::new(2, state.clone());
    let ratings = Rater::create_rating(&mut engine, rater);
    let ratings = add_context_to_ratings(&engine, ratings).unwrap();
    print_ratings_for_moves(state, &ratings);
    ratings
}

pub fn print_ai_ratings(state: &HiveGameState, ai: &HiveAI) {
    let engine = Engine::new(2, state.clone());
    let ratings = ai.run_all_ratings(&engine, || false).unwrap();
    print_ratings_for_moves(state, &ratings);
}

fn print_ratings_for_moves(
    state: &HiveGameState,
    ratings: &[(RatingType, Box<[usize]>, HiveContext)],
) {
    let ratings = ratings
        .iter()
        .map(|(r, indizes, ctx)| (r, *indizes.last().unwrap(), ctx))
        .collect::<Vec<_>>();
    for (r, index, context) in ratings {
        let context = context.clone();
        match context {
            HiveContext::TargetField(context) => {
                let from = state.board().get_field_unchecked(*context.inner());
                let p_type = from.content().top().unwrap().p_type;
                println!(
                    "Move  <{}> from {:<2} to {:<2} => {:>4}",
                    p_type,
                    *context.inner(),
                    context[index],
                    r
                );
            }
            HiveContext::Piece(context) => {
                let (p_type, _) = context[index];
                println!(
                    "Place <{}>  at  {:<2}             => {:>4}",
                    p_type,
                    *context.inner(),
                    r
                );
            }
            HiveContext::SkipPlayer => print!("Skip turn!"),
            HiveContext::BaseField(_) => unreachable!(),
        }
    }
}

// TODO: cut empty space
pub fn print_annotated_board<T: Display>(
    state: &HiveGameState,
    map: &HiveMap<T>,
    print_empty: bool,
    from_field: Option<OpenIndex>,
    to_field: Option<OpenIndex>,
) {
    let board = state.board();
    let num_lines = 4 * board.num_rows() + 2 * board.num_cols() + 1;
    let mut lines = vec![String::new(); num_lines];
    lines[0].push_str("  ");
    let (mut min_line, mut max_line) = (lines.len(), 0);
    for x in 0..board.num_cols() as isize {
        let buffer_y = (board.num_cols() as isize - x) / 2;
        for y in (-buffer_y..board.num_rows() as isize).rev() {
            let index = OpenIndex::from((board.lower_x() + x, board.lower_y() + y));
            let line = (2 * x + 4 * (board.num_rows() as isize - y - 1)) as usize;
            if x == 0 {
                draw_border(&mut lines[line..])
            }
            if board.contains(index) {
                min_line = usize::min(min_line, line);
                max_line = usize::max(max_line, line + 4);
            }
            draw_field(
                board,
                index,
                &mut lines[line..],
                map.get(index),
                x == 0,
                y + 1 == board.num_rows() as isize,
                print_empty,
                from_field,
                to_field,
            );
        }
    }
    for line in &lines[min_line..=max_line] {
        println!("{}", line);
    }
}

fn draw_border(lines: &mut [String]) {
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
    print_empty: bool,
    from_field: Option<OpenIndex>,
    to_field: Option<OpenIndex>,
) {
    assert!(lines.len() >= 5);
    if to_field == Some(index) {
        print_target_field(
            board,
            lines,
            index,
            draw_left,
            print_empty,
            from_field.is_none(),
        );
        return;
    }

    match board.get_field(index) {
        Some(field) if (print_empty || !field.is_empty()) => {
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

            match field.content().bottom() {
                Some(&Piece { player, p_type }) => {
                    let fill = if from_field == Some(index) {
                        '~'
                    } else {
                        fill_char(player, print_empty)
                    };
                    if field.content().len() > 1 {
                        let &Piece {
                            player: player_top,
                            p_type: p_top,
                        } = field.content().get(1).unwrap();
                        let fill_top = if from_field == Some(index) {
                            '~'
                        } else {
                            fill_char(player_top, print_empty)
                        };
                        lines[1].push_str(&format!("{} {} {}", fill_top, p_top, fill_top));
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
                        lines[3].push_str(&format!("{}  {}  {}", fill, p_type, fill));
                    } else {
                        for _ in 0..5 {
                            lines[1].push(fill);
                        }
                        lines[2].push_str(&format!("{}  {}  {}", fill, p_type, fill));
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
        }
        _ => {
            print_empty_field(board, lines, index, annot, draw_left, print_empty);
        }
    }
}

fn print_empty_field<T: Display>(
    board: &HiveBoard,
    lines: &mut [String],
    index: OpenIndex,
    annot: Option<&T>,
    draw_left: bool,
    print_empty: bool,
) {
    if draw_left {
        for l in &mut lines[1..=4] {
            l.push(' ');
        }
    }
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

    print_empty_remaining(board, lines, index, print_empty);
}

fn print_target_field(
    board: &HiveBoard,
    lines: &mut [String],
    index: OpenIndex,
    draw_left: bool,
    print_empty: bool,
    is_placing: bool,
) {
    if draw_left {
        for l in &mut lines[1..=4] {
            l.push(' ');
        }
    }

    if is_placing {
        lines[1].push_str(" \\ / ");
        lines[2].push_str("   X   ");
        lines[3].push_str("  / \\  ");
    } else {
        lines[1].push_str("     ");
        lines[2].push_str(" __|__ ");
        lines[3].push_str("   |   ");
    }

    print_empty_remaining(board, lines, index, print_empty);
}

fn print_empty_remaining(
    board: &HiveBoard,
    lines: &mut [String],
    index: OpenIndex,
    print_empty: bool,
) {
    if board
        .get_field(index + HexaDirection::Down)
        .map_or(false, |f| print_empty || !f.is_empty())
    {
        for _ in 0..5 {
            lines[4].push('_');
        }
    } else {
        lines[4].push_str("     ")
    }
    for l in &mut lines[1..=2] {
        l.push(
            if board
                .get_field(index + HexaDirection::UpRight)
                .map_or(false, |f| print_empty || !f.is_empty())
            {
                '\\'
            } else {
                ' '
            },
        );
    }
    for l in &mut lines[3..=4] {
        l.push(
            if board
                .get_field(index + HexaDirection::DownRight)
                .map_or(false, |f| print_empty || !f.is_empty())
            {
                '/'
            } else {
                ' '
            },
        );
    }
}

fn fill_char(p: Player, print_empty: bool) -> char {
    match p {
        Player::White => '#',
        Player::Black => {
            if print_empty {
                '-'
            } else {
                ' '
            }
        }
    }
}
