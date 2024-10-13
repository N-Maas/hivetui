use ratatui::{style::Color, text::Line, widgets::canvas::Context};
use tgp_board::{
    open_board::OpenIndex,
    structures::directions::{DirectionEnumerable, HexaDirection},
    Board,
};

use crate::{
    pieces::{PieceType, Player},
    state::{HiveBoard, HiveContent, HiveResult},
    tui_graphics,
    tui_runner::{
        game_setup::GameSetup,
        tui_animations::{AnimationContext, Layer},
        tui_settings::{BordersStyle, ColorScheme, DarkTileColor, FillingStyle, InputMode},
        UIState,
    },
};

use super::{color_palette, translate_index, AllState};

fn draw_level_of_board(
    ctx: &mut Context<'_>,
    state: AllState<'_>,
    anim_ctx: AnimationContext<'_>,
    board: &HiveBoard,
    level: usize,
) {
    assert!(level <= 2);
    let zoom = state.graphics_state.zoom_level.multiplier();
    let animation = state.animation_state.animation();
    let get_piece = |content: &HiveContent| {
        if level >= 2 {
            if content.len() > 2 {
                content.top().copied()
            } else {
                None
            }
        } else {
            content.get(level).copied()
        }
    };
    let scale = match level {
        0 => 1.0,
        1 => 0.7,
        _ => 0.5,
    };

    // draw interiors
    for field in board.iter_fields() {
        let (x_mid, y_mid) = translate_index(field.index());
        field
            .content_checked()
            .and_then(get_piece)
            .inspect(|piece| {
                let is_white = piece.player == Player::White;
                if is_white || state.settings.dark_tile_color != DarkTileColor::Black || level > 0 {
                    let color = if is_white {
                        ColorScheme::DARK_WHITE
                    } else {
                        state.settings.dark_tile_color.color()
                    };
                    if level == 0 {
                        draw_interior(ctx, state.settings.filling_style, x_mid, y_mid, color);
                    } else {
                        let y_offset = (1.0 - scale) * 10.0 + level as f64 * 0.5;
                        tui_graphics::draw_hex_interior(
                            ctx,
                            x_mid,
                            y_mid + y_offset,
                            color,
                            false,
                            scale,
                        );
                    }
                }
            });
    }
    animation.inspect(|a| a.draw(ctx, anim_ctx, Layer::Interiors(level)));
    ctx.layer();

    // draw pieces
    for field in board.iter_fields() {
        let (x_mid, mut y_mid) = translate_index(field.index());
        y_mid += level as f64 * 0.3;
        field
            .content_checked()
            .and_then(|content| get_piece(content).map(|p| (p, content.len())))
            .inspect(|&(piece, stack_size)| match level {
                0 => {
                    let y_mid = if stack_size == 1 {
                        y_mid
                    } else {
                        y_mid - tui_graphics::to_bottom_offset(piece.p_type)
                    };
                    tui_graphics::draw_piece(ctx, piece.p_type, x_mid, y_mid, zoom)
                }
                1 => tui_graphics::draw_small_piece(ctx, piece.p_type, x_mid, y_mid, zoom),
                _ => tui_graphics::draw_tiny_piece(ctx, piece.p_type, x_mid, y_mid, zoom),
            });
    }
    animation.inspect(|a| a.draw(ctx, anim_ctx, Layer::Pieces(level)));
    ctx.layer();
}

pub fn draw_board(
    ctx: &mut Context<'_>,
    state: AllState<'_>,
    x_bounds: [f64; 2],
    y_bounds: [f64; 2],
) {
    let zoom = state.graphics_state.zoom_level.multiplier();
    let animation = state.animation_state.animation();
    let anim_ctx = AnimationContext {
        graphics_state: &state.graphics_state,
        x_bounds,
        y_bounds,
    };
    let temporary_state = animation.and_then(|a| a.get_temporary_state(state.game_state));
    let board = temporary_state
        .as_ref()
        .map_or(state.game_state.board(), |s| s.board());

    // first round: draw borders
    for field in board.iter_fields() {
        let (x_mid, y_mid) = translate_index(field.index());
        if let Some(content) = field.content_checked() {
            if content.is_empty()
                && state.settings.borders_style == BordersStyle::Partial
                && board.size() > 1
            {
                // we don't want to draw "lonely" borders, thus we check where tiles are adjacent
                let mut to_draw = Vec::new();
                for direction in HexaDirection::enumerate_all() {
                    if let Some(next) = field.next(direction).and_then(|f| f.content_checked()) {
                        if !next.is_empty() {
                            to_draw.push(direction.prev_direction());
                            to_draw.push(direction);
                            to_draw.push(direction.next_direction());
                        }
                    }
                }
                to_draw.sort();
                to_draw.dedup();
                tui_graphics::draw_restricted_hex_border(
                    ctx,
                    x_mid,
                    y_mid,
                    ColorScheme::GRAY,
                    &to_draw,
                );
            } else if state.settings.borders_style != BordersStyle::None {
                tui_graphics::draw_hex_border(ctx, x_mid, y_mid, ColorScheme::GRAY);
            }
            // which sides should be drawn?
        }
    }
    animation.inspect(|a| a.draw(ctx, anim_ctx, Layer::Borders));
    ctx.layer();

    // draw the three levels (bottom, stacked, top)
    draw_level_of_board(ctx, state, anim_ctx, board, 0);
    draw_level_of_board(ctx, state, anim_ctx, board, 1);
    draw_level_of_board(ctx, state, anim_ctx, board, 2);

    // is a specific field selected?
    let primary_color = state.settings.color_scheme.primary();
    match state.ui_state {
        UIState::PositionSelected(index) | UIState::PieceSelected(index) => {
            let (x_mid, y_mid) = translate_index(index);
            let stack_size = board[index].len();
            let level = if stack_size <= 1 { 0 } else { stack_size - 1 };
            tui_graphics::draw_interior_hex_border_lvl(
                ctx,
                x_mid,
                y_mid,
                0.0,
                0.0,
                primary_color,
                level,
            );
        }
        UIState::GameFinished(result, _) => {
            if let Some((index, _)) = find_losing_queen(board, result) {
                let (x_mid, y_mid) = translate_index(index);
                tui_graphics::draw_interior_hex_border(ctx, x_mid, y_mid, 0.0, 0.0, primary_color);
            }
        }
        _ => (),
    }
    animation.inspect(|a| a.draw(ctx, anim_ctx, Layer::Selection));
    ctx.layer();

    animation.inspect(|a| a.draw(ctx, anim_ctx, Layer::Final));

    // print indizes
    if matches!(
        state.ui_state,
        UIState::ShowOptions(false, _) | UIState::PieceSelected(_)
    ) {
        let color = if matches!(state.ui_state, UIState::ShowOptions(false, _)) {
            state.settings.color_scheme.primary()
        } else {
            state.settings.color_scheme.secondary()
        };
        for (&board_index, &number) in state.board_annotations.iter() {
            let (x, y) = translate_index(board_index);
            if number >= 9 {
                let (marker, offset) = if state.settings.input_mode == InputMode::Direct {
                    (format!("[ {}]", number + 1), 4.0)
                } else {
                    (format!("[{}]", number + 1), 2.5)
                };
                ctx.print(
                    x - zoom * offset,
                    y - zoom * 2.0,
                    Line::styled(marker, color),
                );
            } else {
                ctx.print(
                    x - zoom * 1.0,
                    y - zoom * 2.0,
                    Line::styled(format!("[{}]", number + 1), color),
                );
            }
        }
    } else if let UIState::ShowAIMoves(_) = state.ui_state {
        if let Some(ai_result) = state.ai_state.actual_result() {
            let annot_to_str = |(i, piece_t): (usize, Option<PieceType>)| {
                let str = if let Some(piece_t) = piece_t {
                    format!("({}{})", piece_t.letter(), i + 1)
                } else {
                    format!("({})", i + 1)
                };
                Line::styled(str, color_palette(i))
            };

            for (&field, annots) in ai_result.annotations.iter().filter(|(_, a)| !a.is_empty()) {
                let pair_up = annots.len() > 2;
                let rows = if pair_up {
                    (annots.len() + 1) / 2
                } else {
                    annots.len()
                };
                let y_diff_per_row = 4.26;
                let y_start_offset = 0.5 * y_diff_per_row * ((rows - 1) as f64) - 2.0;
                for row_index in 0..rows {
                    let y_offset = zoom * (y_start_offset - row_index as f64 * y_diff_per_row);
                    let (x, y) = translate_index(field);
                    if pair_up && 2 * row_index + 1 < annots.len() {
                        ctx.print(
                            x - zoom * 5.97,
                            y + y_offset,
                            annot_to_str(annots[2 * row_index]),
                        );
                        ctx.print(
                            x + zoom * 2.1,
                            y + y_offset,
                            annot_to_str(annots[2 * row_index + 1]),
                        );
                    } else {
                        let a = annots[if pair_up { 2 * row_index } else { row_index }];
                        let x_shift = if a.1.is_some() { 2.0 } else { 1.0 };
                        ctx.print(x - zoom * x_shift, y + y_offset, annot_to_str(a));
                    }
                }
            }
        }
    }
}

fn available_pieces_player(state: AllState<'_>) -> Player {
    if matches!(
        state.ui_state,
        UIState::PlaysAnimation(true)
            | UIState::ShowOptions(_, true)
            | UIState::GameFinished(_, true)
            | UIState::ShowAIMoves(true)
    ) {
        state.game_state.player().switched()
    } else {
        state.game_state.player()
    }
}

fn row_column_index(state: AllState<'_>, stack_index: usize, xlen: f64) -> (usize, usize) {
    let zoom = state.settings.piece_zoom_level.multiplier();
    let stacks_per_row = f64::floor((xlen - 2.0 * 12.5) / (23.0 + 4.0 * zoom)) as usize + 1;
    (stack_index / stacks_per_row, stack_index % stacks_per_row)
}

pub fn draw_pieces(ctx: &mut Context<'_>, state: AllState<'_>, game_setup: &GameSetup, xlen: f64) {
    let zoom = state.settings.piece_zoom_level.multiplier();
    let player = available_pieces_player(state);
    let (pieces, _) = state.game_state.pieces_for_player(player);
    let interior_color = match player {
        Player::White => ColorScheme::DARK_WHITE,
        Player::Black => state.settings.dark_tile_color.color(),
    };

    let pieces_with_counts = if let UIState::PositionSelected(_) = state.ui_state {
        let mut annots = state
            .piece_annotations
            .iter()
            .map(|(&p_type, &index)| (p_type, index))
            .collect::<Vec<_>>();
        annots.sort_by_key(|&(_, index)| index);
        annots
            .iter()
            .map(|(p_type, _)| (*p_type, pieces[p_type]))
            .collect::<Vec<_>>()
    } else {
        pieces
            .iter()
            .map(|(&p_type, &count)| (p_type, count))
            .collect::<Vec<_>>()
    };

    let max_depth = 3;
    let initial_pieces = game_setup.pieces_for_player(player);
    let max_initial_count = initial_pieces
        .iter()
        .map(|(_, &count)| count)
        .max()
        .unwrap();
    let y_offset_per_row = -58.0 + (u32::saturating_sub(4, max_initial_count) as f64) * 9.0;
    let get_coords = |i: usize, depth: u32| {
        let (row, col) = row_column_index(state, i, xlen);
        let x = 12.5 + (col as f64) * (23.0 + 4.0 * zoom);
        let y = -10_f64 - 8.0 * zoom - f64::from(max_depth - depth) * 9.0;
        (x, y + (row as f64) * y_offset_per_row)
    };
    for depth in 0..=max_depth {
        // multiple iteration to draw "stacked" pieces
        let it = pieces_with_counts
            .iter()
            .copied()
            .filter(|&(_, count)| count > 0)
            .enumerate();

        // draw the interior
        for (i, (_, count)) in it.clone() {
            if count > max_depth - depth {
                let (x, y) = get_coords(i, depth);
                draw_interior(ctx, state.settings.filling_style, x, y, interior_color);
            }
        }
        ctx.layer();
        // draw the pieces themselves
        let secondary_color = state.settings.color_scheme.secondary();
        for (i, (piece_t, count)) in it {
            if count > max_depth - depth {
                let (x, y) = get_coords(i, depth);
                tui_graphics::draw_piece(ctx, piece_t, x, y, zoom);

                if depth == max_depth {
                    let y_pos: f64 = (row_column_index(state, i, xlen).0 as f64)
                        * (y_offset_per_row - 2.5 * zoom)
                        - 2.0;
                    let intial_count = initial_pieces[&piece_t];
                    if let UIState::PositionSelected(_) = state.ui_state {
                        let number = state.piece_annotations[&piece_t] + 1;
                        ctx.print(
                            x - zoom * 3.5 - 2.5,
                            y_pos,
                            Line::styled(format!("[{number}]"), secondary_color),
                        );
                        ctx.print(
                            x + zoom * 1.0 + 1.2,
                            y_pos,
                            Line::raw(format!("{count}/{intial_count}")),
                        );
                    } else {
                        let (x_shift, content) = if zoom < 1.5 {
                            (zoom * 4.0, format!("{count} / {intial_count}"))
                        } else {
                            (zoom + 1.0, format!("{count}/{intial_count}"))
                        };
                        ctx.print(x - x_shift, y_pos, Line::raw(content));
                    }
                }
            }
        }
        ctx.layer();
    }
}

pub fn draw_interior(ctx: &mut Context<'_>, style: FillingStyle, x: f64, y: f64, color: Color) {
    match style {
        FillingStyle::Full => tui_graphics::draw_hex_interior(ctx, x, y, color, false, 1.0),
        FillingStyle::Border => tui_graphics::draw_interior_hex_border(ctx, x, y, 1.5, 1.5, color),
        FillingStyle::Hybrid => tui_graphics::draw_hex_interior(ctx, x, y, color, true, 1.0),
    }
}

pub fn find_losing_queen(board: &HiveBoard, result: HiveResult) -> Option<(OpenIndex, Player)> {
    result.to_player().and_then(|player| {
        for field in board.iter_fields() {
            let queen_pos = field.content().bottom().and_then(|&piece| {
                (piece.p_type == PieceType::Queen && piece.player != player)
                    .then_some(field.index())
            });
            if let Some(pos) = queen_pos {
                return Some((pos, player));
            }
        }
        None
    })
}
