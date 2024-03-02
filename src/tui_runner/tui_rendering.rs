use super::{
    tui_animations::{AnimationContext, Layer},
    tui_settings::{
        render_settings, BordersStyle, GraphicsState, MenuSetting, ScreenSplitting, Settings,
        WhiteTilesStyle,
    },
    AnimationState, UIState,
};
use crate::{
    pieces::{PieceType, Player},
    state::{HiveBoard, HiveContent, HiveGameState, HiveResult},
    tui_graphics,
};
use ratatui::{
    layout::{Alignment, Constraint, Layout},
    prelude::{CrosstermBackend, Terminal},
    style::Color,
    text::{Line, Text},
    widgets::{
        canvas::{Canvas, Context},
        Block, Borders, Clear, Paragraph,
    },
};
use std::{
    collections::{BTreeMap, HashMap},
    fmt::format,
    io::{self, Stdout},
};
use tgp_board::{
    open_board::{OpenBoard, OpenIndex},
    structures::directions::{DirectionEnumerable, HexaDirection},
    Board,
};

#[derive(Clone, Copy)]
pub struct AllState<'a> {
    pub game_state: &'a HiveGameState,
    pub settings: Settings,
    pub board_annotations: &'a HashMap<OpenIndex, usize>,
    pub piece_annotations: &'a HashMap<PieceType, usize>,
    pub ui_state: UIState,
    pub animation_state: &'a AnimationState,
    pub menu_index: usize,
    pub graphics_state: GraphicsState,
}

pub const RED: Color = Color::from_u32(0x00E05959);

pub const ORANGE: Color = Color::from_u32(0x00D89040);

pub const DARK_WHITE: Color = Color::from_u32(0x00DADADA);

pub fn render(
    terminal: &mut Terminal<CrosstermBackend<Stdout>>,
    settings_list: &[Box<dyn MenuSetting>],
    state: AllState<'_>,
    // we don't actually mutate anything, this is just an API limitation
    settings: &mut Settings,
    initial_pieces: &BTreeMap<PieceType, u32>,
) -> io::Result<()> {
    terminal.draw(|frame| {
        let area = frame.size();
        let percentage_left = match state.settings.splitting {
            ScreenSplitting::FarLeft => 55,
            ScreenSplitting::Left => 60,
            ScreenSplitting::Normal => 65,
            ScreenSplitting::Right => 70,
            ScreenSplitting::FarRight => 75,
        };
        let diff = 15;
        let splitted_top_level = Layout::horizontal(vec![
            Constraint::Percentage(percentage_left - diff),
            Constraint::Percentage(diff),
            Constraint::Percentage(100 - percentage_left),
        ])
        .split(area);
        let splitted_in_game = Layout::horizontal(vec![
            Constraint::Percentage(percentage_left),
            Constraint::Percentage(100 - percentage_left),
        ])
        .split(area);

        let splitted_layout = if let UIState::Toplevel = state.ui_state {
            splitted_top_level
        } else {
            splitted_in_game.clone()
        };
        let canvas_area = splitted_layout[0];
        let &menu_area = splitted_layout.last().unwrap();
        {
            // the board
            let y_factor = 2.1;
            let zoom = state.graphics_state.zoom_level.multiplier();
            let mut center_x = state.graphics_state.center_x;
            let center_y = state.graphics_state.center_y;

            let x_len = zoom * (f64::from(canvas_area.width) - 2.5);
            let y_len = zoom * y_factor * (f64::from(canvas_area.height) - 2.5);

            // use same center with the top level layout
            if let UIState::Toplevel = state.ui_state {
                let alt_x_len = zoom * (f64::from(splitted_in_game[0].width) - 2.5);
                let diff = x_len - alt_x_len;
                center_x += diff;
            }
            let x_bounds = [center_x - x_len, center_x + x_len];
            let y_bounds = [center_y - y_len, center_y + y_len];
            let canvas = Canvas::default()
                .block(Block::default().borders(Borders::ALL))
                .x_bounds(x_bounds)
                .y_bounds(y_bounds)
                .paint(|ctx| draw_board(ctx, state, x_bounds, y_bounds));
            frame.render_widget(canvas, canvas_area);
        }

        if let UIState::Toplevel = state.ui_state {
            let action_area = splitted_layout[1];
            let text = "[c]ontinue game  [↲]\n\
                [n]ew game\n\
                \n\
                [q]uit";
            let paragraph =
                Paragraph::new(text).block(Block::default().title("Actions").borders(Borders::ALL));
            frame.render_widget(paragraph, action_area);

            let [menu_area, help_area] =
                *Layout::vertical([Constraint::Fill(1), Constraint::Max(7)]).split(menu_area)
            else {
                unreachable!()
            };
            let text = render_settings(settings, settings_list, state.menu_index);
            let paragraph = Paragraph::new(text)
                .block(Block::default().title("Settings").borders(Borders::ALL));
            frame.render_widget(paragraph, menu_area);

            let text = "This is a TUI version of the Hive board game.";
            let paragraph =
                Paragraph::new(text).block(Block::default().title("Help").borders(Borders::ALL));
            frame.render_widget(paragraph, help_area);
        } else {
            let [piece_area, tooltip_area] =
                *Layout::vertical([Constraint::Fill(1), Constraint::Max(8)]).split(menu_area)
            else {
                unreachable!()
            };
            // the pieces
            let zoom = state.settings.piece_zoom_level.multiplier();
            let x_len = zoom * f64::from(2 * piece_area.width);
            let y_len = zoom * 2.1 * f64::from(2 * piece_area.height);
            let canvas = Canvas::default()
                .block(
                    Block::default()
                        .title("Available Pieces")
                        .borders(Borders::ALL),
                )
                .x_bounds([0.0, x_len])
                .y_bounds([-y_len, 0.0])
                .paint(|ctx| draw_pieces(ctx, state, initial_pieces));
            frame.render_widget(canvas, piece_area);

            // TODO: [z] and [y]
            let text = "press a number to select a move\n   \
                (two digits: press [Space] or [↲] first)\n\
                [u]ndo or [r]edo a move\n\
                [↑↓←→] or [wasd] to move the screen\n\
                [+-] or [PageDown PageUp] for zooming\n\
                [Esc] or [q] to get back to the menu\n\
                [←] or [c] to cancel AI move/animation";
            let paragraph =
                Paragraph::new(text).block(Block::default().title("Help").borders(Borders::ALL));
            frame.render_widget(paragraph, tooltip_area);

            let message_hor = Layout::horizontal(vec![
                Constraint::Fill(1),
                Constraint::Max(42),
                Constraint::Fill(1),
            ])
            .split(canvas_area);
            let message_vert = Layout::vertical(vec![
                Constraint::Fill(1),
                Constraint::Max(6),
                Constraint::Fill(20),
            ])
            .split(message_hor[1]);
            let msg_area = message_vert[1];
            match state.ui_state {
                UIState::ShowOptions(true) => {
                    let player_name = match state.game_state.player() {
                        Player::White => "white",
                        Player::Black => "black",
                    };
                    let msg = Text::from(vec![
                        Line::raw(format!("The {} player has no available move", player_name))
                            .alignment(Alignment::Center),
                        Line::raw("and must therefore skip its turn.").alignment(Alignment::Center),
                        Line::raw(""),
                        Line::raw("continue with [1] or [↲]").alignment(Alignment::Center),
                    ]);
                    frame.render_widget(Clear, msg_area);
                    frame.render_widget(
                        Paragraph::new(msg).block(Block::default().borders(Borders::ALL)),
                        msg_area,
                    );
                }
                UIState::GameFinished(result) => {
                    let msg;
                    if let Some(player) = result.to_player() {
                        let player_name = match player {
                            Player::White => "white",
                            Player::Black => "black",
                        };
                        msg = Text::from(vec![
                            Line::raw(""),
                            Line::styled("Victory!", RED).alignment(Alignment::Center),
                            Line::raw(""),
                            Line::styled(
                                format!("The {} player has won!", player_name),
                                Color::White,
                            )
                            .alignment(Alignment::Center),
                        ]);
                    } else {
                        msg = Text::from(vec![
                            Line::raw(""),
                            Line::styled("Draw!", RED).alignment(Alignment::Center),
                            Line::raw(""),
                            Line::styled("None of the players could get on top!", Color::White)
                                .alignment(Alignment::Center),
                        ]);
                    }
                    frame.render_widget(Clear, msg_area);
                    frame.render_widget(
                        Paragraph::new(msg)
                            .block(Block::default().borders(Borders::ALL).style(RED)),
                        msg_area,
                    );
                }
                _ => (),
            }
        }
    })?;
    Ok(())
}

pub fn translate_index(OpenIndex { x, y }: OpenIndex) -> (f64, f64) {
    let x = f64::from(i32::try_from(x).unwrap());
    let y = f64::from(i32::try_from(y).unwrap());
    (x * 21.0, y * 24.0 - x * 12.0)
}

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
            .and_then(|content| get_piece(content))
            .inspect(|piece| {
                let is_white = piece.player == Player::White;
                if is_white || level > 0 {
                    let color = if is_white {
                        DARK_WHITE
                    } else {
                        Color::from_u32(0)
                    };
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
                tui_graphics::draw_restricted_hex_border(ctx, x_mid, y_mid, &to_draw);
            } else if state.settings.borders_style != BordersStyle::None {
                tui_graphics::draw_hex_border(ctx, x_mid, y_mid);
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
    match state.ui_state {
        UIState::PositionSelected(index) | UIState::PieceSelected(index) => {
            let (x_mid, y_mid) = translate_index(index);
            let stack_size = board[index].len();
            let level = if stack_size <= 1 { 0 } else { stack_size - 1 };
            tui_graphics::draw_interior_hex_border_lvl(ctx, x_mid, y_mid, 0.0, 0.0, RED, level);
        }
        UIState::GameFinished(result) => {
            if let Some((index, _)) = find_losing_queen(board, result) {
                let (x_mid, y_mid) = translate_index(index);
                tui_graphics::draw_interior_hex_border(ctx, x_mid, y_mid, 0.0, 0.0, RED);
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
        UIState::ShowOptions(false) | UIState::PieceSelected(_)
    ) {
        let color = if state.ui_state == UIState::ShowOptions(false) {
            RED
        } else {
            ORANGE
        };
        for (&board_index, &number) in state.board_annotations.iter() {
            let (x, y) = translate_index(board_index);
            if number >= 9 {
                ctx.print(
                    x - zoom * 4.0,
                    y - zoom * 2.0,
                    Line::styled(format!("[ {}]", number + 1), color),
                );
            } else {
                ctx.print(
                    x - zoom * 1.0,
                    y - zoom * 2.0,
                    Line::styled(format!("[{}]", number + 1), color),
                );
            }
        }
    }
}

pub fn draw_pieces(
    ctx: &mut Context<'_>,
    state: AllState<'_>,
    initial_pieces: &BTreeMap<PieceType, u32>,
) {
    let zoom = state.settings.piece_zoom_level.multiplier();
    let player = if state.ui_state == UIState::PlaysAnimation(false) {
        state.game_state.player().switched()
    } else {
        state.game_state.player()
    };
    let (pieces, _) = state.game_state.pieces_for_player(player);
    let interior_color = match player {
        Player::White => DARK_WHITE,
        Player::Black => Color::from_u32(0x00303030),
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

    let get_coords = |i: usize, depth: u32| {
        let x = 12_f64 + f64::from(u32::try_from(i).unwrap()) * (22.0 + 4.0 * zoom);
        let y = -10_f64 - 8.0 * zoom - f64::from(u32::try_from(3 - depth).unwrap()) * 10.0;
        (x, y)
    };
    for depth in 0..=3 {
        // multiple iteration to draw "stacked" pieces
        let it = pieces_with_counts
            .iter()
            .copied()
            .filter(|&(_, count)| count > 0)
            .enumerate();

        // draw the interior
        for (i, (_, count)) in it.clone() {
            if count > 3 - depth {
                let (x, y) = get_coords(i, depth);
                draw_interior(ctx, state.settings.white_tiles_style, x, y, interior_color);
            }
        }
        ctx.layer();
        // draw the pieces themselves
        for (i, (piece_t, count)) in it {
            if count > 3 - depth {
                let (x, y) = get_coords(i, depth);
                tui_graphics::draw_piece(ctx, piece_t, x, y, zoom);

                if depth == 3 {
                    let intial_count = initial_pieces[&piece_t];
                    if let UIState::PositionSelected(_) = state.ui_state {
                        let number = state.piece_annotations[&piece_t] + 1;
                        ctx.print(
                            x - zoom * 3.5 - 2.5,
                            -2.0,
                            Line::styled(format!("[{number}]"), ORANGE),
                        );
                        ctx.print(
                            x + zoom * 1.0 + 1.2,
                            -2.0,
                            Line::raw(format!("{count}/{intial_count}")),
                        );
                    } else {
                        let content;
                        let x_shift;
                        if zoom < 1.5 {
                            x_shift = zoom * 4.0;
                            content = format!("{count} / {intial_count}")
                        } else {
                            x_shift = zoom + 1.0;
                            content = format!("{count}/{intial_count}")
                        };
                        ctx.print(x - x_shift, -2.0, Line::styled(content, Color::White));
                    }
                }
            }
        }
        ctx.layer();
    }
}

pub fn draw_interior(ctx: &mut Context<'_>, style: WhiteTilesStyle, x: f64, y: f64, color: Color) {
    match style {
        WhiteTilesStyle::Full => tui_graphics::draw_hex_interior(ctx, x, y, color, false, 1.0),
        WhiteTilesStyle::Border => {
            tui_graphics::draw_interior_hex_border(ctx, x, y, 1.5, 1.5, color)
        }
        WhiteTilesStyle::Hybrid => tui_graphics::draw_hex_interior(ctx, x, y, color, true, 1.0),
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
