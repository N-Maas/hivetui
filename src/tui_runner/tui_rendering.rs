use super::tui_settings::WhiteTilesStyle;
use crate::pieces::Player;

use super::tui_animations::Layer;

use crate::tui_graphics;

use ratatui::text::Span;
use tgp_board::structures::directions::DirectionEnumerable;
use tgp_board::structures::directions::HexaDirection;
use tgp_board::Board;

use super::tui_settings::BordersStyle;

use ratatui::widgets::canvas::Context;

use ratatui::text::Text;

use ratatui::text::Line;

use ratatui::layout::Constraint;

use ratatui::widgets::Paragraph;

use ratatui::widgets::Borders;

use ratatui::widgets::Block;

use ratatui::widgets::canvas::Canvas;

use ratatui::layout::Layout;

use super::tui_settings::ScreenSplitting;

use std::io;

use std::collections::BTreeMap;

use super::tui_settings::MenuSetting;

use std::io::Stdout;

use ratatui::prelude::CrosstermBackend;

use ratatui::prelude::Terminal;

use ratatui::style::Color;

use super::tui_settings::GraphicsState;

use super::tui_animations::Animation;

use super::UIState;

use crate::pieces::PieceType;

use tgp_board::open_board::OpenIndex;

use std::collections::HashMap;

use crate::state::HiveGameState;

#[derive(Clone, Copy)]
pub struct AllState<'a> {
    pub game_state: &'a HiveGameState,
    pub board_annotations: &'a HashMap<OpenIndex, usize>,
    pub piece_annotations: &'a HashMap<PieceType, usize>,
    pub ui_state: UIState,
    pub animation: Option<&'a Animation>,
    pub menu_index: usize,
    pub graphics_state: GraphicsState,
}

pub const RED: Color = Color::from_u32(0x00E05959);

pub const ORANGE: Color = Color::from_u32(0x00D89040);

pub const DARK_WHITE: Color = Color::from_u32(0x00DADADA);

pub fn render(
    terminal: &mut Terminal<CrosstermBackend<Stdout>>,
    settings: &[Box<dyn MenuSetting>],
    state: AllState<'_>,
    // we don't actually mutate anything, this is just an API limitation
    graphics_state: &mut GraphicsState,
    initial_pieces: &BTreeMap<PieceType, u32>,
) -> io::Result<()> {
    terminal.draw(|frame| {
        let area = frame.size();
        let percentage_left = match state.graphics_state.splitting {
            ScreenSplitting::FarLeft => 55,
            ScreenSplitting::Left => 60,
            ScreenSplitting::Normal => 65,
            ScreenSplitting::Right => 70,
            ScreenSplitting::FarRight => 75,
        };
        let contraints = if matches!(state.ui_state, UIState::Toplevel) {
            let diff = 15;
            vec![
                Constraint::Percentage(percentage_left - diff),
                Constraint::Percentage(diff),
                Constraint::Percentage(100 - percentage_left),
            ]
        } else {
            vec![
                Constraint::Percentage(percentage_left),
                Constraint::Percentage(100 - percentage_left),
            ]
        };
        let splitted_layout = Layout::horizontal(contraints).split(area);
        let canvas_area = splitted_layout[0];
        let &menu_area = splitted_layout.last().unwrap();
        {
            // the board
            let zoom = state.graphics_state.zoom_level.multiplier();
            let center_x = state.graphics_state.center_x;
            let center_y = state.graphics_state.center_y;

            let x_len = zoom * f64::from(canvas_area.width);
            let y_len = zoom * 2.1 * f64::from(canvas_area.height);
            let canvas = Canvas::default()
                .block(Block::default().borders(Borders::ALL))
                .x_bounds([center_x - x_len, center_x + x_len])
                .y_bounds([center_y - y_len, center_y + y_len])
                .paint(|ctx| draw_board(ctx, state));
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
            let mut lines = Vec::<Line>::new();
            for (i, option) in settings.iter().enumerate() {
                let color = if state.menu_index == i {
                    RED
                } else {
                    Color::White
                };
                let mut spans = vec![Span::styled(format!("[{}] ", i + 1), color)];
                spans.extend(option.get_line(graphics_state, state.menu_index == i));
                lines.push(Line::from(spans));
            }
            let text = Text::from(lines);
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
            let zoom = state.graphics_state.piece_zoom_level.multiplier();
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

            let text = "press a number to select a move\n   \
                (two digits: press [Space] or [↲] first)\n\
                [u]ndo or [r]edo a move\n\
                [↑↓←→] or [wasd] to move the screen\n\
                [+-] or [PageDown PageUp] for zooming\n\
                [Esc] or [q] to get back to the menu";
            let paragraph =
                Paragraph::new(text).block(Block::default().title("Help").borders(Borders::ALL));
            frame.render_widget(paragraph, tooltip_area);
        }
    })?;
    Ok(())
}

pub fn translate_index(OpenIndex { x, y }: OpenIndex) -> (f64, f64) {
    let x = f64::from(i32::try_from(x).unwrap());
    let y = f64::from(i32::try_from(y).unwrap());
    (x * 21.0, y * 24.0 - x * 12.0)
}

pub fn draw_board(ctx: &mut Context<'_>, state: AllState<'_>) {
    let zoom = state.graphics_state.zoom_level.multiplier();
    let temporary_state = state
        .animation
        .and_then(|a| a.get_temporary_state(state.game_state));
    let board = temporary_state
        .as_ref()
        .map_or(state.game_state.board(), |s| s.board());

    // first round: draw borders
    for field in board.iter_fields() {
        let (x_mid, y_mid) = translate_index(field.index());
        if let Some(content) = field.content_checked() {
            if content.is_empty()
                && state.graphics_state.borders_style == BordersStyle::Partial
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
            } else if state.graphics_state.borders_style != BordersStyle::None {
                tui_graphics::draw_hex_border(ctx, x_mid, y_mid);
            }
            // which sides should be drawn?
        }
    }
    state
        .animation
        .inspect(|a| a.draw(ctx, &state.graphics_state, Layer::Borders));
    ctx.layer();

    // second round: draw interiors
    for field in board.iter_fields() {
        let (x_mid, y_mid) = translate_index(field.index());
        field
            .content_checked()
            .and_then(|content| content.top())
            .inspect(|piece| {
                if piece.player == Player::White {
                    draw_interior(ctx, &state.graphics_state, x_mid, y_mid, DARK_WHITE);
                }
            });
    }
    state
        .animation
        .inspect(|a| a.draw(ctx, &state.graphics_state, Layer::Interiors));
    ctx.layer();

    // third round: draw pieces
    for field in board.iter_fields() {
        let (x_mid, y_mid) = translate_index(field.index());
        field
            .content_checked()
            .and_then(|content| content.top())
            .inspect(|piece| tui_graphics::draw_piece(ctx, piece.p_type, x_mid, y_mid, zoom));
    }
    state
        .animation
        .inspect(|a| a.draw(ctx, &state.graphics_state, Layer::Pieces));
    ctx.layer();

    // is a specific field selected?
    match state.ui_state {
        UIState::PositionSelected(index) | UIState::PieceSelected(index) => {
            let (x_mid, y_mid) = translate_index(index);
            tui_graphics::draw_interior_hex_border(ctx, x_mid, y_mid, 0.0, 0.0, RED);
        }
        _ => (),
    }
    state
        .animation
        .inspect(|a| a.draw(ctx, &state.graphics_state, Layer::Selection));
    ctx.layer();

    state
        .animation
        .inspect(|a| a.draw(ctx, &state.graphics_state, Layer::Final));

    // print indizes
    if matches!(
        state.ui_state,
        UIState::ShowOptions | UIState::PieceSelected(_)
    ) {
        let color = if state.ui_state == UIState::ShowOptions {
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
    let zoom = state.graphics_state.piece_zoom_level.multiplier();
    let player = if state.ui_state == UIState::PlaysAnimation {
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
                draw_interior(ctx, &state.graphics_state, x, y, interior_color);
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

pub fn draw_interior(
    ctx: &mut Context<'_>,
    graphics_state: &GraphicsState,
    x: f64,
    y: f64,
    color: Color,
) {
    match graphics_state.white_tiles_style {
        WhiteTilesStyle::Full => tui_graphics::draw_hex_interior(ctx, x, y, color, false),
        WhiteTilesStyle::Border => {
            tui_graphics::draw_interior_hex_border(ctx, x, y, 1.5, 1.5, color)
        }
        WhiteTilesStyle::Hybrid => tui_graphics::draw_hex_interior(ctx, x, y, color, true),
    }
}
