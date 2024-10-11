use super::{
    animation_and_ai_state::{AIState, AnimationState},
    game_setup::GameSetup,
    setting_renderer::{SettingRenderer, SettingSelection},
    text_input::TextInput,
    tui_settings::{ColorScheme, GraphicsState, ScreenSplitting, Settings},
    AIResult, Message, UIState,
};
use crate::{
    io_manager::IOManager,
    pieces::{PieceType, Player},
    state::{HiveContext, HiveGameState, HiveResult},
    tui_graphics::{self},
    tui_runner::{tui_settings::FilterAISuggestions, MessageType},
};
use board::{draw_board, draw_pieces};
use chrono::offset::Local;
use chrono::DateTime;
use ratatui::{
    layout::{Alignment, Constraint, Layout, Rect},
    prelude::{CrosstermBackend, Terminal},
    style::{Color, Stylize},
    text::{Line, Span, Text},
    widgets::{canvas::Canvas, Block, Borders, Clear, Padding, Paragraph, Wrap},
    Frame,
};
use std::{
    collections::{HashMap, HashSet},
    ffi::OsString,
    io::{self, Stdout},
    time::SystemTime,
};
use tgp_ai::RatingType;
use tgp_board::open_board::OpenIndex;
use tutorial::{bee_offset, build_bees_at_offset, build_rules_summary, build_tutorial};

#[derive(Clone, Copy)]
pub struct AllState<'a> {
    pub game_state: &'a HiveGameState,
    pub settings: Settings,
    pub board_annotations: &'a HashMap<OpenIndex, usize>,
    pub piece_annotations: &'a HashMap<PieceType, usize>,
    pub ui_state: UIState,
    pub animation_state: &'a AnimationState,
    pub ai_state: &'a AIState,
    pub menu_selection: SettingSelection,
    pub text_input: &'a TextInput,
    pub graphics_state: GraphicsState,
}

pub const TUTORIAL_HEIGHT: u16 = 42;

const MENU_HELP: &str = "\
    press [j] to show the rules and the tutorial\n\
    \n\
    [↑↓] navigate settings\n\
    [←→] change current setting\n\
    [1-9] directly jump to according setting\n\
    [⇆] switch between general/graphic settings
";

const IN_GAME_HELP_LONG: &str = "\
    press a number to select a move\n   \
    (two digits: press [Space] first)\n\
    \n\
    [↑↓←→][wasd] move the camera\n\
    [PgDown PgUp][+-] zooming\n\
    [⇆] switch displayed pieces\n\
    \n\
    [h] show AI suggested moves\n\
    [u] undo a move\n\
    [r] redo a move\n\
    [c] cancel AI move\n\
    [n] start AI move (if not automatic)\n\
    \n\
    [l] load game\n\
    [k] save game\n\
    [j] show rules and tutorial\n\
    [Esc] back to menu\
";

const IN_GAME_HELP_SHORT: &str = "\
    press a number to select a move\n   \
    (two digits: press [Space] first)\n\
    \n\
    [↑↓←→][wasd] move the camera\n\
    [PgDown PgUp][+-] zooming\n\
    [⇆] switch displayed pieces\n\
    [h] show AI suggested moves\n\
    [u] undo move      [r] redo move\n\
    [c] cancel AI move\n\
    [n] start AI move (if not automatic)\n\
    [l] load game      [k] save game\n\
    [Esc] back to menu\
";

pub mod board;
mod tutorial;

pub fn translate_index(OpenIndex { x, y }: OpenIndex) -> (f64, f64) {
    let x = f64::from(i32::try_from(x).unwrap());
    let y = f64::from(i32::try_from(y).unwrap());
    (x * 21.0, y * 24.0 - x * 12.0)
}

pub fn render(
    terminal: &mut Terminal<CrosstermBackend<Stdout>>,
    setting_renderer: &SettingRenderer,
    state: AllState<'_>,
    messages: &[Message],
    // we don't actually mutate anything, this is just an API limitation
    settings: &mut Settings,
    game_setup: &GameSetup,
    io_manager: &Option<IOManager>,
) -> io::Result<([f64; 2], [f64; 2])> {
    let mut output_bound = ([0.0, 0.0], [0.0, 0.0]);
    terminal.draw(|frame| {
        let area = frame.size();

        // first: decide whether to split vertically
        let action_desired_width = 35;
        let ingame_help_min_width = 45;
        let ingame_help_height = Text::raw(IN_GAME_HELP_LONG).height() as u16 + 2;
        let ingame_help_small_height = Text::raw(IN_GAME_HELP_SHORT).height() as u16 + 2;
        let vertical_split = 7 * area.height.saturating_sub(ingame_help_small_height)
            >= 3 * area.width.saturating_sub(ingame_help_min_width);
        let mut small_in_game_help = false;

        // now compute the three main areas
        let (canvas_area, action_area, menu_area) = if !state.ui_state.top_level() && vertical_split
        {
            let mut final_help_height =
                if area.height.saturating_sub(ingame_help_height + 1) > area.height / 2 {
                    ingame_help_height
                } else {
                    small_in_game_help = true;
                    ingame_help_small_height
                };
            {
                // try to further increase the height to better display available pieces
                let max_bonus = 6;
                let cutoff = 12;
                final_help_height +=
                    max_bonus * (area.height / 2).saturating_sub(final_help_height) / cutoff;
            }
            let constraints = [Constraint::Fill(1), Constraint::Max(final_help_height)];
            let [canvas_area, menu_area] = Layout::vertical(constraints).areas(area);
            let [_, action_area] =
                Layout::horizontal([Constraint::Fill(1), Constraint::Max(action_desired_width)])
                    .areas(canvas_area);
            (canvas_area, action_area, menu_area)
        } else {
            let menu_desired_width = 55;
            let menu_min_width = 45;
            let action_min_width = 24;
            let menu_width = u16::min(
                menu_desired_width,
                u16::max(menu_min_width, area.width.saturating_sub(action_min_width)),
            );
            let menu_contraint = match state.settings.splitting {
                ScreenSplitting::Auto => {
                    let cutoff_low = 125;
                    let cutoff_high = 250;
                    let max_bonus = 10;
                    let added = max_bonus
                        * u16::min(area.width, cutoff_high).saturating_sub(cutoff_low)
                        / (cutoff_high - cutoff_low);
                    Constraint::Max(menu_width + added)
                }
                // TODO: remove this?
                ScreenSplitting::FarLeft => Constraint::Percentage(50),
                ScreenSplitting::Left => Constraint::Percentage(42),
                ScreenSplitting::Normal => Constraint::Percentage(36),
                ScreenSplitting::Right => Constraint::Percentage(30),
                ScreenSplitting::FarRight => Constraint::Percentage(25),
            };
            let [_, action_area, menu_area] = Layout::horizontal(vec![
                Constraint::Fill(1),
                Constraint::Max(
                    if menu_width + action_desired_width <= area.width
                        || state.settings.splitting != ScreenSplitting::Auto
                    {
                        action_desired_width
                    } else {
                        // ensure well-behavedness for very small screen
                        area.width.saturating_sub(menu_width)
                    },
                ),
                menu_contraint,
            ])
            .areas(area);
            let [canvas_area, _] =
                Layout::horizontal(vec![Constraint::Fill(1), menu_contraint]).areas(area);
            (canvas_area, action_area, menu_area)
        };

        // the board
        {
            let y_factor = 2.1;
            let zoom = state.graphics_state.zoom_level.multiplier();
            let center_x = state.graphics_state.center_x;
            let center_y = state.graphics_state.center_y;

            let x_len = zoom * (f64::from(canvas_area.width) - 2.5);
            let y_len = zoom * y_factor * (f64::from(canvas_area.height) - 2.5);
            let x_bounds = [center_x - x_len, center_x + x_len];
            let y_bounds = [center_y - y_len, center_y + y_len];
            let canvas = Canvas::default()
                .block(Block::default().borders(Borders::ALL))
                .x_bounds(x_bounds)
                .y_bounds(y_bounds)
                .paint(|ctx| draw_board(ctx, state, x_bounds, y_bounds));
            frame.render_widget(canvas, canvas_area);

            // the small message indicating whose turn it is
            let is_animation = matches!(state.ui_state, UIState::PlaysAnimation(_))
                && !state.ai_state.animation_has_started();
            if !state.ui_state.top_level() && state.game_state.result().is_none() && !is_animation {
                render_game_state(frame, state.game_state, canvas_area);
            }
            output_bound = (x_bounds, y_bounds);
        }

        if let UIState::RulesSummary(scroll, _) = state.ui_state {
            // the rules summary and the tutorial
            let [_, rules_area, tutorial_area] = Layout::horizontal(vec![
                Constraint::Fill(1),
                Constraint::Max(84),
                Constraint::Max(79),
            ])
            .areas(area);

            frame.render_widget(Clear, rules_area);
            render_rules_summary(frame, settings, rules_area, scroll);
            frame.render_widget(Clear, tutorial_area);
            render_inline_tutorial(frame, tutorial_area, scroll);
        } else if state.ui_state.top_level() {
            let render_setup_area = matches!(
                state.ui_state,
                UIState::GameSetup(_, _) | UIState::LoadScreen(_, _) | UIState::SaveScreen(_)
            );

            // the menu (actions)
            if !matches!(
                state.ui_state,
                UIState::GameSetup(_, _) | UIState::SaveScreen(_)
            ) {
                let y_offset = render_action_area(frame, action_area);
                render_messages(frame, messages, action_area, y_offset + 1);
            }

            let help_size = Text::raw(MENU_HELP).height() as u16 + 2;
            let (player_size, mut settings_size) = (7, 10);
            if render_setup_area {
                settings_size = 15;
            }
            let both_settings = menu_area.height >= 27 + help_size && !render_setup_area;
            if both_settings {
                settings_size = 22;
            }
            let [player_area, settings_area, help_area] = Layout::vertical([
                Constraint::Max(player_size),
                Constraint::Min(settings_size),
                Constraint::Max(help_size),
            ])
            .areas(menu_area);
            let remaining_size = menu_area
                .height
                .saturating_sub(player_size)
                .saturating_sub(help_size);

            // the players
            {
                let selection = if let UIState::GameSetup(index, is_char) = state.ui_state {
                    Some((index, is_char))
                } else if let UIState::LoadScreen(index, is_char) = state.ui_state {
                    Some((index, is_char))
                } else {
                    state.menu_selection.player_index()
                };
                let text = setting_renderer.render_player_settings(settings, selection);
                let paragraph = Paragraph::new(text).block(
                    Block::default()
                        .title("Players")
                        .borders(Borders::BOTTOM.complement()),
                );
                frame.render_widget(paragraph, player_area);
            }

            // the settings (or game setup/load/save)
            {
                if let UIState::GameSetup(index, _) = state.ui_state {
                    let par = game_setup.render_game_setup(settings, index, 2);
                    frame.render_widget(par, settings_area);
                } else if let UIState::LoadScreen(index, _) = state.ui_state {
                    let save_games = io_manager.as_ref().unwrap().save_files_list();
                    let par = load_game_widget(settings, remaining_size, save_games, index, 2);
                    frame.render_widget(par, settings_area);
                } else if let UIState::SaveScreen(_) = state.ui_state {
                    let save_games = io_manager.as_ref().unwrap().save_files_list();
                    render_save_game_widget(
                        frame,
                        settings_area,
                        settings,
                        remaining_size,
                        state.text_input,
                        save_games,
                    );
                } else if both_settings {
                    let [general, graphic] =
                        Layout::vertical([Constraint::Fill(1), Constraint::Fill(1)])
                            .areas(settings_area);
                    let par =
                        setting_renderer.render_general_settings(settings, state.menu_selection);
                    frame.render_widget(par, general);
                    let par =
                        setting_renderer.render_graphic_settings(settings, state.menu_selection);
                    frame.render_widget(par, graphic);
                } else {
                    let par =
                        setting_renderer.render_current_settings(settings, state.menu_selection);
                    frame.render_widget(par, settings_area);
                }
            }

            // the top level help text
            {
                let paragraph = Paragraph::new(MENU_HELP)
                    .block(Block::default().borders(Borders::ALL))
                    .wrap(Wrap { trim: true })
                    .style(ColorScheme::TEXT_GRAY);
                frame.render_widget(paragraph, help_area);
            }

            // the tutorial at the start of the game
            if let UIState::Tutorial(scroll) = state.ui_state {
                let [_, tutorial_area, _] = Layout::horizontal([
                    Constraint::Fill(1),
                    Constraint::Max(92),
                    Constraint::Fill(1),
                ])
                .areas(area);
                let [_, tutorial_area, _] = Layout::vertical([
                    Constraint::Fill(3),
                    Constraint::Max(TUTORIAL_HEIGHT),
                    Constraint::Fill(4),
                ])
                .areas(tutorial_area);

                let mut extended_area = tutorial_area;
                let trim = u16::min(extended_area.y, 1);
                extended_area.y -= trim;
                extended_area.height += trim + 1;
                extended_area.height = u16::min(extended_area.height, area.height);
                let trim = u16::min(extended_area.x, 2);
                extended_area.x -= trim;
                extended_area.width += trim + 2;
                extended_area.width = u16::min(extended_area.width, area.width);
                frame.render_widget(
                    Block::default()
                        .borders(Borders::NONE)
                        .style(ColorScheme::TEXT_GRAY),
                    area,
                );
                frame.render_widget(Clear, extended_area);
                frame.render_widget(Block::default().borders(Borders::ALL), extended_area);
                frame.render_widget(Clear, tutorial_area);
                render_startup_tutorial(frame, tutorial_area, scroll);
            }
        } else {
            // in-game rendering
            let mut render_both = false;
            let (piece_area, suggestion_area, help_area) = if vertical_split {
                // TODO: make this more refined?
                let [piece_area, help_area] = Layout::horizontal([
                    Constraint::Fill(1),
                    Constraint::Max(ingame_help_min_width),
                ])
                .areas(menu_area);
                (piece_area, piece_area, help_area)
            } else {
                let piece_height = 10;
                let double_piece_height = 20;
                small_in_game_help = menu_area.height < double_piece_height + ingame_help_height;

                let constraints = if small_in_game_help {
                    [
                        Constraint::Min(piece_height - 3),
                        Constraint::Max(ingame_help_small_height),
                    ]
                } else {
                    [
                        Constraint::Min(piece_height),
                        Constraint::Max(ingame_help_height),
                    ]
                };
                let [piece_area, help_area] = Layout::vertical(constraints).areas(menu_area);
                let suggestion_height = 12;
                render_both = matches!(state.ui_state, UIState::ShowAIMoves(_))
                    && piece_area.height >= suggestion_height + piece_height;
                let [piece_area, suggestion_area] = if render_both {
                    Layout::vertical([Constraint::Fill(1), Constraint::Max(suggestion_height)])
                        .areas(piece_area)
                } else {
                    [piece_area, piece_area]
                };
                (piece_area, suggestion_area, help_area)
            };

            // the AI suggested moves
            if let UIState::ShowAIMoves(_) = state.ui_state {
                let text;
                if let Some(ai_result) = state.ai_state.actual_result() {
                    text = ai_suggestions(ai_result, state.game_state);
                } else {
                    text = Text::raw("Waiting for AI calculation...");
                }
                let paragraph = Paragraph::new(text).block(
                    Block::default()
                        .title("Suggested Moves")
                        .borders(Borders::ALL),
                );
                frame.render_widget(paragraph, suggestion_area);
            }
            // the available pieces
            if render_both || !matches!(state.ui_state, UIState::ShowAIMoves(_)) {
                let zoom = state.settings.piece_zoom_level.multiplier();
                let x_len = zoom * (f64::from(2 * piece_area.width) - 4.5);
                let y_len = zoom * 2.1 * (f64::from(2 * piece_area.height) - 4.48);
                let canvas = Canvas::default()
                    .block(
                        Block::default()
                            .title("Available Pieces")
                            .borders(Borders::ALL),
                    )
                    .x_bounds([0.0, x_len])
                    .y_bounds([-y_len, 0.0])
                    .paint(|ctx| draw_pieces(ctx, state, game_setup, x_len));
                frame.render_widget(canvas, piece_area);
            }

            // the in game help text
            {
                let text = if small_in_game_help {
                    IN_GAME_HELP_SHORT
                } else {
                    IN_GAME_HELP_LONG
                };
                let paragraph = Paragraph::new(text)
                    .block(Block::default().borders(Borders::ALL))
                    .style(ColorScheme::TEXT_GRAY);
                frame.render_widget(paragraph, help_area);
            }

            // the messages
            render_messages(frame, messages, action_area, 0);

            // the in-game message that might be shown at the top of the board
            {
                let [_, message_hor, _] = Layout::horizontal(vec![
                    Constraint::Fill(1),
                    Constraint::Max(42),
                    Constraint::Fill(1),
                ])
                .areas(canvas_area);
                let [_, msg_area, _] = Layout::vertical(vec![
                    Constraint::Fill(1),
                    Constraint::Max(6),
                    Constraint::Fill(20),
                ])
                .areas(message_hor);
                match state.ui_state {
                    UIState::ShowOptions(true, _) => {
                        frame.render_widget(Clear, msg_area);
                        frame.render_widget(skip_turn_message(state.game_state), msg_area);
                    }
                    UIState::GameFinished(result, _) => {
                        frame.render_widget(Clear, msg_area);
                        frame.render_widget(game_finished_message(settings, result), msg_area);
                    }
                    _ => (),
                }
            }
        }
    })?;
    Ok(output_bound)
}

pub fn color_palette(index: usize) -> Color {
    match index {
        0 => ColorScheme::RED,
        1 => ColorScheme::BLUE,
        2 => ColorScheme::GREEN,
        3 => ColorScheme::PURPLE,
        4 => ColorScheme::ORANGE,
        5 => ColorScheme::TURQUOISE,
        6 => ColorScheme::YELLOW_GREEN,
        7 => ColorScheme::PINK,
        _ => panic!("invalid color index"),
    }
}

fn render_game_state(frame: &mut Frame, game_state: &HiveGameState, area: Rect) {
    let player_name = match game_state.player() {
        Player::White => "white",
        Player::Black => "black",
    };
    let game_state_area = Rect {
        x: 1,
        y: 1,
        width: u16::min(11, area.width.saturating_sub(1)),
        height: 1,
    };
    frame.render_widget(Clear, game_state_area);
    frame.render_widget(Text::from(format!(" {player_name} turn")), game_state_area);
}

fn skip_turn_message(game_state: &HiveGameState) -> Paragraph<'static> {
    let player_name = match game_state.player() {
        Player::White => "white",
        Player::Black => "black",
    };
    let msg = Text::from(vec![
        Line::raw(format!("The {} player has no available move", player_name))
            .alignment(Alignment::Center),
        Line::raw("and must therefore skip its turn.").alignment(Alignment::Center),
        Line::raw(""),
        Line::styled("continue with [1] or [↲]", ColorScheme::TEXT_GRAY)
            .alignment(Alignment::Center),
    ]);
    Paragraph::new(msg).block(Block::default().borders(Borders::ALL))
}

fn game_finished_message(settings: &Settings, result: HiveResult) -> Paragraph<'static> {
    let primary_color = settings.color_scheme.primary();
    let msg;
    if let Some(player) = result.to_player() {
        let player_name = match player {
            Player::White => "white",
            Player::Black => "black",
        };
        msg = Text::from(vec![
            Line::raw(""),
            Line::styled("Victory!", primary_color)
                .bold()
                .alignment(Alignment::Center),
            Line::raw(""),
            Line::styled(format!("The {} player has won!", player_name), Color::White)
                .alignment(Alignment::Center),
        ]);
    } else {
        msg = Text::from(vec![
            Line::raw(""),
            Line::styled("Draw!", primary_color)
                .bold()
                .alignment(Alignment::Center),
            Line::raw(""),
            Line::styled("None of the players could get on top!", Color::White)
                .alignment(Alignment::Center),
        ]);
    }
    Paragraph::new(msg).block(Block::default().borders(Borders::ALL).style(primary_color))
}

fn render_rules_summary(frame: &mut Frame, settings: &Settings, area: Rect, scroll: u16) {
    let [top_area, body_area] =
        Layout::vertical(vec![Constraint::Max(3), Constraint::Fill(1)]).areas(area);
    let top_line = Line::styled(" [↑↓][Space] scroll", ColorScheme::TEXT_GRAY);
    let paragraph =
        Paragraph::new(top_line).block(Block::default().borders(Borders::BOTTOM.complement()));
    frame.render_widget(paragraph, top_area);

    let textwidth = area.width.saturating_sub(4);
    let textheight = area.height.saturating_sub(4);
    let threshold = bee_offset(textwidth, textheight);
    let (text, scroll) = if scroll < threshold {
        (build_rules_summary(settings, textwidth, textheight), scroll)
    } else {
        (
            build_bees_at_offset(textwidth, textheight, scroll - threshold),
            0,
        )
    };
    let paragraph = Paragraph::new(text)
        .block(
            Block::default()
                .padding(Padding::horizontal(1))
                .borders(Borders::TOP.complement()),
        )
        .wrap(Wrap { trim: true })
        .scroll((scroll, 0));
    frame.render_widget(paragraph, body_area);
}

fn render_inline_tutorial(frame: &mut Frame, area: Rect, scroll: u16) {
    let [top_area, body_area] =
        Layout::vertical(vec![Constraint::Max(3), Constraint::Fill(1)]).areas(area);
    let top_line =
        Line::styled("[Esc] return ", ColorScheme::TEXT_GRAY).alignment(Alignment::Right);
    let paragraph =
        Paragraph::new(top_line).block(Block::default().borders(Borders::BOTTOM.complement()));
    frame.render_widget(paragraph, top_area);

    let mut lines = Vec::new();
    build_tutorial(&mut lines, area.width.saturating_sub(4));
    let paragraph = Paragraph::new(lines)
        .block(
            Block::default()
                .padding(Padding::horizontal(1))
                .borders(Borders::TOP.complement()),
        )
        .scroll((scroll, 0));
    frame.render_widget(paragraph, body_area);
}

fn render_startup_tutorial(frame: &mut Frame, area: Rect, scroll: u16) {
    let [top_area, body_area, bottom_area] = Layout::vertical(vec![
        Constraint::Max(3),
        Constraint::Fill(1),
        Constraint::Max(3),
    ])
    .areas(area);
    let top_line = Line::styled(" [↑↓][Space] scroll", ColorScheme::TEXT_GRAY);
    let paragraph =
        Paragraph::new(top_line).block(Block::default().borders(Borders::BOTTOM.complement()));
    frame.render_widget(paragraph, top_area);

    let mut lines = Vec::new();
    build_tutorial(&mut lines, area.width - 4);
    let paragraph = Paragraph::new(lines)
        .block(
            Block::default()
                .padding(Padding::horizontal(1))
                .borders(Borders::TOP.complement().difference(Borders::BOTTOM)),
        )
        .scroll((scroll, 0));
    frame.render_widget(paragraph, body_area);

    let bottom_left = " [q] leave";
    let bottom_right = "[x] don't show again   [↲] continue ";
    let fill_up = area.width.saturating_sub(
        2 + bottom_left.chars().count() as u16 + bottom_right.chars().count() as u16,
    );
    let bottom_line = Line::styled(
        bottom_left.to_string() + &" ".repeat(usize::from(fill_up)) + bottom_right,
        ColorScheme::TEXT_GRAY,
    );
    let paragraph = Paragraph::new(vec![Line::raw(""), bottom_line])
        .block(Block::default().borders(Borders::TOP.complement()));
    frame.render_widget(paragraph, bottom_area);
}

/// returns the y offset
fn render_action_area(frame: &mut Frame, mut area: Rect) -> u16 {
    let lines = vec![
        Line::from(vec![
            Span::styled("[c]", ColorScheme::TEXT_GRAY),
            Span::raw(" continue game"),
            Span::styled("  [↲]", ColorScheme::TEXT_GRAY),
        ]),
        Line::from(vec![
            Span::styled("[n]", ColorScheme::TEXT_GRAY),
            Span::raw(" new game"),
        ]),
        Line::from(vec![
            Span::styled("[l]", ColorScheme::TEXT_GRAY),
            Span::raw(" load game"),
        ]),
        Line::from(vec![
            Span::styled("[k]", ColorScheme::TEXT_GRAY),
            Span::raw(" save game"),
        ]),
        Line::from(vec![
            Span::styled("[j]", ColorScheme::TEXT_GRAY),
            Span::raw(" rules and tutorial"),
        ]),
        Line::from(vec![
            Span::styled("[r]", ColorScheme::TEXT_GRAY),
            Span::raw(" restore default settings"),
        ]),
        Line::raw(""),
        Line::from(vec![
            Span::styled("[q]", ColorScheme::TEXT_GRAY),
            Span::raw(" quit"),
        ]),
    ];
    let text = Text::from(lines);
    area.height = u16::min(area.height, text.height() as u16 + 2);
    let paragraph = Paragraph::new(text).block(Block::default().borders(Borders::ALL));
    frame.render_widget(Clear, area);
    frame.render_widget(paragraph, area);
    area.height
}

fn render_messages(frame: &mut Frame, messages: &[Message], area: Rect, mut offset: u16) {
    if area.width <= 10 {
        return;
    }
    for msg in messages {
        let mut msg_area = area;
        let msg_len = msg.content.len() as u16;
        let line_estimate = if msg_len <= msg_area.width.saturating_sub(2) {
            1
        } else {
            msg_len / (msg_area.width.saturating_sub(5)) + 1
        };
        msg_area.y = offset;
        msg_area.height = line_estimate + 4;
        offset += msg_area.height;
        if offset > area.height {
            break;
        }
        let header = match msg.msg_type {
            MessageType::Success => "Success",
            MessageType::Info => "Info",
            MessageType::Warning => "Warning",
            MessageType::Error => "Error",
        };
        let color = match msg.msg_type {
            MessageType::Success => ColorScheme::GREEN,
            MessageType::Info => Color::White,
            MessageType::Warning => ColorScheme::TEXT_YELLOW,
            MessageType::Error => ColorScheme::RED,
        };
        let lines = vec![
            Line::styled(header.to_string(), color)
                .bold()
                .alignment(Alignment::Center),
            Line::raw(msg.content.clone()).alignment(if line_estimate == 1 {
                Alignment::Center
            } else {
                Alignment::Left
            }),
            Line::styled("[Esc]", ColorScheme::TEXT_GRAY).alignment(Alignment::Right),
        ];
        let paragraph = Paragraph::new(lines)
            .block(Block::default().borders(Borders::ALL).border_style(color))
            .wrap(Wrap { trim: true });
        frame.render_widget(Clear, msg_area);
        frame.render_widget(paragraph, msg_area);
    }
}

pub fn load_game_widget(
    settings: &Settings,
    available_size: u16,
    save_games: &[(OsString, SystemTime)],
    selection: usize,
    offset: usize,
) -> Paragraph<'static> {
    let mut lines = vec![Line::raw("Select game to load:"), Line::raw("")];
    let n_displayable = usize::from(available_size.saturating_sub(8) / 2);
    let n_skip = (selection + 1).saturating_sub(offset + n_displayable);
    create_save_game_list(
        &mut lines,
        settings,
        save_games,
        selection,
        offset,
        n_displayable,
        n_skip,
    );

    lines.push(Line::raw(""));
    lines.push(Line::styled(
        "[↲] load selected game",
        ColorScheme::TEXT_GRAY,
    ));
    lines.push(Line::styled("[Esc] return", ColorScheme::TEXT_GRAY));
    let text = Text::from(lines);
    Paragraph::new(text).block(Block::default().title("Load Game").borders(Borders::ALL))
}

pub fn render_save_game_widget(
    frame: &mut Frame,
    area: Rect,
    settings: &Settings,
    available_size: u16,
    text_input: &TextInput,
    save_games: &[(OsString, SystemTime)],
) {
    let prefix = "Enter name:";
    let mut lines = vec![
        Line::raw(""),
        Line::raw(prefix),
        Line::raw(""),
        Line::raw(""),
    ];
    let n_displayable = usize::from((available_size - 10) / 2);
    create_save_game_list(&mut lines, settings, save_games, 0, 1, n_displayable, 0);

    lines.push(Line::raw(""));
    lines.push(Line::styled("[↲] save the game", ColorScheme::TEXT_GRAY));
    lines.push(Line::styled("[Esc] return", ColorScheme::TEXT_GRAY));
    let text = Text::from(lines);
    let par = Paragraph::new(text).block(Block::default().title("Save Game").borders(Borders::ALL));
    frame.render_widget(par, area);
    let plen = prefix.len() as u16;
    let input_area = Rect::new(
        area.x + plen + 3,
        area.y + u16::min(1, area.height),
        area.width.saturating_sub(plen + 11),
        u16::min(3, area.height),
    );
    frame.render_widget(Clear, input_area);
    text_input.render(frame, input_area, settings.color_scheme.primary());
}

pub fn create_save_game_list(
    lines: &mut Vec<Line<'static>>,
    settings: &Settings,
    save_games: &[(OsString, SystemTime)],
    selection: usize,
    offset: usize,
    n_displayable: usize,
    n_skip: usize,
) {
    let iter = save_games
        .iter()
        .enumerate()
        .skip(n_skip)
        .take(n_displayable);

    for (i, (name, time)) in iter {
        let selected = selection == offset + i;
        let name = name.to_string_lossy();
        let datetime = DateTime::<Local>::from(*time);
        if selected {
            lines.push(Line::styled(
                format!("<{name}>"),
                settings.color_scheme.primary(),
            ));
        } else {
            lines.push(Line::raw(format!(" {name} ")));
        }
        lines.push(
            Line::styled(
                datetime.format("%Y-%m-%d %H:%M        ").to_string(),
                ColorScheme::TEXT_GRAY,
            )
            .alignment(Alignment::Right),
        );
    }
    if save_games.is_empty() {
        lines.push(Line::styled(
            " (no saved game found)",
            ColorScheme::TEXT_GRAY,
        ));
    }
    if n_displayable + n_skip < save_games.len() {
        lines.push(Line::raw(" ..."));
    } else {
        lines.push(Line::raw(""));
    }
}

pub fn postprocess_ai_suggestions(ai_result: &mut AIResult, settings: &Settings) {
    const MOVES_CUTOFF: RatingType = 20;
    const MAX_PER_FIELD: usize = 2;
    const MAX_SHOWN: usize = 6;

    let ratings = &mut ai_result.all_ratings;
    let annotations = &mut ai_result.annotations;
    let mut seen_pieces = HashSet::new();
    let (best_rating, _, _) = ratings[0];
    ratings.retain(|(r, _, _)| best_rating - r <= MOVES_CUTOFF);
    ratings.truncate(MAX_SHOWN);

    let filter_moves = settings.filter_ai_suggestions == FilterAISuggestions::Yes;
    annotations.clear();
    let mut index = 0;
    while index < ratings.len() {
        let (_, path, ctx) = &ratings[index];
        let keep_entry;
        match ctx {
            HiveContext::TargetField(ctx) => {
                let &from = ctx.inner();
                let to = ctx[*path.last().unwrap()];
                let from_len = annotations.entry(from).or_default().len();
                let to_len = annotations.entry(to).or_default().len();
                keep_entry = !filter_moves || (from_len < MAX_PER_FIELD && to_len < MAX_PER_FIELD);
                if keep_entry {
                    // unwrap: the entries already exist due to the previous entry calls
                    annotations.get_mut(&from).unwrap().push((index, None));
                    annotations.get_mut(&to).unwrap().push((index, None));
                }
            }
            HiveContext::Piece(ctx) => {
                let &field = ctx.inner();
                let (piece_t, _) = ctx[*path.last().unwrap()];
                let entry = annotations.entry(field).or_default();
                keep_entry = !filter_moves
                    || (entry.len() < MAX_PER_FIELD && !seen_pieces.contains(&piece_t));
                if keep_entry {
                    entry.push((index, Some(piece_t)));
                    seen_pieces.insert(piece_t);
                }
            }
            HiveContext::SkipPlayer => return,
            HiveContext::BaseField(_) => unreachable!("this context should not be possible here"),
        };
        if keep_entry {
            index += 1;
        } else {
            ratings.remove(index);
        }
    }
}

pub fn ai_suggestions(ai_result: &AIResult, game_state: &HiveGameState) -> Text<'static> {
    let desired_width = 45;
    let mut text = Text::raw(format!("    {:<40}{}", "Move Description", "Rating"));
    for (i, (rating, indizes, ctx)) in ai_result.all_ratings.iter().enumerate() {
        let mut line = match ctx {
            HiveContext::TargetField(ctx) => {
                let &from = ctx.inner();
                let piece = game_state.board()[from].top().unwrap();
                let p_color = tui_graphics::piece_color(piece.p_type);

                Line::from(vec![
                    Span::styled(format!("[{}] ", i + 1), color_palette(i)),
                    Span::raw("move "),
                    Span::styled(piece.p_type.name(), p_color),
                    Span::raw(" according to "),
                    Span::styled(format!("({})", i + 1), color_palette(i)),
                ])
            }
            HiveContext::Piece(ctx) => {
                let (p_type, _) = ctx[*indizes.last().unwrap()];
                let p_color = tui_graphics::piece_color(p_type);

                Line::from(vec![
                    Span::styled(format!("[{}] ", i + 1), color_palette(i)),
                    Span::raw("place "),
                    Span::styled(p_type.name(), p_color),
                    Span::raw(" at field "),
                    Span::styled(format!("({}{})", p_type.letter(), i + 1), color_palette(i)),
                ])
            }
            _ => unreachable!("this context should not be possible here"),
        };
        let offset = desired_width - usize::min(line.width(), desired_width - 1);
        let placeholder = " ".repeat(offset);
        line.spans.extend([
            Span::raw(placeholder),
            Span::styled(format!("{:>3}", rating), color_palette(i)),
        ]);
        text.lines.push(line);
    }
    text.extend([
        Line::raw(""),
        Line::styled(
            "press a number to apply the according move",
            ColorScheme::TEXT_GRAY,
        ),
        Line::styled("[Esc] return", ColorScheme::TEXT_GRAY),
    ]);
    text
}
