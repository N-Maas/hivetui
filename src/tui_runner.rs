use crossterm::{
    event::{self, KeyCode, KeyEventKind},
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    ExecutableCommand,
};
use ratatui::{
    layout::{Constraint, Layout},
    prelude::{CrosstermBackend, Terminal},
    style::Color,
    text::{Line, Span, Text},
    widgets::{
        canvas::{Canvas, Context},
        Block, Borders, Paragraph,
    },
};
use std::io::stdout;
use std::{collections::BTreeMap, io::Stdout};
use std::{collections::HashMap, io};
use tgp::engine::{logging::EventLog, Engine, GameEngine, GameState};
use tgp_board::{
    open_board::OpenIndex,
    structures::directions::{DirectionEnumerable, HexaDirection},
    Board, BoardIndexable,
};

use crate::{
    pieces::{PieceType, Player},
    state::{HiveBoard, HiveContext, HiveGameState, HiveResult},
    tui_graphics,
};

use self::{
    animations::{build_blink_animation, build_complete_piece_move_animation, Animation, Layer},
    tui_settings::{BordersStyle, GraphicsState, MenuSetting, ScreenSplitting, WhiteTilesStyle},
};

mod animations;
mod tui_settings;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum UIState {
    /// selected menu option
    Toplevel,
    ShowOptions,
    /// selected base field
    PositionSelected(OpenIndex),
    /// selected base field
    PieceSelected(OpenIndex),
    GameFinished(HiveResult),
    PlaysAnimation,
}

impl UIState {
    fn show_game(&self) -> bool {
        match self {
            UIState::Toplevel => false,
            UIState::ShowOptions => true,
            UIState::PositionSelected(_) => true,
            UIState::PieceSelected(_) => true,
            UIState::GameFinished(_) => true,
            UIState::PlaysAnimation => true,
        }
    }
}

fn pull_key_event() -> io::Result<Option<KeyCode>> {
    if event::poll(std::time::Duration::from_millis(16))? {
        if let event::Event::Key(key) = event::read()? {
            if key.kind == KeyEventKind::Press {
                return Ok(Some(key.code));
            }
        }
    }
    Ok(None)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Event {
    // TODO: two-digit numbers!!
    Selection(usize),
    MenuOption(usize),
    MenuUp,
    MenuDown,
    MenuIncrease,
    MenuDecrease,
    TwoDigitInit,
    TwoDigitAdd(usize),
    Exit,
    ContinueGame,
    NewGame,
    Undo,
    Redo,
    ZoomIn,
    ZoomOut,
    MoveLeft,
    MoveRight,
    MoveUp,
    MoveDown,
}

impl Event {
    fn as_selection(&self) -> Option<usize> {
        if let Event::Selection(index) = *self {
            Some(index)
        } else {
            None
        }
    }
}

/// pull event in internal represenation: handles mapping of raw key event
fn pull_event(top_level: bool, two_digit: bool) -> io::Result<Option<Event>> {
    Ok(pull_key_event()?.and_then(|key| match key {
        KeyCode::Esc => Some(if top_level {
            Event::ContinueGame
        } else {
            Event::Exit
        }),
        KeyCode::Char('q') => Some(Event::Exit),
        KeyCode::Char('u') => Some(Event::Undo).filter(|_| !top_level),
        KeyCode::Char('r') => Some(Event::Redo).filter(|_| !top_level),
        KeyCode::Char('c') => Some(Event::ContinueGame).filter(|_| top_level),
        KeyCode::Char('n') => Some(Event::NewGame).filter(|_| top_level),
        KeyCode::Char('+') => Some(Event::ZoomIn),
        KeyCode::Char('-') => Some(Event::ZoomOut),
        KeyCode::Char('w') => Some(Event::MoveUp),
        KeyCode::Char('a') => Some(Event::MoveLeft),
        KeyCode::Char('s') => Some(Event::MoveDown),
        KeyCode::Char('d') => Some(Event::MoveRight),
        KeyCode::Enter | KeyCode::Char(' ') => Some(if top_level {
            Event::ContinueGame
        } else {
            Event::TwoDigitInit
        }),
        KeyCode::Char(c) => {
            let to_index = c.to_string().parse::<usize>();
            to_index
                .ok()
                .filter(|&i| i > 0 || (!top_level && two_digit))
                .map(|i| {
                    if top_level {
                        Event::MenuOption(i - 1)
                    } else if two_digit {
                        Event::TwoDigitAdd(i)
                    } else {
                        Event::Selection(i - 1)
                    }
                })
        }
        KeyCode::Left => Some(if top_level {
            Event::MenuDecrease
        } else {
            Event::MoveLeft
        }),
        KeyCode::Right => Some(if top_level {
            Event::MenuIncrease
        } else {
            Event::MoveRight
        }),
        KeyCode::Up => Some(if top_level {
            Event::MenuUp
        } else {
            Event::MoveUp
        }),
        KeyCode::Down => Some(if top_level {
            Event::MenuDown
        } else {
            Event::MoveDown
        }),
        KeyCode::PageDown => Some(Event::ZoomIn),
        KeyCode::PageUp => Some(Event::ZoomOut),
        _ => None,
    }))
}

pub fn run_in_tui(pieces: BTreeMap<PieceType, u32>) -> io::Result<()> {
    stdout().execute(EnterAlternateScreen)?;
    enable_raw_mode()?;
    let mut terminal = Terminal::new(CrosstermBackend::new(stdout()))?;
    terminal.hide_cursor()?;
    terminal.clear()?;

    let settings = tui_settings::build_settings();
    let mut engine = Engine::new_logging(2, HiveGameState::new(pieces.clone()));
    let mut board_annotations = HashMap::new();
    let mut piece_annotations = HashMap::new();
    let mut graphics_state = GraphicsState::new();
    let mut ui_state = UIState::Toplevel;
    let mut menu_index = 0;
    let mut current_animation: Option<Animation> = None;
    let mut digits: Option<Vec<usize>> = None;
    loop {
        let board = engine.data().board();
        let (boundaries_x, boundaries_y) = compute_view_boundaries(board);
        if current_animation.as_ref().is_some_and(|a| !a.is_finished()) && ui_state.show_game() {
            ui_state = UIState::PlaysAnimation;
        } else if ui_state == UIState::PlaysAnimation {
            ui_state = UIState::ShowOptions;
        }
        // first, pull for user input and directly apply any ui status changes or high-level commands (e.g. undo)
        let mut event = pull_event(ui_state == UIState::Toplevel, digits.is_some())?;
        if let Some(e) = event {
            // two digit handling happens first
            match e {
                Event::TwoDigitInit => {
                    digits = Some(Vec::new());
                }
                Event::TwoDigitAdd(digit) => {
                    let digits_ref = digits.as_mut().unwrap();
                    digits_ref.push(digit);
                    if let &[first, second] = digits_ref.as_slice() {
                        let val = 10 * first + second;
                        (val > 0).then(|| {
                            event = Some(Event::Selection(val - 1));
                            digits = None;
                        });
                    }
                }
                _ => digits = None,
            }
            // general event mapping
            match e {
                Event::Exit => match ui_state {
                    UIState::Toplevel => break,
                    UIState::ShowOptions => ui_state = UIState::Toplevel,
                    UIState::PositionSelected(_) => ui_state = UIState::ShowOptions,
                    UIState::PieceSelected(_) => ui_state = UIState::ShowOptions,
                    UIState::GameFinished(_) => ui_state = UIState::Toplevel,
                    UIState::PlaysAnimation => ui_state = UIState::Toplevel,
                },
                Event::ContinueGame => {
                    ui_state = UIState::ShowOptions;
                }
                Event::NewGame => {
                    engine = Engine::new_logging(2, HiveGameState::new(pieces.clone()));
                    ui_state = UIState::ShowOptions;
                }
                Event::Undo => {
                    engine.undo_last_decision();
                }
                Event::Redo => {
                    engine.redo_decision();
                }
                Event::ZoomIn => graphics_state.zoom_in(),
                Event::ZoomOut => graphics_state.zoom_out(),
                Event::MoveLeft => {
                    graphics_state.move_in_step_size(-1.0, 0.0, boundaries_x, boundaries_y)
                }
                Event::MoveRight => {
                    graphics_state.move_in_step_size(1.0, 0.0, boundaries_x, boundaries_y)
                }
                Event::MoveUp => {
                    graphics_state.move_in_step_size(0.0, 1.0, boundaries_x, boundaries_y)
                }
                Event::MoveDown => {
                    graphics_state.move_in_step_size(0.0, -1.0, boundaries_x, boundaries_y)
                }
                Event::MenuOption(new_index) => {
                    if new_index < settings.len() {
                        menu_index = new_index;
                    }
                }
                Event::MenuUp => {
                    if menu_index > 0 {
                        menu_index -= 1;
                    }
                }
                Event::MenuDown => {
                    if menu_index + 1 < settings.len() {
                        menu_index += 1;
                    }
                }
                Event::MenuIncrease => {
                    settings[menu_index].increase(&mut graphics_state);
                }
                Event::MenuDecrease => {
                    settings[menu_index].decrease(&mut graphics_state);
                }
                Event::Selection(_) => (),
                Event::TwoDigitInit => (),
                Event::TwoDigitAdd(_) => (),
            }
        }
        // now we pull the game state, possibly applying the user input
        board_annotations.clear();
        piece_annotations.clear();
        update_game_state_and_fill_input_mapping(
            &mut engine,
            &mut board_annotations,
            &mut piece_annotations,
            &mut ui_state,
            &graphics_state,
            &mut current_animation,
            event.and_then(|e| e.as_selection()),
        );

        // finally we render the UI
        let state = AllState {
            game_state: engine.data(),
            board_annotations: &board_annotations,
            piece_annotations: &piece_annotations,
            ui_state,
            animation: current_animation.as_ref(),
            menu_index,
            graphics_state,
        };
        render(
            &mut terminal,
            &settings,
            state,
            &mut graphics_state,
            &pieces,
        )?;
        // update animation state
        if let Some(animation) = current_animation.as_mut() {
            if animation.is_finished() {
                current_animation = None;
            } else {
                animation.next_step();
            }
        }
    }

    stdout().execute(LeaveAlternateScreen)?;
    disable_raw_mode()?;
    Ok(())
}

fn compute_view_boundaries(board: &HiveBoard) -> ([f64; 2], [f64; 2]) {
    let mut min_x = 0_f64;
    let mut max_x = 0_f64;
    let mut min_y = 0_f64;
    let mut max_y = 0_f64;
    for board_index in board.all_indices() {
        let (this_x, this_y) = translate_index(board_index);
        min_x = f64::min(min_x, this_x);
        max_x = f64::max(max_x, this_x);
        min_y = f64::min(min_y, this_y);
        max_y = f64::max(max_y, this_y);
    }
    ([min_x - 10.0, max_x + 10.0], [min_y - 10.0, max_y + 10.0])
}

fn update_game_state_and_fill_input_mapping(
    engine: &mut Engine<HiveGameState, EventLog<HiveGameState>>,
    board_annotations: &mut HashMap<OpenIndex, usize>,
    piece_annotations: &mut HashMap<PieceType, usize>,
    ui_state: &mut UIState,
    graphics_state: &GraphicsState,
    animation: &mut Option<Animation>,
    input: Option<usize>,
) {
    match engine.pull() {
        GameState::PendingEffect(pe) => {
            // TODO: start animations here?!
            pe.next_effect()
        }
        GameState::PendingDecision(decision) => {
            let follow_up = decision.try_into_follow_up_decision();
            match (&ui_state, follow_up) {
                (UIState::Toplevel, Ok(d)) => d.retract_all(),
                (UIState::Toplevel, Err(_)) => (),
                (UIState::ShowOptions, Ok(d)) => d.retract_all(),
                (UIState::ShowOptions, Err(d)) => {
                    match d.context() {
                        HiveContext::BaseField(board_indizes) => {
                            if let Some(index) = input.filter(|&index| index < d.option_count()) {
                                // We need to update the UI state according to the selected option.
                                // This means, we need to find out whether we selected a piece or an empty
                                // field
                                let b_index = board_indizes[index];
                                let board = d.data().board();
                                if board[b_index].is_empty() {
                                    *ui_state = UIState::PositionSelected(b_index);
                                } else {
                                    *ui_state = UIState::PieceSelected(b_index);
                                }
                                d.select_option(index);
                            } else {
                                // fill the annotation mapping
                                for (i, &board_index) in board_indizes.into_iter().enumerate() {
                                    board_annotations.insert(board_index, i);
                                }
                            }
                        }
                        HiveContext::SkipPlayer => todo!("what are edge cases here?"),
                        _ => unreachable!("this can not be a follow-up decision"),
                    }
                }
                (UIState::PositionSelected(b_index), Ok(d)) => match d.context() {
                    HiveContext::Piece(pieces) => {
                        if let Some(index) = input.filter(|&index| index < d.option_count()) {
                            let player = Player::from(d.player());
                            if graphics_state.should_play_animation(player) {
                                *animation = Some(build_blink_animation(
                                    graphics_state,
                                    player,
                                    *b_index,
                                    false,
                                ));
                            }

                            d.select_option(index);
                        } else {
                            // fill the annotation mapping
                            for (i, &(piece_type, _)) in pieces.into_iter().enumerate() {
                                piece_annotations.insert(piece_type, i);
                            }
                        }
                    }
                    HiveContext::TargetField(_) => panic!("this should never happen"),
                    _ => unreachable!("this must be a follow-up decision"),
                },
                (UIState::PositionSelected(_), Err(_)) => {
                    *ui_state = UIState::ShowOptions;
                }
                (UIState::PieceSelected(b_index), Ok(d)) => match d.context() {
                    HiveContext::TargetField(board_indizes) => {
                        if let Some(index) = input.filter(|&index| index < d.option_count()) {
                            let player = Player::from(d.player());
                            if graphics_state.should_play_animation(player) {
                                let board = d.data().board();
                                let piece_t =
                                    board.get(*b_index).and_then(|c| c.top()).unwrap().p_type;
                                let target = board_indizes[index];
                                *animation = Some(build_complete_piece_move_animation(
                                    graphics_state,
                                    piece_t,
                                    player,
                                    *b_index,
                                    target,
                                ));
                            }

                            d.select_option(index);
                        } else {
                            // fill the annotation mapping
                            for (i, &board_index) in board_indizes.into_iter().enumerate() {
                                board_annotations.insert(board_index, i);
                            }
                        }
                    }
                    HiveContext::Piece(_) => panic!("this should never happen"),
                    _ => unreachable!("this must be a follow-up decision"),
                },
                (UIState::PieceSelected(_), Err(_)) => {
                    *ui_state = UIState::ShowOptions;
                }
                (UIState::GameFinished(_), follow_up) => {
                    *ui_state = UIState::ShowOptions;
                    if let Ok(d) = follow_up {
                        d.retract_all();
                    }
                }
                (UIState::PlaysAnimation, Ok(_)) => {
                    unreachable!("selection during animation should be impossible")
                }
                (UIState::PlaysAnimation, Err(_)) => (),
            }
        }
        GameState::Finished(finished) => {
            let result = finished
                .data()
                .result()
                .expect("game is finished and must have a result");
            if ui_state.show_game() {
                *ui_state = UIState::GameFinished(result);
            }
        }
    }
}

#[derive(Clone, Copy)]
struct AllState<'a> {
    game_state: &'a HiveGameState,
    board_annotations: &'a HashMap<OpenIndex, usize>,
    piece_annotations: &'a HashMap<PieceType, usize>,
    ui_state: UIState,
    animation: Option<&'a Animation>,
    menu_index: usize,
    graphics_state: GraphicsState,
}

const RED: Color = Color::from_u32(0x00E05959);
const ORANGE: Color = Color::from_u32(0x00D89040);
const DARK_WHITE: Color = Color::from_u32(0x00DADADA);

fn render(
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

fn translate_index(OpenIndex { x, y }: OpenIndex) -> (f64, f64) {
    let x = f64::from(i32::try_from(x).unwrap());
    let y = f64::from(i32::try_from(y).unwrap());
    (x * 21.0, y * 24.0 - x * 12.0)
}

fn draw_board(ctx: &mut Context<'_>, state: AllState<'_>) {
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

fn draw_pieces(
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

fn draw_interior(
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
