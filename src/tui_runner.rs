use crossterm::{
    event::{self, KeyCode, KeyEventKind},
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    ExecutableCommand,
};
use ratatui::{
    prelude::{CrosstermBackend, Terminal},
    style::Color,
    widgets::{
        canvas::{Canvas, Context},
        Block, Borders,
    },
};
use std::io::stdout;
use std::{collections::BTreeMap, io::Stdout};
use std::{collections::HashMap, io};
use tgp::engine::{logging::EventLog, Engine, GameEngine, GameState};
use tgp_board::{open_board::OpenIndex, Board};

use crate::{
    pieces::{PieceType, Player},
    state::{HiveContext, HiveGameState, HiveResult},
    tui_graphics,
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum UIState {
    Toplevel,
    ShowOptions,
    /// selected base field
    PositionSelected(OpenIndex),
    /// selected base field
    PieceSelected(OpenIndex),
    GameFinished(HiveResult),
    // TODO...
}

impl UIState {
    fn show_game(&self) -> bool {
        match self {
            UIState::Toplevel => false,
            UIState::ShowOptions => true,
            UIState::PositionSelected(_) => true,
            UIState::PieceSelected(_) => true,
            UIState::GameFinished(_) => true,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ZoomLevel {
    ZERO,
    ONE,
    TWO,
    THREE,
}

impl ZoomLevel {
    fn zoom_in(self) -> ZoomLevel {
        match self {
            ZoomLevel::ZERO => ZoomLevel::ONE,
            ZoomLevel::ONE => ZoomLevel::TWO,
            ZoomLevel::TWO => ZoomLevel::THREE,
            ZoomLevel::THREE => ZoomLevel::THREE,
        }
    }

    fn zoom_out(self) -> ZoomLevel {
        match self {
            ZoomLevel::ZERO => ZoomLevel::ZERO,
            ZoomLevel::ONE => ZoomLevel::ZERO,
            ZoomLevel::TWO => ZoomLevel::ONE,
            ZoomLevel::THREE => ZoomLevel::TWO,
        }
    }

    fn multiplier(&self) -> f64 {
        match self {
            ZoomLevel::ZERO => 1.3,
            ZoomLevel::ONE => 1.0,
            ZoomLevel::TWO => 0.7,
            ZoomLevel::THREE => 0.5,
        }
    }

    fn move_offset(&self) -> f64 {
        self.multiplier() * 24.0
    }
}

impl Default for ZoomLevel {
    fn default() -> Self {
        ZoomLevel::TWO
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
struct GraphicsState {
    center_x: f64,
    center_y: f64,
    zoom_level: ZoomLevel,
}

impl GraphicsState {
    fn new() -> Self {
        GraphicsState {
            center_x: 0.0,
            center_y: 0.0,
            zoom_level: ZoomLevel::default(),
        }
    }

    fn zoom_in(&mut self) {
        self.zoom_level = self.zoom_level.zoom_in();
    }

    fn zoom_out(&mut self) {
        self.zoom_level = self.zoom_level.zoom_out();
    }

    fn move_in_step_size(&mut self, x_mult: f64, y_mult: f64) {
        self.center_x += x_mult * self.zoom_level.move_offset();
        self.center_y += y_mult * self.zoom_level.move_offset();
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
    Selection(usize),
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
fn pull_event() -> io::Result<Option<Event>> {
    Ok(pull_key_event()?.and_then(|key| match key {
        KeyCode::Esc => Some(Event::Exit),
        KeyCode::Char('q') => Some(Event::Exit),
        KeyCode::Char('u') => Some(Event::Undo),
        KeyCode::Char('r') => Some(Event::Redo),
        KeyCode::Char('c') => Some(Event::ContinueGame),
        KeyCode::Char('n') => Some(Event::NewGame),
        KeyCode::Char('+') => Some(Event::ZoomIn),
        KeyCode::Char('-') => Some(Event::ZoomOut),
        KeyCode::Char('w') => Some(Event::MoveUp),
        KeyCode::Char('a') => Some(Event::MoveLeft),
        KeyCode::Char('s') => Some(Event::MoveDown),
        KeyCode::Char('d') => Some(Event::MoveRight),
        KeyCode::Char(c) => {
            let to_index = c.to_string().parse::<usize>();
            match to_index {
                Ok(index) => Some(Event::Selection(index)),
                Err(_) => None,
            }
        }
        KeyCode::Left => Some(Event::MoveLeft),
        KeyCode::Right => Some(Event::MoveRight),
        KeyCode::Up => Some(Event::MoveUp),
        KeyCode::Down => Some(Event::MoveDown),
        _ => None,
    }))
}

pub fn run_in_tui(pieces: BTreeMap<PieceType, u32>) -> io::Result<()> {
    stdout().execute(EnterAlternateScreen)?;
    enable_raw_mode()?;
    let mut terminal = Terminal::new(CrosstermBackend::new(stdout()))?;
    terminal.clear()?;

    let mut engine = Engine::new_logging(2, HiveGameState::new(pieces.clone()));
    let mut annotations = HashMap::new();
    let mut graphics_state = GraphicsState::new();
    let mut ui_state = UIState::ShowOptions; // TODO: change to top-level
    loop {
        // first, pull for user input and directly apply any ui status changes or high-level commands (e.g. undo)
        let event = pull_event()?;
        if let Some(e) = event {
            match e {
                Event::Exit => match ui_state {
                    UIState::Toplevel => break,
                    UIState::ShowOptions => ui_state = UIState::Toplevel,
                    UIState::PositionSelected(_) => ui_state = UIState::ShowOptions,
                    UIState::PieceSelected(_) => ui_state = UIState::ShowOptions,
                    UIState::GameFinished(_) => ui_state = UIState::Toplevel,
                },
                Event::ContinueGame => {
                    if ui_state == UIState::Toplevel {
                        ui_state = UIState::ShowOptions;
                    }
                }
                Event::NewGame => {
                    if ui_state == UIState::Toplevel {
                        engine = Engine::new_logging(2, HiveGameState::new(pieces.clone()));
                        ui_state = UIState::ShowOptions;
                    }
                }
                Event::Undo => {
                    engine.undo_last_decision();
                }
                Event::Redo => {
                    engine.redo_decision();
                }
                Event::ZoomIn => graphics_state.zoom_in(),
                Event::ZoomOut => graphics_state.zoom_out(),
                Event::MoveLeft => graphics_state.move_in_step_size(-1.0, 0.0),
                Event::MoveRight => graphics_state.move_in_step_size(1.0, 0.0),
                Event::MoveUp => graphics_state.move_in_step_size(0.0, 1.0),
                Event::MoveDown => graphics_state.move_in_step_size(0.0, -1.0),
                Event::Selection(_) => todo!(),
            }
        }
        // now we pull the game state, possibly applying the user input
        annotations.clear();
        update_game_state_and_fill_input_mapping(
            &mut engine,
            &mut annotations,
            &mut ui_state,
            event.and_then(|e| e.as_selection()),
        );

        // finally we render the UI
        let state = AllState {
            game_state: engine.data(),
            ui_state: ui_state,
            graphics_state,
        };
        render(&mut terminal, state)?;
    }

    stdout().execute(LeaveAlternateScreen)?;
    disable_raw_mode()?;
    Ok(())
}

fn update_game_state_and_fill_input_mapping(
    engine: &mut Engine<HiveGameState, EventLog<HiveGameState>>,
    annotations: &mut HashMap<OpenIndex, usize>,
    ui_state: &mut UIState,
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
                                    annotations.insert(board_index, i);
                                }
                            }
                        }
                        HiveContext::SkipPlayer => todo!("what are edge cases here?"),
                        _ => unreachable!("this can not be a follow-up decision"),
                    }
                }
                (UIState::PositionSelected(_), Ok(d)) => match d.context() {
                    HiveContext::Piece(pieces) => {
                        if let Some(index) = input.filter(|&index| index < d.option_count()) {
                            d.select_option(index);
                        } else {
                            // TODO: we need a UI representation of the pieces!
                        }
                    }
                    HiveContext::TargetField(_) => panic!("this should never happen"),
                    _ => unreachable!("this must be a follow-up decision"),
                },
                (UIState::PositionSelected(_), Err(_)) => {
                    *ui_state = UIState::ShowOptions;
                }
                (UIState::PieceSelected(_), Ok(d)) => match d.context() {
                    HiveContext::TargetField(board_indizes) => {
                        if let Some(index) = input.filter(|&index| index < d.option_count()) {
                            d.select_option(index);
                        } else {
                            // fill the annotation mapping
                            for (i, &board_index) in board_indizes.into_iter().enumerate() {
                                annotations.insert(board_index, i);
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
    ui_state: UIState,
    graphics_state: GraphicsState,
}

fn render(
    terminal: &mut Terminal<CrosstermBackend<Stdout>>,
    state: AllState<'_>,
) -> io::Result<()> {
    let zoom = state.graphics_state.zoom_level.multiplier();
    let center_x = state.graphics_state.center_x;
    let center_y = state.graphics_state.center_y;

    terminal.draw(|frame| {
        let area = frame.size();
        let x_len = zoom * f64::from(area.width);
        let y_len = zoom * 2.1 * f64::from(area.height);
        let canvas = Canvas::default()
            .block(Block::default().title("Canvas").borders(Borders::ALL))
            .x_bounds([center_x - x_len, center_x + x_len])
            .y_bounds([center_y - y_len, center_y + y_len])
            .paint(|ctx| draw(ctx, state));
        frame.render_widget(canvas, area);
    })?;
    Ok(())
}

fn translate_index(OpenIndex { x, y }: OpenIndex) -> (f64, f64) {
    let x = f64::from(i32::try_from(x).unwrap());
    let y = f64::from(i32::try_from(y).unwrap());
    (x * 21.0, y * 24.0 - x * 12.0)
}

fn draw(ctx: &mut Context<'_>, state: AllState<'_>) {
    let zoom = state.graphics_state.zoom_level.multiplier();
    let board = state.game_state.board();
    // first round: draw borders
    for field in board.iter_fields() {
        let (x_mid, y_mid) = translate_index(field.index());
        if field.content_checked().is_some() {
            tui_graphics::draw_hex_border(ctx, x_mid, y_mid);
        }
    }
    ctx.layer();

    // second round: draw interiors
    for field in board.iter_fields() {
        let (x_mid, y_mid) = translate_index(field.index());
        field
            .content_checked()
            .and_then(|content| content.top())
            .inspect(|piece| {
                match piece.player {
                    Player::White => {
                        tui_graphics::draw_hex_interior(ctx, x_mid, y_mid, Color::Gray)
                    }
                    Player::Black => (),
                };
            });
    }
    ctx.layer();

    // third round: draw pieces
    for field in board.iter_fields() {
        let (x_mid, y_mid) = translate_index(field.index());
        field
            .content_checked()
            .and_then(|content| content.top())
            .inspect(|piece| match piece.p_type {
                PieceType::Queen => tui_graphics::draw_queen(ctx, x_mid, y_mid, zoom),
                PieceType::Ant => tui_graphics::draw_ant(ctx, x_mid, y_mid, zoom),
                PieceType::Spider => tui_graphics::draw_spider(ctx, x_mid, y_mid, zoom),
                PieceType::Grasshopper => tui_graphics::draw_grasshopper(ctx, x_mid, y_mid, zoom),
                PieceType::Beetle => tui_graphics::draw_beetle(ctx, x_mid, y_mid, zoom),
            });
    }
}
