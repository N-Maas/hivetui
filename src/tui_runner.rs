use crossterm::{
    event::{self, KeyCode, KeyEvent, KeyEventKind},
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    ExecutableCommand,
};
use ratatui::{
    prelude::{CrosstermBackend, Terminal}, style::Color, widgets::{
        canvas::{Canvas, Context},
        Block, Borders,
    }
};
use std::io;
use std::io::stdout;
use std::{collections::BTreeMap, io::Stdout};
use tgp::engine::{logging::EventLog, Engine};

use crate::{pieces::PieceType, state::HiveGameState, tui_graphics};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum UIState {
    Toplevel,
    ShowOptions,
    PositionSelected, // DATA
    PieceSelected,
    // TODO...
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
    Undo,
    Redo,
    ZoomIn,
    ZoomOut,
    MoveLeft,
    MoveRight,
    MoveUp,
    MoveDown,
}

/// pull event in internal represenation: handles mapping of raw key event
fn pull_event() -> io::Result<Option<Event>> {
    Ok(pull_key_event()?.and_then(|key| match key {
        KeyCode::Esc => Some(Event::Exit),
        KeyCode::Char('q') => Some(Event::Exit),
        KeyCode::Char('u') => Some(Event::Undo),
        KeyCode::Char('r') => Some(Event::Redo),
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

    let mut engine = Engine::new_logging(2, HiveGameState::new(pieces));
    let mut graphics_state = GraphicsState::new();
    let mut ui_state = UIState::Toplevel;
    loop {
        // first, pull for user input and directly apply any ui status changes
        let event = pull_event()?;
        match event {
            Some(Event::Exit) => match ui_state {
                UIState::Toplevel => break,
                UIState::ShowOptions => ui_state = UIState::Toplevel,
                UIState::PositionSelected => ui_state = UIState::ShowOptions,
                UIState::PieceSelected => ui_state = UIState::ShowOptions,
            },
            Some(Event::ZoomIn) => graphics_state.zoom_in(),
            Some(Event::ZoomOut) => graphics_state.zoom_out(),
            Some(Event::MoveLeft) => graphics_state.move_in_step_size(-1.0, 0.0),
            Some(Event::MoveRight) => graphics_state.move_in_step_size(1.0, 0.0),
            Some(Event::MoveUp) => graphics_state.move_in_step_size(0.0, 1.0),
            Some(Event::MoveDown) => graphics_state.move_in_step_size(0.0, -1.0),
            _ => (),
        }

        // now we render the UI
        let state = AllState {
            engine: &engine,
            ui_state: ui_state,
            graphics_state,
            event
        };
        render(&mut terminal, state)?;
    }

    stdout().execute(LeaveAlternateScreen)?;
    disable_raw_mode()?;
    Ok(())
}


type HiveEngine = Engine<HiveGameState, EventLog<HiveGameState>>;

#[derive(Clone, Copy)]
struct AllState<'a> {
    engine: &'a HiveEngine,
    ui_state: UIState,
    graphics_state: GraphicsState,
    event: Option<Event>,
}


fn render(terminal: &mut Terminal<CrosstermBackend<Stdout>>, state: AllState<'_>) -> io::Result<()> {
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


fn draw(ctx: &mut Context<'_>, state: AllState<'_>) {
    let zoom = state.graphics_state.zoom_level.multiplier();
    tui_graphics::draw_hex_border(ctx, -30.0, 20.0);
    tui_graphics::draw_hex_interior(ctx, -30.0, 20.0, Color::Gray);
    ctx.layer();
    tui_graphics::draw_grasshopper(ctx, 20.0, -10.0, zoom);
    ctx.layer();
    // piece_drawing::draw_interior_hex_border(ctx, -30.0, -20.0, 0.5, 1.0, Color::Blue);
    tui_graphics::draw_interior_hex_border(ctx, 20.0, 15.0, 1.5, 1.0, Color::Blue);
}
