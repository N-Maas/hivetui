use crossterm::{
    event::{self, KeyCode, KeyEventKind},
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    ExecutableCommand,
};
use num_enum::TryFromPrimitive;
use ratatui::{
    layout::{Constraint, Layout},
    prelude::{CrosstermBackend, Terminal},
    style::Color,
    text::Line,
    widgets::{
        canvas::{self, Canvas, Context},
        Block, Borders, Paragraph,
    },
};
use std::io::stdout;
use std::{collections::BTreeMap, io::Stdout};
use std::{collections::HashMap, io};
use tgp::engine::{logging::EventLog, Engine, GameEngine, GameState};
use tgp_board::{open_board::OpenIndex, Board, BoardIndexable};

use crate::{
    pieces::{PieceType, Player},
    state::{HiveBoard, HiveContext, HiveGameState, HiveResult},
    tui_graphics,
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum UIState {
    /// selected menu option
    Toplevel(usize),
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
            UIState::Toplevel(_) => false,
            UIState::ShowOptions => true,
            UIState::PositionSelected(_) => true,
            UIState::PieceSelected(_) => true,
            UIState::GameFinished(_) => true,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, TryFromPrimitive)]
#[repr(u8)]
enum ZoomLevel {
    Bird = 0,
    Strategical = 1,
    Wider = 2,
    #[default]
    Normal = 3,
    Close = 4,
}

impl ZoomLevel {
    fn zoom_in(self) -> ZoomLevel {
        match self {
            ZoomLevel::Bird => ZoomLevel::Strategical,
            ZoomLevel::Strategical => ZoomLevel::Wider,
            ZoomLevel::Wider => ZoomLevel::Normal,
            ZoomLevel::Normal => ZoomLevel::Close,
            ZoomLevel::Close => ZoomLevel::Close,
        }
    }

    fn zoom_out(self) -> ZoomLevel {
        match self {
            ZoomLevel::Bird => ZoomLevel::Bird,
            ZoomLevel::Strategical => ZoomLevel::Bird,
            ZoomLevel::Wider => ZoomLevel::Strategical,
            ZoomLevel::Normal => ZoomLevel::Wider,
            ZoomLevel::Close => ZoomLevel::Normal,
        }
    }

    fn multiplier(&self) -> f64 {
        match self {
            ZoomLevel::Bird => 2.0,
            ZoomLevel::Strategical => 1.3,
            ZoomLevel::Wider => 1.0,
            ZoomLevel::Normal => 0.7,
            ZoomLevel::Close => 0.5,
        }
    }

    fn move_offset(&self) -> f64 {
        self.multiplier() * 12.0
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, TryFromPrimitive)]
#[repr(u8)]
enum WhiteTilesStyle {
    Full = 0,
    Border = 1,
    #[default]
    Hybrid = 2,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, TryFromPrimitive)]
#[repr(u8)]
enum ScreenSplitting {
    FarLeft = 0,
    Left = 1,
    #[default]
    Normal = 2,
    Right = 3,
    FarRight = 4,
}

#[derive(Debug, PartialEq, Clone, Copy)]
struct GraphicsState {
    center_x: f64,
    center_y: f64,
    zoom_level: ZoomLevel,
    piece_zoom_level: ZoomLevel,
    white_tiles_style: WhiteTilesStyle,
    splitting: ScreenSplitting,
}

impl GraphicsState {
    fn new() -> Self {
        GraphicsState {
            center_x: 0.0,
            center_y: 0.0,
            zoom_level: ZoomLevel::default(),
            piece_zoom_level: ZoomLevel::Wider,
            white_tiles_style: WhiteTilesStyle::Hybrid,
            splitting: ScreenSplitting::default(),
        }
    }

    fn zoom_in(&mut self) {
        self.zoom_level = self.zoom_level.zoom_in();
    }

    fn zoom_out(&mut self) {
        self.zoom_level = self.zoom_level.zoom_out();
    }

    fn move_in_step_size(
        &mut self,
        x_mult: f64,
        y_mult: f64,
        boundaries_x: [f64; 2],
        boundaries_y: [f64; 2],
    ) {
        self.center_x += x_mult * self.zoom_level.move_offset();
        self.center_y += y_mult * self.zoom_level.move_offset();

        self.center_x = f64::max(self.center_x, boundaries_x[0]);
        self.center_x = f64::min(self.center_x, boundaries_x[1]);
        self.center_y = f64::max(self.center_y, boundaries_y[0]);
        self.center_y = f64::min(self.center_y, boundaries_y[1]);
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
    MenuOption(usize),
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
fn pull_event(top_level: bool) -> io::Result<Option<Event>> {
    Ok(pull_key_event()?.and_then(|key| match key {
        KeyCode::Esc => Some(Event::Exit),
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
        KeyCode::Char(c) => {
            let to_index = c.to_string().parse::<usize>();
            to_index.ok().filter(|&i| i > 0).map(|i| {
                if top_level {
                    Event::MenuOption(i - 1)
                } else {
                    Event::Selection(i - 1)
                }
            })
        }
        KeyCode::Left => Some(Event::MoveLeft),
        KeyCode::Right => Some(Event::MoveRight),
        KeyCode::Up => Some(Event::MoveUp),
        KeyCode::Down => Some(Event::MoveDown),
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

    let mut engine = Engine::new_logging(2, HiveGameState::new(pieces.clone()));
    let mut board_annotations = HashMap::new();
    let mut piece_annotations = HashMap::new();
    let mut graphics_state = GraphicsState::new();
    let mut ui_state = UIState::ShowOptions; // TODO: change to top-level
    loop {
        let board = engine.data().board();
        let (boundaries_x, boundaries_y) = compute_view_boundaries(board);
        // first, pull for user input and directly apply any ui status changes or high-level commands (e.g. undo)
        let event = pull_event(matches!(ui_state, UIState::Toplevel(_)))?;
        if let Some(e) = event {
            match e {
                Event::Exit => match ui_state {
                    UIState::Toplevel(_) => break,
                    UIState::ShowOptions => ui_state = UIState::Toplevel(0),
                    UIState::PositionSelected(_) => ui_state = UIState::ShowOptions,
                    UIState::PieceSelected(_) => ui_state = UIState::ShowOptions,
                    UIState::GameFinished(_) => ui_state = UIState::Toplevel(0),
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
                Event::Selection(_) => (),
                Event::MenuOption(_) => todo!(),
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
            event.and_then(|e| e.as_selection()),
        );

        // finally we render the UI
        let state = AllState {
            game_state: engine.data(),
            board_annotations: &board_annotations,
            piece_annotations: &piece_annotations,
            ui_state,
            graphics_state,
        };
        render(&mut terminal, state, &pieces)?;
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
                (UIState::Toplevel(_), Ok(d)) => d.retract_all(),
                (UIState::Toplevel(_), Err(_)) => (),
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
                (UIState::PositionSelected(_), Ok(d)) => match d.context() {
                    HiveContext::Piece(pieces) => {
                        if let Some(index) = input.filter(|&index| index < d.option_count()) {
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
                (UIState::PieceSelected(_), Ok(d)) => match d.context() {
                    HiveContext::TargetField(board_indizes) => {
                        if let Some(index) = input.filter(|&index| index < d.option_count()) {
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
    graphics_state: GraphicsState,
}

const RED: Color = Color::from_u32(0x00E05959);

fn render(
    terminal: &mut Terminal<CrosstermBackend<Stdout>>,
    state: AllState<'_>,
    initial_pieces: &BTreeMap<PieceType, u32>,
) -> io::Result<()> {
    terminal.draw(|frame| {
        let area = frame.size();
        let mut percentage_left = match state.graphics_state.splitting {
            ScreenSplitting::FarLeft => 55,
            ScreenSplitting::Left => 60,
            ScreenSplitting::Normal => 65,
            ScreenSplitting::Right => 70,
            ScreenSplitting::FarRight => 75,
        };
        let contraints = if matches!(state.ui_state, UIState::Toplevel(_)) {
            vec![
                Constraint::Percentage(percentage_left - 20),
                Constraint::Percentage(20),
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
                .block(Block::default().title("The Board").borders(Borders::ALL))
                .x_bounds([center_x - x_len, center_x + x_len])
                .y_bounds([center_y - y_len, center_y + y_len])
                .paint(|ctx| draw_board(ctx, state));
            frame.render_widget(canvas, canvas_area);
        }

        if let UIState::Toplevel(index) = state.ui_state {
            let [menu_area, help_area] =
                *Layout::vertical([Constraint::Fill(1), Constraint::Max(7)]).split(menu_area)
            else {
                unreachable!()
            };
            let text = "[0] Player 1: <human> AI-1 AI-2 AI-3 AI-4";
            let paragraph =
                Paragraph::new(text).block(Block::default().title("Help").borders(Borders::ALL));
            frame.render_widget(paragraph, menu_area);

            let text = "This is a TUI version of the hive board game.";
            let paragraph =
                Paragraph::new(text).block(Block::default().title("Help").borders(Borders::ALL));
            frame.render_widget(paragraph, help_area);
        } else {
            let [piece_area, tooltip_area] =
                *Layout::vertical([Constraint::Fill(1), Constraint::Max(7)]).split(menu_area)
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

            let text = "[u]ndo or [r]edo a move\n\
                moving the screen: [↑↓←→] or [wasd]\n\
                zooming: [+-] or [PageDown PageUp]\n\
                back to menu: [Esc] or [q]";
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
                let dark_white = Color::from_u32(0x00D0D0D0);
                if piece.player == Player::White {
                    draw_interior(ctx, &state.graphics_state, x_mid, y_mid, dark_white);
                }
            });
    }
    ctx.layer();

    // third round: draw pieces
    for field in board.iter_fields() {
        let (x_mid, y_mid) = translate_index(field.index());
        field
            .content_checked()
            .and_then(|content| content.top())
            .inspect(|piece| tui_graphics::draw_piece(ctx, piece.p_type, x_mid, y_mid, zoom));
    }
    ctx.layer();

    // is a specific field selected?
    match state.ui_state {
        UIState::PositionSelected(index) | UIState::PieceSelected(index) => {
            let (x_mid, y_mid) = translate_index(index);
            tui_graphics::draw_interior_hex_border(ctx, x_mid, y_mid, 0.0, 0.0, RED);
        }
        _ => (),
    }
    ctx.layer();

    // print indizes
    if matches!(
        state.ui_state,
        UIState::ShowOptions | UIState::PieceSelected(_)
    ) {
        for (&board_index, &number) in state.board_annotations.iter() {
            let (x, y) = translate_index(board_index);
            ctx.print(
                x - zoom * 1.0,
                y - zoom * 2.0,
                Line::styled(format!("[{}]", number + 1), RED),
            );
        }
    }
}

fn draw_pieces(
    ctx: &mut Context<'_>,
    state: AllState<'_>,
    initial_pieces: &BTreeMap<PieceType, u32>,
) {
    let zoom = state.graphics_state.piece_zoom_level.multiplier();
    let (pieces, _) = state.game_state.pieces();
    let interior_color = match state.game_state.player() {
        Player::White => Color::from_u32(0x00D0D0D0),
        Player::Black => Color::from_u32(0x00404040),
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
        let x = 12_f64 + f64::from(u32::try_from(i).unwrap()) * 26.0;
        let y = -17_f64 - f64::from(u32::try_from(3 - depth).unwrap()) * 10.0;
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
                        let number = state.piece_annotations[&piece_t];
                        ctx.print(
                            x - zoom * 6.5,
                            -2.0,
                            Line::styled(format!("[{number}]"), RED),
                        );
                        ctx.print(
                            x + zoom * 2.0,
                            -2.0,
                            Line::styled(format!("{count}/{intial_count}"), Color::White),
                        );
                    } else {
                        ctx.print(
                            x - zoom * 4.0,
                            -2.0,
                            Line::styled(format!("{count} / {intial_count}"), Color::White),
                        );
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
            tui_graphics::draw_interior_hex_border(ctx, x, y, 1.5, 2.0, color)
        }
        WhiteTilesStyle::Hybrid => tui_graphics::draw_hex_interior(ctx, x, y, color, true),
    }
}
