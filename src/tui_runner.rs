use crossterm::{
    event::{self, KeyCode, KeyEventKind},
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    ExecutableCommand,
};
use ratatui::prelude::{CrosstermBackend, Terminal};
use std::{backtrace::Backtrace, cell::RefCell, io::stdout, panic, process};
use std::{
    collections::BTreeMap,
    sync::{Arc, Mutex},
};
use std::{collections::HashMap, io};
use tgp::{
    engine::{logging::EventLog, Engine, FollowUpDecision, GameEngine, GameState},
    vec_context::VecContext,
};
use tgp_ai::RatingType;
use tgp_board::{open_board::OpenIndex, Board, BoardIndexable};

use crate::{
    pieces::{PieceType, Player},
    state::{HiveBoard, HiveContext, HiveGameState, HiveResult},
    tui_runner::{tui_animations::loader, tui_settings::AIMoves},
};

use self::{
    ai_worker::start_ai_worker_thread,
    tui_animations::{build_blink_animation, build_complete_piece_move_animation, Animation},
    tui_rendering::find_losing_queen,
    tui_settings::{is_ai_setting, AILevel, GraphicsState, PlayerType, Settings},
};

mod ai_worker;
mod tui_animations;
mod tui_rendering;
mod tui_settings;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum UIState {
    /// selected menu option
    Toplevel,
    /// true if the current turn must be skipped
    ShowOptions(bool),
    /// selected base field
    PositionSelected(OpenIndex),
    /// selected base field
    PieceSelected(OpenIndex),
    GameFinished(HiveResult),
    /// true if the animation is waiting for ai
    PlaysAnimation(bool),
}

impl UIState {
    fn show_game(&self) -> bool {
        match self {
            UIState::Toplevel => false,
            UIState::ShowOptions(_) => true,
            UIState::PositionSelected(_) => true,
            UIState::PieceSelected(_) => true,
            UIState::GameFinished(_) => true,
            UIState::PlaysAnimation(_) => true,
        }
    }
}

#[derive(Default)]
struct AnimationState {
    animation: Option<Animation>,
    count: usize,
}

struct AnimationStateSetter<'a> {
    animation: &'a mut Option<Animation>,
    count: &'a mut usize,
}

impl AnimationStateSetter<'_> {
    fn set_animation(self, a: Animation) {
        *self.animation = Some(a);
        *self.count += 1;
    }
}

impl AnimationState {
    fn animation(&self) -> Option<&Animation> {
        self.animation.as_ref()
    }

    fn runs(&self) -> bool {
        self.animation.is_some()
    }

    fn next_step(&mut self) {
        if let Some(animation) = self.animation.as_mut() {
            if animation.is_finished() {
                self.animation = None;
            } else {
                animation.next_step();
            }
        }
    }

    fn try_set(&mut self) -> Option<AnimationStateSetter<'_>> {
        match self.animation {
            Some(_) => None,
            None => Some(AnimationStateSetter {
                animation: &mut self.animation,
                count: &mut self.count,
            }),
        }
    }

    fn stop(&mut self) {
        self.animation = None;
    }

    fn reset(&mut self) {
        self.animation = None;
        self.count = 0;
    }

    fn reset_count(&mut self) {
        self.count = 0;
    }
}

#[derive(Debug)]
struct AIResult {
    player: Player,
    best_move: Box<[usize]>,
    all_ratings: Vec<(RatingType, Box<[usize]>, HiveContext)>,
}

#[derive(Debug)]
enum AIMessage {
    Start(AILevel, Box<HiveGameState>),
    Cancel,
}

#[derive(Default)]
struct AIExchange {
    for_runner: Option<Box<AIResult>>,
    for_worker: Option<AIMessage>,
}

struct AIState {
    exchange_point: Arc<Mutex<AIExchange>>,
    current_player: Player,
    is_started: bool,
    // TODO: update in case of menu changes?!
    should_use_ai: bool,
    should_show_animation: bool,
    result: Option<AIResult>,
    animation_progress: usize,
}

impl AIState {
    const AI_DELAY: usize = 40;

    fn new() -> Self {
        Self {
            exchange_point: Arc::new(Mutex::default()),
            current_player: Player::White,
            is_started: false,
            should_use_ai: false,
            should_show_animation: false,
            result: None,
            animation_progress: 0,
        }
    }

    fn reset(&mut self) {
        {
            let mut exchange = self.exchange_point.lock().unwrap();
            exchange.for_runner = None;
            if self.is_started {
                exchange.for_worker = Some(AIMessage::Cancel);
            }
        }
        self.is_started = false;
        self.result = None;
        self.animation_progress = 0;
    }

    fn update(
        &mut self,
        state: &HiveGameState,
        settings: &Settings,
        player: Player,
        animation: &mut AnimationState,
    ) {
        // TODO: interaction with undo
        assert!(self.current_player == player || !self.is_started);
        if !self.is_started {
            assert!(self.result.is_none() && self.animation_progress == 0);
            let level = if settings.is_ai(player) {
                settings.player_type(player).into_ai_level()
            } else {
                settings.ai_assistant
            };
            self.exchange_point.lock().unwrap().for_worker =
                Some(AIMessage::Start(level, Box::new(state.clone())));
            self.current_player = player;
            self.should_use_ai = settings.is_ai(player) && settings.ai_moves == AIMoves::AUTOMATIC;
            self.should_show_animation = settings.is_ai(player);
            self.is_started = true;
        }

        {
            let mut exchange = self.exchange_point.lock().unwrap();
            if let Some(result) = exchange.for_runner.take() {
                self.result = Some(*result);
            }
        }
        if self.should_show_animation && (self.animation_progress > 0 || !animation.runs()) {
            self.animation_progress += 1;
        }
        if self.should_show_animation && self.animation_progress < Self::AI_DELAY {
            animation
                .try_set()
                .map(|s| s.set_animation(Animation::new(loader(settings, 30))));
        }
    }

    fn use_ai(&mut self, settings: &Settings) {
        if settings.is_ai(self.current_player) {
            self.should_use_ai = true;
            self.should_show_animation = true;
        }
    }

    fn dont_use_ai(&mut self) {
        self.should_use_ai = false;
        self.should_show_animation = false;
        self.animation_progress = 0;
    }

    fn result(&mut self) -> Option<AIResult> {
        if self.animation_progress >= Self::AI_DELAY {
            self.result.take()
        } else {
            None
        }
    }

    fn animation_has_started(&self) -> bool {
        self.animation_progress > 0
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
    Cancel,
    SoftCancel,
    ContinueGame,
    NewGame,
    Undo,
    Redo,
    LetAIMove,
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
fn pull_event(ui_state: UIState, two_digit: bool, animation: bool) -> io::Result<Option<Event>> {
    let top_level = ui_state == UIState::Toplevel;
    let is_skip = ui_state == UIState::ShowOptions(true);
    Ok(pull_key_event()?.and_then(|key| match key {
        KeyCode::Esc => Some(if top_level {
            Event::ContinueGame
        } else {
            Event::Exit
        }),
        KeyCode::Char('q') => Some(Event::Exit),
        KeyCode::Char('u') | KeyCode::Char('z') => Some(Event::Undo).filter(|_| !top_level),
        KeyCode::Char('r') | KeyCode::Char('y') => Some(Event::Redo).filter(|_| !top_level),
        KeyCode::Char('c') => Some(if top_level {
            Event::ContinueGame
        } else {
            Event::Cancel
        }),
        KeyCode::Char('n') => Some(if top_level {
            Event::NewGame
        } else {
            Event::LetAIMove
        }),
        KeyCode::Char('+') => Some(Event::ZoomIn),
        KeyCode::Char('-') => Some(Event::ZoomOut),
        KeyCode::Char('w') => Some(Event::MoveUp),
        KeyCode::Char('a') => Some(Event::MoveLeft),
        KeyCode::Char('s') => Some(Event::MoveDown),
        KeyCode::Char('d') => Some(Event::MoveRight),
        KeyCode::Enter | KeyCode::Char(' ') => {
            if top_level {
                Some(Event::ContinueGame)
            } else if !animation && !two_digit && !is_skip {
                Some(Event::TwoDigitInit)
            } else if key == KeyCode::Enter {
                Some(Event::SoftCancel)
            } else {
                None
            }
        }
        KeyCode::Backspace => Some(Event::Cancel),
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

thread_local! {
    static MSG: RefCell<Option<String>> = RefCell::new(None);
    static BACKTRACE: RefCell<Option<Backtrace>> = RefCell::new(None);
}

pub fn run_in_tui(pieces: BTreeMap<PieceType, u32>) -> io::Result<()> {
    stdout().execute(EnterAlternateScreen)?;
    enable_raw_mode()?;

    // do some panic reporting
    panic::set_hook(Box::new(|info| {
        let payload = info.payload();
        let msg = payload.downcast_ref::<String>().cloned().or_else(|| {
            payload
                .downcast_ref::<&'static str>()
                .map(|s| s.to_string())
        });
        msg.map(|value| {
            MSG.with(|b| b.borrow_mut().replace(value));
        });
        let trace = Backtrace::force_capture();
        BACKTRACE.with(move |b| b.borrow_mut().replace(trace));
    }));
    let result = panic::catch_unwind(|| run_in_tui_impl(pieces));

    stdout().execute(LeaveAlternateScreen)?;
    disable_raw_mode()?;

    match result {
        Ok(val) => val, // just propagate I/O error
        Err(_) => {
            let msg = MSG
                .with(|b| b.borrow_mut().take())
                .map_or(String::new(), |m| m + "\n");
            let trace = BACKTRACE
                .with(|b| b.borrow_mut().take())
                .map_or(String::new(), |b| b.to_string());
            eprintln!("Oh no! A panic (i.e. internal error) occured:\n{msg}\n{trace}");
            process::exit(1);
        }
    }
}

/// this implements the main event loop
pub fn run_in_tui_impl(pieces: BTreeMap<PieceType, u32>) -> io::Result<()> {
    let mut terminal = Terminal::new(CrosstermBackend::new(stdout()))?;
    terminal.hide_cursor()?;
    terminal.clear()?;

    let settings_list = tui_settings::build_settings();
    let mut settings = Settings::default();
    settings.black_player_type = PlayerType::AI2;
    let mut engine = Engine::new_logging(2, HiveGameState::new(pieces.clone()));
    let mut board_annotations = HashMap::new();
    let mut piece_annotations = HashMap::new();
    let mut graphics_state = GraphicsState::new();
    let mut ui_state = UIState::Toplevel;
    let mut ai_state = AIState::new();
    let mut menu_index = 2;
    let mut animation_state: AnimationState = AnimationState::default();
    let mut digits: Option<Vec<usize>> = None;
    start_ai_worker_thread(ai_state.exchange_point.clone());

    loop {
        let board = engine.data().board();
        let (boundaries_x, boundaries_y) = compute_view_boundaries(board);
        if animation_state
            .animation()
            .is_some_and(|a| !a.is_finished())
            && ui_state.show_game()
            && !matches!(ui_state, UIState::GameFinished(_))
            && !ai_state.animation_has_started()
        {
            // TODO: interaction with AI?!
            ui_state = UIState::PlaysAnimation(false);
        } else if matches!(ui_state, UIState::PlaysAnimation(_)) {
            ui_state = UIState::ShowOptions(false);
        }
        // first, pull for user input and directly apply any ui status changes or high-level commands (e.g. undo)
        let mut event = pull_event(ui_state, digits.is_some(), animation_state.runs())?;
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
                    UIState::ShowOptions(_) => ui_state = UIState::Toplevel,
                    UIState::PositionSelected(_) => ui_state = UIState::ShowOptions(false),
                    UIState::PieceSelected(_) => ui_state = UIState::ShowOptions(false),
                    UIState::GameFinished(_) => ui_state = UIState::Toplevel,
                    UIState::PlaysAnimation(_) => ui_state = UIState::Toplevel,
                },
                Event::Cancel => {
                    animation_state.stop();
                    ai_state.dont_use_ai();
                }
                Event::SoftCancel => {
                    animation_state.stop();
                }
                Event::LetAIMove => {
                    ai_state.use_ai(&settings);
                }
                Event::ContinueGame => {
                    ui_state = UIState::ShowOptions(false);
                }
                Event::NewGame => {
                    engine = Engine::new_logging(2, HiveGameState::new(pieces.clone()));
                    graphics_state.center_x = 0.0;
                    graphics_state.center_y = 0.0;
                    animation_state.reset();
                    ai_state.reset();
                    ui_state = UIState::ShowOptions(false);
                }
                Event::Undo => {
                    if engine.undo_last_decision() {
                        animation_state.reset();
                        ai_state.reset();
                    }
                }
                Event::Redo => {
                    if engine.redo_decision() {
                        animation_state.reset();
                        ai_state.reset();
                    }
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
                    if new_index < settings_list.len() {
                        menu_index = new_index;
                    }
                }
                Event::MenuUp => {
                    if menu_index > 0 {
                        menu_index -= 1;
                    }
                }
                Event::MenuDown => {
                    if menu_index + 1 < settings_list.len() {
                        menu_index += 1;
                    }
                }
                Event::MenuIncrease => {
                    settings_list[menu_index].increase(&mut settings);
                    if is_ai_setting(menu_index) {
                        ai_state.reset();
                    }
                }
                Event::MenuDecrease => {
                    settings_list[menu_index].decrease(&mut settings);
                    if is_ai_setting(menu_index) {
                        ai_state.reset();
                    }
                }
                Event::Selection(_) => (),
                Event::TwoDigitInit => (),
                Event::TwoDigitAdd(_) => (),
            }
        }
        // now we pull the game state, possibly applying the user input
        board_annotations.clear();
        piece_annotations.clear();
        let input = event.and_then(|e| {
            if ui_state == UIState::ShowOptions(true) && e == Event::SoftCancel {
                Some(0)
            } else {
                e.as_selection()
            }
        });
        update_game_state_and_fill_input_mapping(
            &mut engine,
            &mut board_annotations,
            &mut piece_annotations,
            &mut ui_state,
            &mut ai_state,
            &settings,
            &mut animation_state,
            input,
        );

        // finally we render the UI
        let state = tui_rendering::AllState {
            game_state: engine.data(),
            settings: settings,
            board_annotations: &board_annotations,
            piece_annotations: &piece_annotations,
            ui_state,
            animation_state: &animation_state,
            menu_index,
            graphics_state,
        };
        tui_rendering::render(&mut terminal, &settings_list, state, &mut settings, &pieces)?;
        // update animation state
        animation_state.next_step();
    }
    Ok(())
}

fn compute_view_boundaries(board: &HiveBoard) -> ([f64; 2], [f64; 2]) {
    let mut min_x = 0_f64;
    let mut max_x = 0_f64;
    let mut min_y = 0_f64;
    let mut max_y = 0_f64;
    for board_index in board.all_indices() {
        let (this_x, this_y) = tui_rendering::translate_index(board_index);
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
    ai_state: &mut AIState,
    settings: &Settings,
    animation_state: &mut AnimationState,
    input: Option<usize>,
) {
    match engine.pull() {
        GameState::PendingEffect(pe) => {
            // TODO: start animations here?!
            ai_state.reset();
            animation_state.reset_count();
            pe.next_effect()
        }
        GameState::PendingDecision(decision) => {
            ai_state.update(
                decision.data(),
                settings,
                Player::from(decision.player()),
                animation_state,
            );
            let follow_up = decision.try_into_follow_up_decision();
            if ai_state.should_use_ai && ui_state.show_game() {
                let decision = match follow_up {
                    Ok(d) => {
                        d.retract_all();
                        return;
                    }
                    Err(d) => d,
                };

                if let Some(result) = ai_state.result() {
                    animation_state.stop();
                    assert!(result.player == Player::from(decision.player()));
                    let best = result.best_move;

                    let context = match decision.context() {
                        HiveContext::BaseField(context) => context,
                        HiveContext::SkipPlayer => {
                            decision.select_option(0);
                            return;
                        }
                        _ => unreachable!(""),
                    };
                    let position = context[best[0]];
                    decision.select_option(best[0]);

                    if let GameState::PendingDecision(next) = engine.pull() {
                        next.into_follow_up_decision().map(|d| match d.context() {
                            HiveContext::TargetField(targets) => {
                                handle_moved_piece(
                                    d,
                                    targets,
                                    best[1],
                                    position,
                                    settings,
                                    animation_state,
                                );
                            }
                            HiveContext::Piece(_) => {
                                handle_placed_piece(
                                    d,
                                    best[1],
                                    position,
                                    settings,
                                    animation_state,
                                );
                            }
                            _ => unreachable!(""),
                        });
                    }
                } else {
                    *ui_state = UIState::PlaysAnimation(ai_state.animation_has_started());
                }
                return; // don't risk a weird decision state
            }

            match (*ui_state, follow_up) {
                (UIState::Toplevel, Ok(d)) => d.retract_all(),
                (UIState::Toplevel, Err(_)) => (),
                (UIState::ShowOptions(_), Ok(d)) => d.retract_all(),
                (UIState::ShowOptions(is_skip), Err(d)) => {
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
                                *ui_state = UIState::ShowOptions(false);
                            }
                        }
                        HiveContext::SkipPlayer => {
                            if is_skip && input == Some(0) {
                                d.select_option(0);
                            } else {
                                *ui_state = UIState::ShowOptions(true);
                            }
                        }
                        _ => unreachable!("this can not be a follow-up decision"),
                    }
                }
                (UIState::PositionSelected(b_index), Ok(d)) => match d.context() {
                    HiveContext::Piece(pieces) => {
                        if let Some(index) = input.filter(|&index| index < d.option_count()) {
                            animation_state.stop();
                            handle_placed_piece(d, index, b_index, settings, animation_state);
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
                    *ui_state = UIState::ShowOptions(false);
                }
                (UIState::PieceSelected(b_index), Ok(d)) => match d.context() {
                    HiveContext::TargetField(board_indizes) => {
                        if let Some(index) = input.filter(|&index| index < d.option_count()) {
                            animation_state.stop();
                            handle_moved_piece(
                                d,
                                board_indizes,
                                index,
                                b_index,
                                settings,
                                animation_state,
                            );
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
                    *ui_state = UIState::ShowOptions(false);
                }
                (UIState::GameFinished(_), follow_up) => {
                    *ui_state = UIState::ShowOptions(false);
                    if let Ok(d) = follow_up {
                        d.retract_all();
                    }
                }
                (UIState::PlaysAnimation(is_ai), Ok(d)) => {
                    assert!(is_ai, "selection during animation should be impossible");
                    d.retract_all();
                }
                (UIState::PlaysAnimation(_), Err(_)) => (),
            }
        }
        GameState::Finished(finished) => {
            ai_state.reset();
            let result = finished
                .data()
                .result()
                .expect("game is finished and must have a result");
            if let Some(set_animation) = animation_state.try_set() {
                if *set_animation.count <= 1 {
                    *set_animation.count = 1;
                    if let Some((index, player)) =
                        find_losing_queen(finished.data().board(), result)
                    {
                        set_animation.set_animation(build_blink_animation(
                            settings, player, index, false, 0,
                        ));
                    }
                }
            }
            if ui_state.show_game() && !animation_state.runs() {
                *ui_state = UIState::GameFinished(result);
            }
        }
    }
}

fn handle_placed_piece(
    dec: FollowUpDecision<'_, HiveGameState, EventLog<HiveGameState>>,
    index: usize,
    pos: OpenIndex,
    settings: &Settings,
    animation: &mut AnimationState,
) {
    let player = Player::from(dec.player());
    if settings.should_play_animation(player) {
        animation
            .try_set()
            .unwrap()
            .set_animation(build_blink_animation(settings, player, pos, false, 0));
    }
    dec.select_option(index);
}

fn handle_moved_piece(
    dec: FollowUpDecision<'_, HiveGameState, EventLog<HiveGameState>>,
    context: VecContext<OpenIndex, OpenIndex>,
    index: usize,
    pos: OpenIndex,
    settings: &Settings,
    animation: &mut AnimationState,
) {
    let player = Player::from(dec.player());
    if settings.should_play_animation(player) {
        let board = dec.data().board();
        let piece_t = board.get(pos).and_then(|c| c.top()).unwrap().p_type;
        let target = context[index];
        let target_level = board[target].len();
        animation
            .try_set()
            .unwrap()
            .set_animation(build_complete_piece_move_animation(
                settings,
                piece_t,
                player,
                pos,
                target,
                target_level,
            ));
    }
    dec.select_option(index);
}
