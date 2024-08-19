use crossterm::{
    event::{self, KeyCode, KeyEventKind},
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    ExecutableCommand,
};
use ratatui::prelude::{CrosstermBackend, Terminal};
use std::{collections::HashMap, ffi::OsString, io};
use std::{io::stdout, panic, process};
use text_input::TextInput;
use tgp::{
    engine::{logging::EventLog, Engine, FollowUpDecision, GameState, LoggingEngine},
    vec_context::VecContext,
};
use tgp_board::{open_board::OpenIndex, Board, BoardIndexable};

use crate::{
    io::{load_game, IOManager},
    panic_handling::{get_panic_data, setup_panic_reporting},
    pieces::{PieceType, Player},
    state::{HiveBoard, HiveContext, HiveGameState, HiveResult},
    tui_runner::{
        tui_animations::loader, tui_rendering::postprocess_ai_suggestions, tui_settings::AIMoves,
    },
    worker::{
        ai_worker::{start_ai_worker_thread, AIEndpoint, AIResult, AIStart},
        io_worker::{start_io_worker_thread, SaveGame},
        MessageForMaster,
    },
};

use self::{
    tui_animations::{build_blink_animation, build_complete_piece_move_animation, Animation},
    tui_rendering::{find_losing_queen, translate_index},
    tui_settings::{
        AutomaticCameraMoves, GameSetup, GraphicsState, PlayerType, SettingRenderer,
        SettingSelection, Settings,
    },
};

// mod dynamic_layout;
mod text_input;
mod tui_animations;
mod tui_rendering;
pub mod tui_settings;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum UIState {
    // TODO: show enemy pieces during animation?
    Toplevel,
    /// the current scrolling
    RulesSummary(u16),
    /// the currently selected option
    GameSetup(usize),
    /// the currently selected save
    LoadScreen(usize),
    SaveScreen,
    /// 1. true if the current turn must be skipped
    /// 2. true if pieces are switched
    ShowOptions(bool, bool),
    /// selected base field
    PositionSelected(OpenIndex),
    /// selected base field
    PieceSelected(OpenIndex),
    GameFinished(HiveResult),
    /// true if the animation is waiting for ai
    PlaysAnimation(bool),
    ShowAIMoves,
}

impl UIState {
    fn top_level(&self) -> bool {
        match self {
            UIState::Toplevel => true,
            UIState::RulesSummary(_) => true,
            UIState::GameSetup(_) => true,
            UIState::LoadScreen(_) => true,
            UIState::SaveScreen => true,
            UIState::ShowOptions(_, _) => false,
            UIState::PositionSelected(_) => false,
            UIState::PieceSelected(_) => false,
            UIState::GameFinished(_) => false,
            UIState::PlaysAnimation(_) => false,
            UIState::ShowAIMoves => false,
        }
    }

    fn show_game(&self) -> bool {
        match self {
            UIState::Toplevel => false,
            UIState::RulesSummary(_) => false,
            UIState::GameSetup(_) => false,
            UIState::LoadScreen(_) => false,
            UIState::SaveScreen => false,
            UIState::ShowOptions(_, _) => true,
            UIState::PositionSelected(_) => true,
            UIState::PieceSelected(_) => true,
            UIState::GameFinished(_) => true,
            UIState::PlaysAnimation(_) => true,
            UIState::ShowAIMoves => false,
        }
    }

    fn alternative_selection(&mut self) -> Option<&mut usize> {
        match self {
            UIState::GameSetup(index) | UIState::LoadScreen(index) => Some(index),
            _ => None,
        }
    }

    fn state_changed(&mut self) {
        match self {
            UIState::PositionSelected(_) => *self = UIState::ShowOptions(false, false),
            UIState::PieceSelected(_) => *self = UIState::ShowOptions(false, false),
            UIState::ShowAIMoves => *self = UIState::ShowOptions(false, false),
            _ => (),
        };
    }
}

struct CameraMove {
    current_step: usize,
    max_steps: usize,
    from: (f64, f64),
    to: (f64, f64),
}

impl CameraMove {
    fn build(steps: usize, from: (f64, f64), to: (f64, f64)) -> Self {
        Self {
            current_step: 0,
            max_steps: steps,
            from,
            to,
        }
    }

    fn make_step(&mut self) -> Option<(f64, f64)> {
        if self.current_step >= self.max_steps {
            return None;
        }

        let pos_from_frac = |fraction| {
            let progress = 2.0
                * if fraction <= 0.5 {
                    fraction * fraction
                } else {
                    0.5 - (1.0 - fraction) * (1.0 - fraction)
                };
            (
                (1.0 - progress) * self.from.0 + progress * self.to.0,
                (1.0 - progress) * self.from.1 + progress * self.to.1,
            )
        };

        let frac_old = self.current_step as f64 / self.max_steps as f64;
        self.current_step += 1;
        let frac = self.current_step as f64 / self.max_steps as f64;
        let (x_old, y_old) = pos_from_frac(frac_old);
        let (x_new, y_new) = pos_from_frac(frac);
        Some((x_new - x_old, y_new - y_old))
    }
}

#[derive(Default)]
struct AnimationState {
    animation: Option<Animation>,
    count: usize,
    center: Option<(f64, f64)>,
}

struct AnimationStateSetter<'a> {
    animation: &'a mut Option<Animation>,
    count: &'a mut usize,
    center: &'a mut Option<(f64, f64)>,
}

impl AnimationStateSetter<'_> {
    fn set_animation(self, a: Animation, center: Option<(f64, f64)>) {
        *self.animation = Some(a);
        *self.count += 1;
        *self.center = center;
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
                self.stop();
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
                center: &mut self.center,
            }),
        }
    }

    fn stop(&mut self) {
        self.animation = None;
        self.center = None;
    }

    fn reset(&mut self) {
        self.stop();
        self.count = 0;
    }

    fn reset_count(&mut self) {
        self.count = 0;
    }
}

struct AIState {
    endpoint: AIEndpoint,
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

    fn new(endpoint: AIEndpoint) -> Self {
        Self {
            endpoint,
            current_player: Player::White,
            is_started: false,
            should_use_ai: false,
            should_show_animation: false,
            result: None,
            animation_progress: 0,
        }
    }

    fn reset(&mut self) {
        self.endpoint.cancel();
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
            self.endpoint
                .send(AIStart(level.as_difficulty(), Box::new(state.clone())));
            self.current_player = player;
            self.should_use_ai = settings.is_ai(player) && settings.ai_moves == AIMoves::Automatic;
            self.should_show_animation = settings.is_ai(player);
            self.is_started = true;
        }

        {
            match self.endpoint.get_msg() {
                Some(MessageForMaster::Msg(mut result)) => {
                    postprocess_ai_suggestions(result.as_mut(), settings);
                    self.result = Some(*result);
                }
                Some(MessageForMaster::Killed(msg, trace)) => {
                    // try to print the ai error
                    stdout().execute(LeaveAlternateScreen).unwrap();
                    disable_raw_mode().unwrap();
                    eprintln!("Oh no! A panic (i.e. internal error) occured:\n{msg}\n{trace}");
                    process::exit(1);
                }
                None => (),
            }
        }
        if self.should_show_animation {
            if self.animation_progress > 0 || !animation.runs() {
                self.animation_progress += 1;
            }
            if self.animation_progress < Self::AI_DELAY || self.result.is_none() {
                if let Some(s) = animation.try_set() {
                    s.set_animation(Animation::new(loader(settings, 30)), None)
                }
            }
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

    fn result(&self) -> Option<&AIResult> {
        if self.animation_progress >= Self::AI_DELAY {
            self.result.as_ref()
        } else {
            None
        }
    }

    fn actual_result(&self) -> Option<&AIResult> {
        self.result.as_ref()
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
    SelectMenuOption,
    MenuUp,
    MenuDown,
    MenuIncrease,
    MenuDecrease,
    TwoDigitInit,
    TwoDigitAdd(usize),
    Exit,
    Switch,
    Cancel,
    SoftCancel,
    ContinueGame,
    StartGame,
    NewGame,
    LoadGame,
    SaveGame,
    Undo,
    Redo,
    LetAIMove,
    Help,
    ZoomIn,
    ZoomOut,
    MoveLeft,
    MoveRight,
    MoveUp,
    MoveDown,
    ScrollUp,
    ScrollDown,
    EnterChar(char),
    CursorLeft,
    CursorRight,
    DeleteLeft,
    DeleteRight,
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
    let show_suggestions = ui_state == UIState::ShowAIMoves;
    let top_level = ui_state.top_level();
    let rules_summary = matches!(ui_state, UIState::RulesSummary(_));
    let game_setup = matches!(ui_state, UIState::GameSetup(_));
    let is_skip = matches!(ui_state, UIState::ShowOptions(true, _));
    Ok(pull_key_event()?.and_then(|key| {
        if ui_state == UIState::SaveScreen {
            let result = match key {
                KeyCode::Char(c) => Some(Event::EnterChar(c)),
                KeyCode::Left => Some(Event::CursorLeft),
                KeyCode::Right => Some(Event::CursorRight),
                KeyCode::Backspace => Some(Event::DeleteLeft),
                KeyCode::Delete => Some(Event::DeleteRight),
                _ => None,
            };
            if result.is_some() {
                return result;
            }
        }
        match key {
            KeyCode::Esc => Some(Event::Exit).filter(|_| ui_state != UIState::Toplevel),
            KeyCode::Char('q') => Some(Event::Exit),
            KeyCode::Char('u') | KeyCode::Char('z') => Some(Event::Undo).filter(|_| !top_level),
            KeyCode::Char('r') | KeyCode::Char('y') => Some(Event::Redo).filter(|_| !top_level),
            KeyCode::Char('c') => Some(if top_level {
                Event::ContinueGame
            } else {
                Event::Cancel
            })
            .filter(|_| !rules_summary && !game_setup),
            KeyCode::Char('n') => Some(if top_level {
                Event::NewGame
            } else {
                Event::LetAIMove
            })
            .filter(|_| !rules_summary && !game_setup),
            KeyCode::Char('h') => Some(Event::Help),
            KeyCode::Char('k') => Some(Event::SaveGame),
            KeyCode::Char('l') => Some(Event::LoadGame),
            KeyCode::Char('+') => Some(Event::ZoomIn).filter(|_| !rules_summary),
            KeyCode::Char('-') => Some(Event::ZoomOut).filter(|_| !rules_summary),
            KeyCode::Char('w') => Some(if rules_summary {
                Event::ScrollUp
            } else {
                Event::MoveUp
            }),
            KeyCode::Char('a') => Some(Event::MoveLeft).filter(|_| !rules_summary),
            KeyCode::Char('s') => Some(if rules_summary {
                Event::ScrollDown
            } else {
                Event::MoveDown
            }),
            KeyCode::Char('d') => Some(Event::MoveRight).filter(|_| !rules_summary),
            KeyCode::Enter | KeyCode::Char(' ') => {
                if ui_state == UIState::Toplevel {
                    Some(Event::ContinueGame)
                } else if matches!(ui_state, UIState::GameSetup(_)) {
                    Some(Event::StartGame)
                } else if matches!(ui_state, UIState::LoadScreen(_) | UIState::SaveScreen) {
                    Some(Event::SelectMenuOption)
                } else if matches!(ui_state, UIState::RulesSummary(_)) && key == KeyCode::Enter {
                    Some(Event::Exit)
                } else if top_level {
                    Some(Event::ScrollDown)
                } else if !animation && !two_digit && !is_skip && !show_suggestions {
                    Some(Event::TwoDigitInit)
                } else if key == KeyCode::Enter {
                    Some(Event::SoftCancel)
                } else {
                    None
                }
            }
            KeyCode::Tab => Some(Event::Switch),
            KeyCode::Backspace => Some(Event::Cancel),
            KeyCode::Char(c) => {
                let to_index = c.to_string().parse::<usize>();
                to_index
                    .ok()
                    .filter(|&i| i > 0 || top_level || two_digit)
                    .map(|i| {
                        if top_level {
                            let i = if i == 0 { 10 } else { i };
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
            KeyCode::PageDown => Some(Event::ZoomIn).filter(|_| !rules_summary),
            KeyCode::PageUp => Some(Event::ZoomOut).filter(|_| !rules_summary),
            _ => None,
        }
    }))
}

pub fn run_in_tui() -> io::Result<()> {
    stdout().execute(EnterAlternateScreen)?;
    enable_raw_mode()?;
    setup_panic_reporting();

    let result = panic::catch_unwind(run_in_tui_impl);

    stdout().execute(LeaveAlternateScreen)?;
    disable_raw_mode()?;

    match result {
        Ok(val) => val, // just propagate I/O error
        Err(_) => {
            let (msg, trace) = get_panic_data();
            eprintln!("Oh no! A panic (i.e. internal error) occured:\n{msg}\n{trace}");
            process::exit(1);
        }
    }
}

/// this implements the main event loop
pub fn run_in_tui_impl() -> io::Result<()> {
    let mut terminal = Terminal::new(CrosstermBackend::new(stdout()))?;
    terminal.hide_cursor()?;
    terminal.clear()?;

    let ai_endpoint = start_ai_worker_thread();
    let io_endpoint = start_io_worker_thread();

    let mut io_manager = IOManager::new();
    let setting_renderer = SettingRenderer::build();
    let mut settings = Settings::default();
    let mut game_setup = GameSetup::default();
    settings.black_player_type = PlayerType::AI2;
    let mut engine = Engine::new_logging(2, game_setup.new_game_state());
    let mut board_annotations = HashMap::new();
    let mut piece_annotations = HashMap::new();
    let mut graphics_state = GraphicsState::new();
    let mut ui_state = UIState::Toplevel;
    let mut ai_state = AIState::new(ai_endpoint);
    let mut menu_selection = SettingSelection::General(2);
    let mut text_input = TextInput::new("".to_string());
    let mut animation_state: AnimationState = AnimationState::default();
    let mut camera_move: Option<CameraMove> = None;
    let mut digits: Option<Vec<usize>> = None;

    if let Some(io_manager) = io_manager.as_ref() {
        // attempt to load saved state
        // TODO: config + setting which avoids this?
        let path = io_manager.autosave_path();
        if path.is_file() {
            match load_game(&path) {
                Ok(result) => (engine, game_setup) = result,
                // TODO: properly handle error
                Err(e) => eprintln!("Warning: could not load saved state: {e:?}"),
            }
        }
    } else {
        // TODO: proper message
        eprintln!("Warning: could not initialize game data directory");
    }
    let do_autosave = |io_manager: &Option<IOManager>,
                       setup: &GameSetup,
                       engine: &LoggingEngine<HiveGameState>| {
        if let Some(io_manager) = io_manager.as_ref() {
            io_endpoint.send(SaveGame(
                io_manager.autosave_path(),
                setup.clone(),
                engine.serialized_log(),
            ));
        }
        // TODO: what about errors?
    };

    loop {
        let board = engine.data().board();
        let (boundaries_x, boundaries_y) = compute_view_boundaries(board);
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
            let mut start_game = |game_setup, engine, ui_state: &mut UIState| {
                graphics_state.center_x = 0.0;
                graphics_state.center_y = 0.0;
                animation_state.reset();
                ai_state.reset();
                *ui_state = UIState::ShowOptions(false, false);
                do_autosave(&io_manager, game_setup, engine);
            };
            match e {
                Event::Exit => match ui_state {
                    UIState::Toplevel => break,
                    UIState::RulesSummary(_) => ui_state = UIState::Toplevel,
                    UIState::GameSetup(_) => ui_state = UIState::Toplevel,
                    UIState::LoadScreen(_) => ui_state = UIState::Toplevel,
                    UIState::SaveScreen => ui_state = UIState::Toplevel,
                    UIState::ShowOptions(_, _) => ui_state = UIState::Toplevel,
                    UIState::PositionSelected(_) => ui_state = UIState::ShowOptions(false, false),
                    UIState::PieceSelected(_) => ui_state = UIState::ShowOptions(false, false),
                    UIState::GameFinished(_) => ui_state = UIState::Toplevel,
                    UIState::PlaysAnimation(_) => ui_state = UIState::Toplevel,
                    UIState::ShowAIMoves => ui_state = UIState::ShowOptions(false, false),
                },
                Event::Switch => {
                    // TODO: use similar to escape to return from some menus?
                    if let UIState::GameSetup(index) = ui_state {
                        ui_state = UIState::GameSetup(game_setup.switched_index(index, 2));
                    } else if ui_state.top_level() {
                        menu_selection = menu_selection.switched();
                        let max_index = setting_renderer.max_index(menu_selection);
                        if menu_selection.index() >= max_index {
                            *menu_selection.index_mut() = max_index - 1;
                        }
                    } else if let UIState::ShowOptions(is_skip, switch) = ui_state {
                        ui_state = UIState::ShowOptions(is_skip, !switch);
                    }
                }
                Event::Cancel => {
                    animation_state.stop();
                    ai_state.dont_use_ai();
                }
                Event::SoftCancel => {
                    animation_state.stop();
                    if ui_state == UIState::ShowAIMoves {
                        ui_state = UIState::ShowOptions(false, false);
                    }
                    if let UIState::RulesSummary(_) = ui_state {
                        ui_state = UIState::Toplevel;
                    }
                }
                Event::LetAIMove => {
                    ai_state.use_ai(&settings);
                }
                Event::ContinueGame => {
                    ui_state = UIState::ShowOptions(false, false);
                }
                Event::NewGame => {
                    ui_state = UIState::GameSetup(2);
                }
                Event::LoadGame => {
                    if let Some(io_manager) = io_manager.as_mut() {
                        match io_manager.recompute_save_files_list() {
                            Ok(_) => ui_state = UIState::LoadScreen(2),
                            // TODO: proper error handling
                            Err(e) => eprintln!("Error: could not load save files: {e}"),
                        }
                    }
                }
                Event::SaveGame => {
                    if let Some(io_manager) = io_manager.as_mut() {
                        match io_manager.recompute_save_files_list() {
                            Ok(_) => {
                                ui_state = UIState::SaveScreen;
                                // TODO: proper name (e.g. using a timestamp)
                                text_input = TextInput::new("My Save Game".to_string());
                            }
                            // TODO: proper error handling
                            Err(e) => eprintln!("Error: could not load save files: {e}"),
                        }
                    }
                }
                Event::StartGame => {
                    engine = Engine::new_logging(2, game_setup.new_game_state());
                    start_game(&game_setup, &engine, &mut ui_state);
                }
                Event::Undo => {
                    if engine.undo_last_decision() {
                        animation_state.reset();
                        ai_state.reset();
                        ui_state.state_changed();
                        do_autosave(&io_manager, &game_setup, &engine);
                    }
                }
                Event::Redo => {
                    if engine.redo_decision() {
                        animation_state.reset();
                        ai_state.reset();
                        ui_state.state_changed();
                        do_autosave(&io_manager, &game_setup, &engine);
                    }
                }
                Event::Help => match ui_state {
                    UIState::Toplevel | UIState::LoadScreen(_) | UIState::SaveScreen => {
                        ui_state = UIState::RulesSummary(0)
                    }
                    UIState::RulesSummary(_) => ui_state = UIState::Toplevel,
                    UIState::ShowOptions(false, _)
                    | UIState::PositionSelected(_)
                    | UIState::PieceSelected(_) => ui_state = UIState::ShowAIMoves,
                    UIState::ShowAIMoves => ui_state = UIState::ShowOptions(false, false),
                    UIState::ShowOptions(true, _)
                    | UIState::GameSetup(_)
                    | UIState::PlaysAnimation(_)
                    | UIState::GameFinished(_) => (),
                },
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
                    if let UIState::GameSetup(_) = ui_state {
                        if new_index < game_setup.max_index() + 2 {
                            ui_state = UIState::GameSetup(new_index);
                        }
                    } else if let UIState::LoadScreen(_) = ui_state {
                        let max_len = io_manager.as_ref().unwrap().save_files_list().len();
                        if new_index < max_len + 2 {
                            ui_state = UIState::LoadScreen(new_index);
                        }
                    } else if new_index < setting_renderer.max_index(menu_selection) {
                        *menu_selection.index_mut() = new_index;
                    }
                }
                Event::MenuUp => {
                    if let Some(index) = ui_state.alternative_selection() {
                        *index = index.saturating_sub(1);
                    } else if menu_selection.index() > 0 {
                        *menu_selection.index_mut() -= 1;
                    }
                }
                Event::MenuDown => {
                    if let UIState::GameSetup(index) = &mut ui_state {
                        if *index + 1 < game_setup.max_index() + 2 {
                            *index += 1;
                        }
                    } else if let UIState::LoadScreen(index) = &mut ui_state {
                        let max_len = io_manager.as_ref().unwrap().save_files_list().len();
                        if *index + 1 < max_len + 2 {
                            *index += 1;
                        }
                    } else if menu_selection.index() + 1
                        < setting_renderer.max_index(menu_selection)
                    {
                        *menu_selection.index_mut() += 1;
                    }
                }
                Event::MenuIncrease => {
                    if let UIState::GameSetup(index) = ui_state {
                        if index >= 2 {
                            game_setup.increase_at(index, 2);
                        } else {
                            setting_renderer.get_player(index).increase(&mut settings);
                            ai_state.reset();
                        }
                    } else if let UIState::LoadScreen(index) = ui_state {
                        if index < 2 {
                            setting_renderer.get_player(index).increase(&mut settings);
                            ai_state.reset();
                        }
                    } else {
                        setting_renderer.get(menu_selection).increase(&mut settings);
                        if setting_renderer.is_ai_setting(menu_selection) {
                            ai_state.reset();
                        }
                    }
                }
                Event::MenuDecrease => {
                    if let UIState::GameSetup(index) = ui_state {
                        if index >= 2 {
                            game_setup.decrease_at(index, 2);
                        } else {
                            setting_renderer.get_player(index).decrease(&mut settings);
                            ai_state.reset();
                        }
                    } else if let UIState::LoadScreen(index) = ui_state {
                        if index < 2 {
                            setting_renderer.get_player(index).decrease(&mut settings);
                            ai_state.reset();
                        }
                    } else {
                        setting_renderer.get(menu_selection).decrease(&mut settings);
                        if setting_renderer.is_ai_setting(menu_selection) {
                            ai_state.reset();
                        }
                    }
                }
                Event::ScrollUp => {
                    if let UIState::RulesSummary(i) = ui_state {
                        ui_state = UIState::RulesSummary(i.saturating_sub(1));
                    }
                }
                Event::ScrollDown => {
                    if let UIState::RulesSummary(i) = ui_state {
                        ui_state = UIState::RulesSummary(i + 1);
                    }
                }
                Event::SelectMenuOption => {
                    let io_manager = io_manager.as_ref().unwrap();
                    if let UIState::LoadScreen(index) = ui_state {
                        if index >= 2 {
                            let name = io_manager
                                .save_files_list()
                                .get(index - 2)
                                .map(|(name, _)| name);
                            name.map(|name| {
                                let path = io_manager.save_file_path(name);
                                match load_game(&path) {
                                    Ok(result) => {
                                        (engine, game_setup) = result;
                                        start_game(&game_setup, &engine, &mut ui_state);
                                    }
                                    Err(e) => eprintln!("Error: could not load game: {e:?}"),
                                }
                            });
                        }
                    } else if let UIState::SaveScreen = ui_state {
                        let name = text_input.get_text_or_default();
                        let path = io_manager.save_file_path(&OsString::from(name));
                        io_endpoint.send(SaveGame(
                            path,
                            game_setup.clone(),
                            engine.serialized_log(),
                        ));
                        // TODO what about errors?
                        ui_state = UIState::Toplevel;
                    }
                }
                Event::EnterChar(c) => text_input.insert_char(c),
                Event::CursorLeft => text_input.move_cursor_left(),
                Event::CursorRight => text_input.move_cursor_right(),
                Event::DeleteLeft => text_input.delete_char_left(),
                Event::DeleteRight => text_input.delete_char_right(),
                Event::Selection(_) => (),
                Event::TwoDigitInit => (),
                Event::TwoDigitAdd(_) => (),
            }
        }
        // now we pull the game state, possibly applying the user input
        board_annotations.clear();
        piece_annotations.clear();
        let input = event.and_then(|e| {
            if matches!(ui_state, UIState::ShowOptions(true, _)) && e == Event::SoftCancel {
                Some(0)
            } else {
                e.as_selection()
            }
        });
        let state_change = update_game_state_and_fill_input_mapping(
            &mut engine,
            &mut board_annotations,
            &mut piece_annotations,
            &mut ui_state,
            &mut ai_state,
            &settings,
            &mut animation_state,
            input,
        );
        if state_change == GameStateChange::DecisionCompleted {
            do_autosave(&io_manager, &game_setup, &engine);
        }

        // update ui state if an animation was started
        if animation_state
            .animation()
            .is_some_and(|a| !a.is_finished())
            && ui_state.show_game()
            && !matches!(ui_state, UIState::GameFinished(_))
            && !ai_state.animation_has_started()
        {
            ui_state = UIState::PlaysAnimation(false);
        } else if matches!(ui_state, UIState::PlaysAnimation(_)) {
            ui_state = UIState::ShowOptions(false, false);
        }
        // do we need to update the camera position?
        if let Some(c_move) = camera_move.as_mut() {
            let active = c_move
                .make_step()
                .map(|(x_diff, y_diff)| {
                    graphics_state.center_x += x_diff;
                    graphics_state.center_y += y_diff;
                })
                .is_some();
            if !active {
                camera_move = None;
            }
        }
        // finally we render the UI
        let state = tui_rendering::AllState {
            game_state: engine.data(),
            settings,
            board_annotations: &board_annotations,
            piece_annotations: &piece_annotations,
            ui_state,
            ai_state: &ai_state,
            animation_state: &animation_state,
            menu_selection,
            text_input: &text_input,
            graphics_state,
        };
        let (x_bounds, y_bounds) = tui_rendering::render(
            &mut terminal,
            &setting_renderer,
            state,
            &mut settings,
            &game_setup,
            &io_manager,
        )?;

        // update animation state
        animation_state.next_step();
        // should we move the camera?
        let Some(animation) = animation_state.animation() else {
            continue;
        };
        if ui_state.top_level()
            || settings.automatic_camera_moves == AutomaticCameraMoves::Off
            || animation.current_step > 1
        {
            continue;
        }
        if let Some((x_to, y_to)) = animation_state.center {
            let x_range = (x_bounds[1] - x_bounds[0]).abs();
            let y_range = (y_bounds[1] - y_bounds[0]).abs();
            let x_mid = graphics_state.center_x;
            let y_mid = graphics_state.center_y;
            if (x_to - x_mid).abs() > 0.33 * x_range || (y_to - y_mid).abs() > 0.33 * y_range {
                let new_x = x_mid.max(x_to - 0.15 * x_range).min(x_to + 0.15 * x_range);
                let new_y = y_mid.max(y_to - 0.15 * y_range).min(y_to + 0.15 * y_range);
                camera_move = Some(CameraMove::build(
                    animation.total_steps / 2,
                    (x_mid, y_mid),
                    (new_x, new_y),
                ));
            }
        }
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

#[derive(Debug, PartialEq, Eq)]
enum GameStateChange {
    EffectCompleted,
    DecisionCompleted,
    None,
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
) -> GameStateChange {
    match engine.pull() {
        GameState::PendingEffect(pe) => {
            ai_state.reset();
            animation_state.reset_count();
            pe.next_effect();
            ui_state.state_changed();
            GameStateChange::EffectCompleted
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
                        return GameStateChange::None;
                    }
                    Err(d) => d,
                };

                if let Some(result) = ai_state.result() {
                    animation_state.stop();
                    assert!(result.player == Player::from(decision.player()));
                    let best = &result.best_move;
                    apply_computed_move(engine, best, settings, animation_state);
                    return GameStateChange::DecisionCompleted;
                } else {
                    *ui_state = UIState::PlaysAnimation(ai_state.animation_has_started());
                }
                return GameStateChange::None; // don't risk a weird decision state
            }

            match (*ui_state, follow_up) {
                (
                    UIState::Toplevel
                    | UIState::RulesSummary(_)
                    | UIState::GameSetup(_)
                    | UIState::LoadScreen(_)
                    | UIState::SaveScreen,
                    Ok(d),
                ) => {
                    d.retract_all();
                    GameStateChange::None
                }
                (
                    UIState::Toplevel
                    | UIState::RulesSummary(_)
                    | UIState::GameSetup(_)
                    | UIState::LoadScreen(_)
                    | UIState::SaveScreen,
                    Err(_),
                ) => GameStateChange::None,
                (UIState::ShowOptions(_, _), Ok(d)) => {
                    d.retract_all();
                    GameStateChange::None
                }
                (UIState::ShowOptions(is_skip, switch), Err(d)) => {
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
                                *ui_state = UIState::ShowOptions(false, switch);
                            }
                            // selecting a subdecision is not an actual state change
                            GameStateChange::None
                        }
                        HiveContext::SkipPlayer => {
                            if is_skip && input == Some(0) {
                                d.select_option(0);
                                GameStateChange::DecisionCompleted
                            } else {
                                *ui_state = UIState::ShowOptions(true, switch);
                                GameStateChange::None
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
                            GameStateChange::DecisionCompleted
                        } else {
                            // fill the annotation mapping
                            for (i, &(piece_type, _)) in pieces.into_iter().enumerate() {
                                piece_annotations.insert(piece_type, i);
                            }
                            GameStateChange::None
                        }
                    }
                    HiveContext::TargetField(_) => panic!("this should never happen"),
                    _ => unreachable!("this must be a follow-up decision"),
                },
                (UIState::PositionSelected(_), Err(_)) => {
                    *ui_state = UIState::ShowOptions(false, false);
                    GameStateChange::None
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
                            GameStateChange::DecisionCompleted
                        } else {
                            // fill the annotation mapping
                            for (i, &board_index) in board_indizes.into_iter().enumerate() {
                                board_annotations.insert(board_index, i);
                            }
                            GameStateChange::None
                        }
                    }
                    HiveContext::Piece(_) => panic!("this should never happen"),
                    _ => unreachable!("this must be a follow-up decision"),
                },
                (UIState::PieceSelected(_), Err(_)) => {
                    *ui_state = UIState::ShowOptions(false, false);
                    GameStateChange::None
                }
                (UIState::GameFinished(_), follow_up) => {
                    *ui_state = UIState::ShowOptions(false, false);
                    if let Ok(d) = follow_up {
                        d.retract_all();
                    }
                    GameStateChange::None
                }
                (UIState::PlaysAnimation(is_ai), Ok(d)) => {
                    assert!(is_ai, "selection during animation should be impossible");
                    d.retract_all();
                    GameStateChange::None
                }
                (UIState::PlaysAnimation(_), Err(_)) => GameStateChange::None,
                (UIState::ShowAIMoves, Ok(d)) => {
                    d.retract_all();
                    GameStateChange::None
                }
                (UIState::ShowAIMoves, Err(_)) => {
                    if let Some(index) = input {
                        let choice = ai_state
                            .actual_result()
                            .and_then(|result| result.all_ratings.get(index));
                        if let Some((_, path, _)) = choice {
                            apply_computed_move(engine, path, settings, animation_state);
                            return GameStateChange::DecisionCompleted;
                        }
                    }
                    GameStateChange::None
                }
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
                        let center = translate_index(index);
                        set_animation.set_animation(
                            build_blink_animation(settings, player, index, false, 0),
                            Some(center),
                        );
                    }
                }
            }
            if !ui_state.top_level() && !animation_state.runs() {
                *ui_state = UIState::GameFinished(result);
            }
            GameStateChange::None
        }
    }
}

fn apply_computed_move(
    engine: &mut Engine<HiveGameState, EventLog<HiveGameState>>,
    path: &[usize],
    settings: &Settings,
    animation_state: &mut AnimationState,
) {
    let GameState::PendingDecision(decision) = engine.pull() else {
        panic!("invalid engine state for this call");
    };
    let context = match decision.context() {
        HiveContext::BaseField(context) => context,
        HiveContext::SkipPlayer => {
            decision.select_option(0);
            return;
        }
        _ => panic!("invalid engine state for this call"),
    };
    let position = context[path[0]];
    decision.select_option(path[0]);

    if let GameState::PendingDecision(next) = engine.pull() {
        next.into_follow_up_decision().map(|d| match d.context() {
            HiveContext::TargetField(targets) => {
                handle_moved_piece(d, targets, path[1], position, settings, animation_state);
            }
            HiveContext::Piece(_) => {
                handle_placed_piece(d, path[1], position, settings, animation_state);
            }
            _ => unreachable!(""),
        });
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
        let center = translate_index(pos);
        animation.try_set().unwrap().set_animation(
            build_blink_animation(settings, player, pos, false, 0),
            Some(center),
        );
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
        let (x1, y1) = translate_index(pos);
        let (x2, y2) = translate_index(target);
        animation.try_set().unwrap().set_animation(
            build_complete_piece_move_animation(
                settings,
                piece_t,
                player,
                pos,
                target,
                target_level,
            ),
            Some(((0.2 * x1 + 0.8 * x2), (0.2 * y1 + 0.8 * y2))),
        );
    }
    dec.select_option(index);
}
