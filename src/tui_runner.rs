use animation_and_ai_state::{AIState, AnimationState, CameraMove};
use chrono::Local;
use crossterm::{
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    ExecutableCommand,
};
use event_mapping::{pull_event, Event};
use ratatui::prelude::{CrosstermBackend, Terminal};
use setting_renderer::{SettingRenderer, SettingSelection};
use std::{
    collections::HashMap,
    ffi::OsString,
    fs::{self, File},
    io::{self, BufRead, BufReader},
};
use std::{io::stdout, panic, process};
use text_input::TextInput;
use tgp::{
    engine::{
        io::deserialize_initial_state, logging::EventLog, Engine, FollowUpDecision, GameState,
        LoggingEngine,
    },
    vec_context::VecContext,
};
use tgp_board::{open_board::OpenIndex, Board, BoardIndexable};
use tui_rendering::{board::find_losing_queen, TUTORIAL_HEIGHT};
use tui_settings::InputMode;

use crate::{
    io_manager::{load_game, IOManager, APP_NAME, AUTOSAVE},
    panic_handling::{get_panic_data, report_panic, setup_panic_reporting},
    pieces::{PieceType, Player},
    state::{HiveBoard, HiveContext, HiveGameState, HiveResult},
    tui_runner::tui_rendering::postprocess_ai_suggestions,
    worker::{
        ai_worker::{start_ai_worker_thread, AIResult},
        io_worker::{start_io_worker_thread, WriteTask},
        WorkerResult,
    },
    FatalError,
};

use self::{
    game_setup::GameSetup,
    tui_animations::{build_blink_animation, build_complete_piece_move_animation},
    tui_rendering::translate_index,
    tui_settings::{AutomaticCameraMoves, GraphicsState, Settings},
};

// mod dynamic_layout;
mod animation_and_ai_state;
mod event_mapping;
pub mod game_setup;
mod setting_renderer;
mod text_input;
mod tui_animations;
mod tui_rendering;
pub mod tui_settings;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum UIState {
    Toplevel,
    /// the current scrolling
    Tutorial(u16),
    /// the current scrolling, true if previous screen was toplevel
    RulesSummary(u16, bool),
    /// the currently selected option
    GameSetup(usize, bool),
    /// the currently selected save
    LoadScreen(usize, bool),
    /// true if previous screen was toplevel
    SaveScreen(bool),
    /// 1. true if the current turn must be skipped
    /// 2. true if pieces are switched
    ShowOptions(bool, bool),
    /// selected base field
    PositionSelected(OpenIndex),
    /// selected base field
    PieceSelected(OpenIndex),
    /// true if pieces are switched
    GameFinished(HiveResult, bool),
    /// true if pieces are switched
    PlaysAnimation(bool),
    /// true if pieces are switched
    ShowAIMoves(bool),
}

impl UIState {
    fn top_level(self) -> bool {
        use UIState::*;
        match self {
            Toplevel
            | Tutorial(_)
            | RulesSummary(_, _)
            | GameSetup(_, _)
            | LoadScreen(_, _)
            | SaveScreen(_) => true,
            ShowOptions(_, _) | PositionSelected(_) | PieceSelected(_) => false,
            GameFinished(_, _) | PlaysAnimation(_) | ShowAIMoves(_) => false,
        }
    }

    fn ai_may_move(self) -> bool {
        !self.top_level() && !matches!(self, UIState::ShowAIMoves(_))
    }

    fn mut_index<'a>(
        &'a mut self,
        selection: &'a mut SettingSelection,
    ) -> Option<(&'a mut usize, &'a mut bool)> {
        match self {
            UIState::Toplevel => Some(selection.pair_mut()).filter(|(index, _)| **index < 2),
            UIState::GameSetup(index, is_char) | UIState::LoadScreen(index, is_char) => {
                Some((index, is_char))
            }
            _ => None,
        }
    }

    fn get_menu_selection(self, selection: SettingSelection) -> Option<SettingSelection> {
        match self {
            UIState::GameSetup(index, is_char) | UIState::LoadScreen(index, is_char) => {
                (index < 2).then_some(SettingSelection::General(index, is_char))
            }
            _ => self.top_level().then_some(selection),
        }
    }

    fn state_changed(&mut self) {
        use UIState::*;
        match self {
            PositionSelected(_) => *self = ShowOptions(false, false),
            PieceSelected(_) => *self = ShowOptions(false, false),
            ShowAIMoves(_) => *self = ShowOptions(false, false),
            _ => (),
        };
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MessageType {
    Success,
    Info,
    Warning,
    Error,
}

struct Message {
    pub msg_type: MessageType,
    pub content: String,
}

impl Message {
    fn new<S: ToString>(msg_type: MessageType, content: S) -> Self {
        Self {
            msg_type,
            content: content.to_string(),
        }
    }
    fn success<S: ToString>(content: S) -> Self {
        Self::new(MessageType::Success, content)
    }
    fn info<S: ToString>(content: S) -> Self {
        Self::new(MessageType::Info, content)
    }
    fn warning<S: ToString>(content: S) -> Self {
        Self::new(MessageType::Warning, content)
    }
    fn error<S: ToString>(content: S) -> Self {
        Self::new(MessageType::Error, content)
    }
}

pub fn run_in_tui() -> io::Result<()> {
    stdout().execute(EnterAlternateScreen)?;
    enable_raw_mode()?;
    setup_panic_reporting();

    let result = panic::catch_unwind(run_in_tui_impl);

    stdout().execute(LeaveAlternateScreen)?;
    disable_raw_mode()?;

    match result {
        Ok(Err(FatalError::IOError(e))) => Err(e), // propagate I/O error
        Err(_) | Ok(Err(FatalError::PanicOccured)) => {
            let (msg, trace) = get_panic_data();
            eprintln!("Oh no! A panic (i.e. internal error) occured:\n{msg}\n{trace}");
            if let Some(io_manager) = IOManager::new() {
                let autosave = io_manager.autosave_path();
                let save_path = io_manager.save_file_path(&OsString::from(AUTOSAVE));
                if fs::rename(autosave, save_path).is_ok() {
                    eprintln!();
                    eprintln!("Note: disabled automatic game loading since it might be corrupted");
                    eprintln!("Note: instead available as saved game ({AUTOSAVE})",);
                }
            }
            process::exit(1);
        }
        Ok(_) => Ok(()),
    }
}

/// this implements the main event loop
fn run_in_tui_impl() -> Result<(), FatalError> {
    let mut terminal = Terminal::new(CrosstermBackend::new(stdout()))?;
    terminal.hide_cursor()?;
    terminal.clear()?;

    let ai_endpoint = start_ai_worker_thread();
    let io_endpoint = start_io_worker_thread();
    let setting_renderer = SettingRenderer::build();
    let mut io_manager = IOManager::new();

    // this is the ugly pile of mutable state, which is said to be the heart of any non-trivial UI...
    let mut settings = Settings::default_settings();
    let mut game_setup = GameSetup::default();
    let mut setup_of_current_game = GameSetup::default();
    let mut engine = Engine::new_logging(2, game_setup.new_game_state());
    let mut ui_state = UIState::Toplevel;
    let mut graphics_state = GraphicsState::new();
    let mut ai_state = AIState::new(ai_endpoint);
    let mut messages = Vec::new();
    let mut menu_selection = SettingSelection::General(2, false);
    let mut text_input = TextInput::new("".to_string());
    let mut digits: Option<Vec<usize>> = None;
    let mut animation_state = AnimationState::default();
    let mut camera_move: Option<CameraMove> = None;

    if let Some(io_manager) = io_manager.as_ref() {
        // TODO: CLI flag which avoids this?
        load_initial_state(
            &io_manager,
            &mut engine,
            &mut setup_of_current_game,
            &mut game_setup,
            &mut settings,
            &mut messages,
        );
    } else {
        messages.push(Message::warning(
            "Could not initialize game data directory - saving and loading games is disabled",
        ));
    }
    if settings.show_tutorial {
        ui_state = UIState::Tutorial(0);
    };

    // small helper functions for saving things
    let do_autosave = |io_manager: &Option<IOManager>,
                       setup: &GameSetup,
                       engine: &LoggingEngine<HiveGameState>| {
        if let Some(io_manager) = io_manager.as_ref() {
            io_endpoint.send(WriteTask::save_game(
                io_manager.autosave_path(),
                setup.clone(),
                engine.serialized_log(),
                false,
            ));
        }
    };
    let save_setup = |io_manager: &Option<IOManager>, setup: &GameSetup| {
        if let Some(io_manager) = io_manager.as_ref() {
            let path = io_manager.setup_path();
            io_endpoint.send(WriteTask::save_setup(path, setup.clone(), false));
        }
    };
    let save_settings = |io_manager: &Option<IOManager>, settings: Settings| {
        if let Some(io_manager) = io_manager.as_ref() {
            let path = io_manager.config_path();
            io_endpoint.send(WriteTask::save_settings(path, settings, false));
        }
    };

    let mut board_annotations = HashMap::new();
    let mut piece_annotations = HashMap::new();
    loop {
        let board = engine.data().board();
        let (boundaries_x, boundaries_y) = compute_view_boundaries(board);
        // first, pull for user input and directly apply any ui status changes or high-level commands (e.g. undo)
        let (mut event, removes_msg) = pull_event(
            ui_state,
            digits.is_some() || settings.input_mode == InputMode::Confirm,
            animation_state.runs(),
            !messages.is_empty(),
        )?;
        if removes_msg {
            messages.pop();
        } else if let Some(msg) = io_endpoint.get_msg() {
            match msg {
                WorkerResult::Msg(Ok(())) => {
                    messages.push(Message::success("Saving game completed."))
                }
                WorkerResult::Msg(Err(e)) => {
                    messages.push(Message::error(format!("Could not save game: {e}")))
                }
                WorkerResult::Killed(msg, trace) => {
                    report_panic(Some(msg), trace);
                    return Err(FatalError::PanicOccured);
                }
            }
        }
        if let Some(e) = event {
            // two digit handling happens first
            match e {
                Event::TwoDigitInit => {
                    digits = Some(Vec::new());
                }
                Event::TwoDigitAdd(digit) => {
                    if settings.input_mode == InputMode::Confirm && digits.is_none() {
                        digits = Some(Vec::new());
                    }
                    let digits_ref = digits.as_mut().unwrap();
                    digits_ref.push(digit);
                    if settings.input_mode == InputMode::Direct {
                        if let &[first, second] = digits_ref.as_slice() {
                            let val = 10 * first + second;
                            if val > 0 {
                                event = Some(Event::Selection(val - 1));
                            }
                            digits = None;
                        }
                    }
                }
                Event::SoftCancelConfirm => {
                    if let Some(digits) = digits.as_ref() {
                        if settings.input_mode == InputMode::Confirm {
                            let mut val = 0;
                            for &d in digits {
                                val *= 10;
                                val += d;
                            }
                            if val > 0 {
                                event = Some(Event::Selection(val - 1));
                            }
                        } else {
                            event = None;
                        }
                    }
                    digits = None;
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
                Event::CloseTutorial(show_again) => {
                    assert!(matches!(ui_state, UIState::Tutorial(_)));
                    settings.show_tutorial = show_again;
                    save_settings(&io_manager, settings);
                    ui_state = UIState::Toplevel;
                }
                Event::Exit => match ui_state {
                    UIState::Toplevel | UIState::Tutorial(_) => break,
                    UIState::GameSetup(_, _) => ui_state = UIState::Toplevel,
                    UIState::LoadScreen(_, _) => ui_state = UIState::Toplevel,
                    UIState::RulesSummary(_, top_level) | UIState::SaveScreen(top_level) => {
                        if top_level {
                            ui_state = UIState::Toplevel;
                        } else {
                            ui_state = UIState::ShowOptions(false, false);
                        }
                    }
                    UIState::ShowOptions(_, _) => ui_state = UIState::Toplevel,
                    UIState::PositionSelected(_) => ui_state = UIState::ShowOptions(false, false),
                    UIState::PieceSelected(_) => ui_state = UIState::ShowOptions(false, false),
                    UIState::GameFinished(_, _) => ui_state = UIState::Toplevel,
                    UIState::PlaysAnimation(_) => ui_state = UIState::Toplevel,
                    UIState::ShowAIMoves(_) => ui_state = UIState::ShowOptions(false, false),
                },
                Event::Switch => {
                    // TODO: use similar to escape to return from some menus?
                    if let UIState::GameSetup(index, is_char) = ui_state {
                        ui_state = UIState::GameSetup(game_setup.switched_index(index, 2), is_char);
                    } else if ui_state.top_level() {
                        menu_selection = menu_selection.switched();
                        let max_index = setting_renderer.max_index(menu_selection);
                        if menu_selection.index() >= max_index {
                            *menu_selection.index_mut() = max_index - 1;
                        }
                    } else if let UIState::ShowOptions(_, switch)
                    | UIState::GameFinished(_, switch)
                    | UIState::PlaysAnimation(switch)
                    | UIState::ShowAIMoves(switch) = &mut ui_state
                    {
                        *switch = !*switch;
                    }
                }
                Event::Cancel => {
                    animation_state.stop();
                    ai_state.dont_use_ai();
                }
                Event::SoftCancelConfirm => {
                    animation_state.stop();
                    if let UIState::ShowAIMoves(_) = ui_state {
                        ui_state = UIState::ShowOptions(false, false);
                    }
                }
                Event::LetAIMove => {
                    ai_state.use_ai(&settings);
                }
                Event::ContinueGame => {
                    ui_state = UIState::ShowOptions(false, false);
                }
                Event::NewGame => {
                    if let UIState::GameSetup(_, _) = ui_state {
                        ui_state = UIState::Toplevel;
                    } else {
                        ui_state = UIState::GameSetup(2, false);
                    }
                }
                Event::LoadGame => {
                    if let UIState::LoadScreen(_, _) = ui_state {
                        ui_state = UIState::Toplevel;
                    } else if let Some(io_manager) = io_manager.as_mut() {
                        match io_manager.recompute_save_files_list() {
                            Ok(_) => ui_state = UIState::LoadScreen(2, false),
                            Err(e) => messages
                                .push(Message::error(format!("Could not load save files: {e}"))),
                        }
                    }
                }
                Event::SaveGame => {
                    if let Some(io_manager) = io_manager.as_mut() {
                        match io_manager.recompute_save_files_list() {
                            Ok(_) => {
                                ui_state = UIState::SaveScreen(ui_state.top_level());
                                // suggest hivetui <timestamp> as default name
                                let name =
                                    format!("{APP_NAME} {}", Local::now().format("%Y-%m-%d %H:%M"));
                                text_input = TextInput::new(name);
                            }
                            Err(e) => messages
                                .push(Message::error(format!("Could not load save files: {e}"))),
                        }
                    }
                }
                Event::StartGame => {
                    engine = Engine::new_logging(2, game_setup.new_game_state());
                    setup_of_current_game = game_setup.clone();
                    start_game(&setup_of_current_game, &engine, &mut ui_state);
                }
                Event::RestoreDefault => {
                    game_setup = GameSetup::default();
                    settings = Settings::default_settings();
                    save_settings(&io_manager, settings);
                    save_setup(&io_manager, &game_setup);
                    messages.push(Message::success("Default settings restored."));
                }
                Event::Undo => {
                    if engine.undo_last_decision() {
                        animation_state.reset();
                        ai_state.reset();
                        ai_state.set_pause();
                        ui_state.state_changed();
                        do_autosave(&io_manager, &setup_of_current_game, &engine);
                    }
                }
                Event::Redo => {
                    if engine.redo_decision() {
                        animation_state.reset();
                        ai_state.reset();
                        ai_state.set_pause();
                        ui_state.state_changed();
                        do_autosave(&io_manager, &setup_of_current_game, &engine);
                    }
                }
                Event::Rules => match ui_state {
                    UIState::Toplevel | UIState::LoadScreen(_, _) => {
                        ui_state = UIState::RulesSummary(0, true)
                    }
                    UIState::ShowOptions(_, _)
                    | UIState::PositionSelected(_)
                    | UIState::PieceSelected(_)
                    | UIState::ShowAIMoves(_)
                    | UIState::PlaysAnimation(_)
                    | UIState::GameFinished(_, _) => ui_state = UIState::RulesSummary(0, false),
                    UIState::RulesSummary(_, _)
                    | UIState::Tutorial(_)
                    | UIState::GameSetup(_, _)
                    | UIState::SaveScreen(_) => (),
                },
                Event::Help => match ui_state {
                    UIState::ShowOptions(false, _)
                    | UIState::PositionSelected(_)
                    | UIState::PieceSelected(_)
                    | UIState::PlaysAnimation(_) => ui_state = UIState::ShowAIMoves(false),
                    UIState::ShowAIMoves(_) => ui_state = UIState::ShowOptions(false, false),
                    _ => (),
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
                    if let UIState::GameSetup(_, _) = ui_state {
                        if new_index < game_setup.max_index() + 2 {
                            ui_state = UIState::GameSetup(new_index, false);
                        }
                    } else if let UIState::LoadScreen(_, _) = ui_state {
                        let max_len = io_manager.as_ref().unwrap().save_files_list().len();
                        if new_index < max_len + 2 {
                            ui_state = UIState::LoadScreen(new_index, false);
                        }
                    } else if new_index < setting_renderer.max_index(menu_selection) {
                        *menu_selection.index_mut() = new_index;
                    }
                }
                Event::MenuUp => {
                    if let Some((index, is_char)) = ui_state.mut_index(&mut menu_selection) {
                        if *is_char {
                            *is_char = false;
                        } else {
                            *index = index.saturating_sub(1);
                        }
                    } else if menu_selection.index() > 0 {
                        *menu_selection.index_mut() -= 1;
                    }
                }
                Event::MenuDown => {
                    if let Some((_, is_char)) = ui_state
                        .mut_index(&mut menu_selection)
                        .filter(|(index, is_char)| **index < 2 && !**is_char)
                        .filter(|(index, _)| {
                            setting_renderer.show_second_row(&mut settings, **index)
                        })
                    {
                        *is_char = true;
                    } else if let UIState::GameSetup(index, is_char) = &mut ui_state {
                        if *index + 1 < game_setup.max_index() + 2 {
                            *index += 1;
                            *is_char = false;
                        }
                    } else if let UIState::LoadScreen(index, is_char) = &mut ui_state {
                        let max_len = io_manager.as_ref().unwrap().save_files_list().len();
                        if *index + 1 < max_len + 2 {
                            *index += 1;
                            *is_char = false;
                        }
                    } else if menu_selection.index() + 1
                        < setting_renderer.max_index(menu_selection)
                    {
                        let (index, is_char) = menu_selection.pair_mut();
                        *index += 1;
                        *is_char = false;
                    }
                }
                Event::MenuIncrease | Event::MenuDecrease => match ui_state {
                    UIState::GameSetup(index, _) if index >= 2 => {
                        if e == Event::MenuIncrease {
                            game_setup.increase_at(index, 2);
                        } else {
                            game_setup.decrease_at(index, 2);
                        }
                        save_setup(&io_manager, &game_setup);
                    }
                    _ => {
                        if let Some(selection) = ui_state.get_menu_selection(menu_selection) {
                            if e == Event::MenuIncrease {
                                setting_renderer.increase(selection, &mut settings);
                            } else {
                                setting_renderer.decrease(selection, &mut settings);
                            }
                            if setting_renderer.is_ai_setting(selection) {
                                ai_state.reset();
                            }
                            save_settings(&io_manager, settings);
                        }
                    }
                },
                Event::ScrollUp => {
                    if let UIState::Tutorial(i) | UIState::RulesSummary(i, _) = &mut ui_state {
                        *i = i.saturating_sub(1);
                    }
                }
                Event::ScrollDown => {
                    if let UIState::Tutorial(i) = &mut ui_state {
                        *i = (*i + 1).clamp(0, TUTORIAL_HEIGHT - 6)
                    } else if let UIState::RulesSummary(i, _) = &mut ui_state {
                        *i += 1;
                    }
                }
                Event::SelectMenuOption => {
                    let io_manager = io_manager.as_ref().unwrap();
                    if let UIState::LoadScreen(index, _) = ui_state {
                        // load the selected game
                        if index >= 2 {
                            let result = io_manager.load_from_index(index - 2);
                            if let Some(result) = result {
                                match result {
                                    Ok(result) => {
                                        (engine, setup_of_current_game) = result;
                                        start_game(&setup_of_current_game, &engine, &mut ui_state);
                                    }
                                    Err(e) => messages
                                        .push(Message::error(format!("Could not load game: {e}"))),
                                }
                            }
                        }
                    } else if let UIState::SaveScreen(toplevel) = ui_state {
                        // save the current game
                        let name = text_input.get_text_or_default();
                        let path = io_manager.save_file_path(&OsString::from(name));
                        io_endpoint.send(WriteTask::save_game(
                            path,
                            game_setup.clone(),
                            engine.serialized_log(),
                            true,
                        ));
                        if toplevel {
                            ui_state = UIState::Toplevel;
                        } else {
                            ui_state = UIState::ShowOptions(false, false);
                        }
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
            if matches!(ui_state, UIState::ShowOptions(true, _)) && e == Event::SoftCancelConfirm {
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
        )?;
        while let GameState::PendingEffect(pe) = engine.pull() {
            ai_state.reset();
            animation_state.reset_count();
            pe.next_effect();
            ui_state.state_changed();
        }
        if state_change {
            do_autosave(&io_manager, &setup_of_current_game, &engine);
        }

        // update ui state if an animation was started
        if animation_state
            .animation()
            .is_some_and(|a| !a.is_finished())
            && !ui_state.top_level()
            && !matches!(ui_state, UIState::GameFinished(_, _))
        {
            if !matches!(ui_state, UIState::PlaysAnimation(_)) && !ai_state.animation_has_started()
            {
                ui_state = UIState::PlaysAnimation(true);
            }
        } else if matches!(ui_state, UIState::PlaysAnimation(_))
            && !ai_state.animation_has_started()
        {
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
            &messages,
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
        if let Some((x_to, y_to)) = animation_state.center() {
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

fn load_initial_state(
    io_manager: &IOManager,
    engine: &mut Engine<HiveGameState, EventLog<HiveGameState>>,
    setup_of_current_game: &mut GameSetup,
    game_setup: &mut GameSetup,
    settings: &mut Settings,
    messages: &mut Vec<Message>,
) {
    // attempt to load saved state
    let game_path = io_manager.autosave_path();
    let mut restored_state = false;
    if game_path.is_file() {
        match load_game(&game_path) {
            Ok(result) => {
                (*engine, *setup_of_current_game) = result;
                restored_state = true;
            }
            Err(e) => messages.push(Message::warning(format!("Could not load saved state: {e}"))),
        }
    }
    let conf_path = io_manager.config_path();
    let mut restored_settngs = false;
    if conf_path.is_file() {
        let result = File::open(conf_path)
            .map_err(|e| e.to_string())
            .and_then(|f| {
                let reader = BufReader::new(f);
                Settings::from_json(reader).map_err(|e| e.to_string())
            });
        match result {
            Ok(new_settings) => {
                *settings = new_settings;
                restored_settngs = true;
            }
            Err(e) => messages.push(Message::warning(format!(
                "Could not load saved settings: {e}"
            ))),
        }
    }
    let setup_path = io_manager.setup_path();
    if setup_path.is_file() {
        let result = File::open(setup_path)
            .map_err(|e| e.to_string())
            .and_then(|f| {
                let mut buf = String::new();
                BufReader::new(f)
                    .read_line(&mut buf)
                    .map_err(|e| e.to_string())?;
                let key_val = deserialize_initial_state(&buf)?;
                GameSetup::from_key_val(key_val.into_iter())
            });
        match result {
            Ok(new_setup) => {
                *game_setup = new_setup;
            }
            Err(e) => messages.push(Message::warning(format!("Could not load game setup: {e}"))),
        }
    }
    match (restored_state, restored_settngs) {
        (true, true) => messages.push(Message::info("Restored game state and settings.")),
        (true, false) => messages.push(Message::info("Restored game state.")),
        (false, true) => messages.push(Message::info("Restored settings.")),
        (false, false) => (),
    }
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
) -> Result<bool, FatalError> {
    let result = match engine.pull() {
        GameState::PendingEffect(_) => false,
        GameState::PendingDecision(decision) => {
            ai_state.update(
                decision.data(),
                settings,
                ui_state.ai_may_move(),
                Player::from(decision.player()),
                animation_state,
                postprocess_ai_suggestions,
            )?;
            let follow_up = decision.try_into_follow_up_decision();
            if ai_state.should_use_ai() && ui_state.ai_may_move() {
                let decision = match follow_up {
                    Ok(d) => {
                        d.retract_all();
                        return Ok(false);
                    }
                    Err(d) => d,
                };

                if let Some(result) = ai_state.result() {
                    animation_state.stop();
                    assert!(result.player == Player::from(decision.player()));
                    let best = &result.best_move;
                    apply_computed_move(engine, best, settings, animation_state);
                    if *ui_state == UIState::PlaysAnimation(false) {
                        *ui_state = UIState::PlaysAnimation(true);
                    }
                    return Ok(true);
                } else if ai_state.animation_has_started()
                    && !ui_state.top_level()
                    && !matches!(ui_state, UIState::PlaysAnimation(_))
                {
                    *ui_state = UIState::PlaysAnimation(false);
                }
                return Ok(false); // don't risk a weird decision state
            } else if ai_state.should_use_ai() {
                // the UI is in a state where the AI move should not be applied,
                // thus we ensure there is also a pause when returning
                ai_state.set_pause();
            }

            use UIState::*;
            match (*ui_state, follow_up) {
                (
                    Toplevel
                    | Tutorial(_)
                    | RulesSummary(_, _)
                    | GameSetup(_, _)
                    | LoadScreen(_, _)
                    | SaveScreen(_),
                    Ok(d),
                ) => {
                    d.retract_all();
                    false
                }
                (
                    Toplevel
                    | Tutorial(_)
                    | RulesSummary(_, _)
                    | GameSetup(_, _)
                    | LoadScreen(_, _)
                    | SaveScreen(_),
                    Err(_),
                ) => false,
                (ShowOptions(_, _), Ok(d)) => {
                    d.retract_all();
                    false
                }
                (ShowOptions(is_skip, switch), Err(d)) => {
                    match d.context() {
                        HiveContext::BaseField(board_indizes) => {
                            if let Some(index) = input.filter(|&index| index < d.option_count()) {
                                // We need to update the UI state according to the selected option.
                                // This means, we need to find out whether we selected a piece or an empty
                                // field
                                let b_index = board_indizes[index];
                                let board = d.data().board();
                                if board[b_index].is_empty() {
                                    *ui_state = PositionSelected(b_index);
                                } else {
                                    *ui_state = PieceSelected(b_index);
                                }
                                d.select_option(index);
                            } else {
                                // fill the annotation mapping
                                for (i, &board_index) in board_indizes.into_iter().enumerate() {
                                    board_annotations.insert(board_index, i);
                                }
                                *ui_state = ShowOptions(false, switch);
                            }
                            // selecting a subdecision is not an actual state change
                            false
                        }
                        HiveContext::SkipPlayer => {
                            if is_skip && input == Some(0) {
                                d.select_option(0);
                                true
                            } else {
                                *ui_state = ShowOptions(true, switch);
                                false
                            }
                        }
                        _ => unreachable!("this can not be a follow-up decision"),
                    }
                }
                (PositionSelected(b_index), Ok(d)) => match d.context() {
                    HiveContext::Piece(pieces) => {
                        if let Some(index) = input.filter(|&index| index < d.option_count()) {
                            animation_state.stop();
                            handle_placed_piece(d, index, b_index, settings, animation_state);
                            true
                        } else {
                            // fill the annotation mapping
                            for (i, &(piece_type, _)) in pieces.into_iter().enumerate() {
                                piece_annotations.insert(piece_type, i);
                            }
                            false
                        }
                    }
                    HiveContext::TargetField(_) => panic!("this should never happen"),
                    _ => unreachable!("this must be a follow-up decision"),
                },
                (PositionSelected(_), Err(_)) => {
                    *ui_state = ShowOptions(false, false);
                    false
                }
                (PieceSelected(b_index), Ok(d)) => match d.context() {
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
                            true
                        } else {
                            // fill the annotation mapping
                            for (i, &board_index) in board_indizes.into_iter().enumerate() {
                                board_annotations.insert(board_index, i);
                            }
                            false
                        }
                    }
                    HiveContext::Piece(_) => panic!("this should never happen"),
                    _ => unreachable!("this must be a follow-up decision"),
                },
                (PieceSelected(_), Err(_)) => {
                    *ui_state = ShowOptions(false, false);
                    false
                }
                (GameFinished(_, _), follow_up) => {
                    *ui_state = ShowOptions(false, false);
                    if let Ok(d) = follow_up {
                        d.retract_all();
                    }
                    false
                }
                (PlaysAnimation(_), Ok(d)) => {
                    d.retract_all();
                    false
                }
                (PlaysAnimation(_), Err(_)) => false,
                (ShowAIMoves(_), Ok(d)) => {
                    d.retract_all();
                    false
                }
                (ShowAIMoves(_), Err(_)) => {
                    if let Some(index) = input {
                        let choice = ai_state
                            .actual_result()
                            .and_then(|result| result.all_ratings.get(index));
                        if let Some((_, path, _)) = choice {
                            apply_computed_move(engine, path, settings, animation_state);
                            return Ok(true);
                        }
                    }
                    false
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
            if !ui_state.top_level()
                && !animation_state.runs()
                && !matches!(ui_state, UIState::GameFinished(_, _))
            {
                *ui_state = UIState::GameFinished(result, false);
            }
            false
        }
    };
    Ok(result)
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
        if let Some(d) = next.into_follow_up_decision() {
            match d.context() {
                HiveContext::TargetField(targets) => {
                    handle_moved_piece(d, targets, path[1], position, settings, animation_state);
                }
                HiveContext::Piece(_) => {
                    handle_placed_piece(d, path[1], position, settings, animation_state);
                }
                _ => unreachable!(""),
            }
        };
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
