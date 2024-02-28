use crossterm::{
    event::{self, KeyCode, KeyEventKind},
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    ExecutableCommand,
};
use ratatui::prelude::{CrosstermBackend, Terminal};
use std::io::stdout;
use std::{
    collections::BTreeMap,
    sync::{Arc, Mutex},
};
use std::{collections::HashMap, io};
use tgp::engine::{logging::EventLog, Engine, GameEngine, GameState};
use tgp_board::{open_board::OpenIndex, Board, BoardIndexable};

use crate::{
    pieces::{PieceType, Player},
    state::{HiveBoard, HiveContext, HiveGameState, HiveResult},
    tui_runner::tui_settings::AIMoves,
};

use self::{
    tui_animations::{build_blink_animation, build_complete_piece_move_animation, Animation},
    tui_settings::{is_ai_setting, AILevel, GraphicsState, PlayerType, Settings},
};

mod tui_animations;
mod tui_rendering;
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
    /// true if the animation is waiting for ai
    PlaysAnimation(bool),
}

impl UIState {
    fn show_game(&self) -> bool {
        match self {
            UIState::Toplevel => false,
            UIState::ShowOptions => true,
            UIState::PositionSelected(_) => true,
            UIState::PieceSelected(_) => true,
            UIState::GameFinished(_) => true,
            UIState::PlaysAnimation(_) => true,
        }
    }
}

#[derive(Debug)]
struct AIResult {
    player: Player,
}

#[derive(Debug)]
enum AIMessage {
    Start(AILevel, Box<HiveGameState>),
    Cancel,
    Result(Box<AIResult>),
}

struct AIState {
    exchange_point: Arc<Mutex<Option<AIMessage>>>,
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
            exchange_point: Arc::new(Mutex::new(None)),
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
            let mut lock = self.exchange_point.lock().unwrap();
            let new_msg = match lock.take() {
                Some(AIMessage::Cancel) => Some(AIMessage::Cancel),
                Some(AIMessage::Start(_, _)) => None,
                Some(AIMessage::Result(_)) => {
                    assert!(self.is_started);
                    None
                }
                None => {
                    if self.is_started {
                        Some(AIMessage::Cancel)
                    } else {
                        None
                    }
                }
            };
            *lock = new_msg;
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
        animation: &mut Option<Animation>,
    ) {
        assert!(self.current_player == player || !self.is_started);
        if !self.is_started {
            assert!(self.result.is_none() && self.animation_progress == 0);
            let level = if settings.is_ai(player) {
                settings.player_type(player).into_ai_level()
            } else {
                settings.ai_assistant
            };
            *self.exchange_point.lock().unwrap() =
                Some(AIMessage::Start(level, Box::new(state.clone())));
            self.current_player = player;
            self.should_use_ai = settings.is_ai(player) && settings.ai_moves == AIMoves::AUTOMATIC;
            self.should_show_animation = settings.is_ai(player);
            self.is_started = true;
        }

        {
            let mut lock = self.exchange_point.lock().unwrap();
            lock.take().map(|msg| match msg {
                AIMessage::Start(_, _) => *lock = Some(msg),
                AIMessage::Cancel => unreachable!(),
                AIMessage::Result(r) => self.result = Some(*r),
            });
        }
        if self.should_use_ai && (self.animation_progress > 0 || animation.is_none()) {
            self.animation_progress += 1;
        }
        if self.should_show_animation && animation.is_none() {
            // TODO: animaton = ...
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
fn pull_event(top_level: bool, two_digit: bool, animation: bool) -> io::Result<Option<Event>> {
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
            } else if !animation && !two_digit {
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

/// this implements the main event loop
pub fn run_in_tui(pieces: BTreeMap<PieceType, u32>) -> io::Result<()> {
    stdout().execute(EnterAlternateScreen)?;
    enable_raw_mode()?;
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
    let mut current_animation: Option<Animation> = None;
    let mut digits: Option<Vec<usize>> = None;
    loop {
        let board = engine.data().board();
        let (boundaries_x, boundaries_y) = compute_view_boundaries(board);
        if current_animation.as_ref().is_some_and(|a| !a.is_finished()) && ui_state.show_game() {
            // TODO: interaction with AI?!
            ui_state = UIState::PlaysAnimation(false);
        } else if matches!(ui_state, UIState::PlaysAnimation(_)) {
            ui_state = UIState::ShowOptions;
        }
        // first, pull for user input and directly apply any ui status changes or high-level commands (e.g. undo)
        let mut event = pull_event(
            ui_state == UIState::Toplevel,
            digits.is_some(),
            current_animation.is_some(),
        )?;
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
                    UIState::PlaysAnimation(_) => ui_state = UIState::Toplevel,
                },
                Event::Cancel => {
                    current_animation = None;
                    ai_state.dont_use_ai();
                }
                Event::SoftCancel => {
                    current_animation = None;
                }
                Event::LetAIMove => {
                    ai_state.use_ai(&settings);
                }
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
        update_game_state_and_fill_input_mapping(
            &mut engine,
            &mut board_annotations,
            &mut piece_annotations,
            &mut ui_state,
            &mut ai_state,
            &settings,
            &mut current_animation,
            event.and_then(|e| e.as_selection()),
        );

        // finally we render the UI
        let state = tui_rendering::AllState {
            game_state: engine.data(),
            settings: settings,
            board_annotations: &board_annotations,
            piece_annotations: &piece_annotations,
            ui_state,
            animation: current_animation.as_ref(),
            menu_index,
            graphics_state,
        };
        tui_rendering::render(&mut terminal, &settings_list, state, &mut settings, &pieces)?;
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
    animation: &mut Option<Animation>,
    input: Option<usize>,
) {
    match engine.pull() {
        GameState::PendingEffect(pe) => {
            // TODO: start animations here?!
            ai_state.reset();
            pe.next_effect()
        }
        GameState::PendingDecision(decision) => {
            ai_state.update(
                decision.data(),
                settings,
                Player::from(decision.player()),
                animation,
            );
            if ai_state.should_use_ai && ui_state.show_game() {
                if let Some(_result) = ai_state.result() {
                    todo!();
                } else {
                    *ui_state = UIState::PlaysAnimation(true);
                }
            }

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
                            if settings.should_play_animation(player) {
                                *animation =
                                    Some(build_blink_animation(settings, player, *b_index, false));
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
                            if settings.should_play_animation(player) {
                                let board = d.data().board();
                                let piece_t =
                                    board.get(*b_index).and_then(|c| c.top()).unwrap().p_type;
                                let target = board_indizes[index];
                                *animation = Some(build_complete_piece_move_animation(
                                    settings, piece_t, player, *b_index, target,
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
                (UIState::PlaysAnimation(is_ai), Ok(_)) => {
                    assert!(is_ai, "selection during animation should be impossible")
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
            if ui_state.show_game() {
                *ui_state = UIState::GameFinished(result);
            }
        }
    }
}
