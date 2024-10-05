use std::io;

use crossterm::event::{self, KeyCode, KeyEventKind};

use super::UIState;

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
pub enum Event {
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
    RestoreDefault,
    Undo,
    Redo,
    LetAIMove,
    Help,
    Rules,
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
    pub fn as_selection(&self) -> Option<usize> {
        if let Event::Selection(index) = *self {
            Some(index)
        } else {
            None
        }
    }
}

/// pull event in internal represenation: handles mapping of raw key event
pub fn pull_event(
    ui_state: UIState,
    two_digit: bool,
    animation: bool,
    has_message: bool,
) -> io::Result<(Option<Event>, bool)> {
    let top_level = ui_state.top_level();
    let show_suggestions = matches!(ui_state, UIState::ShowAIMoves(_));
    let rules_summary = matches!(ui_state, UIState::RulesSummary(_, _));
    let game_setup = matches!(ui_state, UIState::GameSetup(_, _));
    let save_game = matches!(ui_state, UIState::SaveScreen(_));
    let is_skip = matches!(ui_state, UIState::ShowOptions(true, _));
    let msg_visible = !rules_summary && !game_setup && !save_game;
    let key = pull_key_event()?;
    let event = key.and_then(|mut key| {
        if save_game {
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
        if let KeyCode::Char(c) = &mut key {
            *c = c.to_ascii_lowercase();
        }
        match key {
            KeyCode::Esc => Some(Event::Exit)
                .filter(|_| ui_state != UIState::Toplevel && !(has_message && msg_visible)),
            KeyCode::Char('q') => Some(Event::Exit),
            KeyCode::Char('u') => Some(Event::Undo).filter(|_| !top_level),
            KeyCode::Char('r') => {
                if top_level {
                    Some(Event::RestoreDefault).filter(|_| !rules_summary && !game_setup)
                } else {
                    Some(Event::Redo)
                }
            }
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
            .filter(|_| !rules_summary),
            KeyCode::Char('h') => Some(Event::Help),
            KeyCode::Char('j') => Some(Event::Rules),
            KeyCode::Char('k') => Some(Event::SaveGame).filter(|_| !rules_summary && !game_setup),
            KeyCode::Char('l') => Some(Event::LoadGame).filter(|_| !rules_summary && !game_setup),
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
            KeyCode::Enter => {
                if ui_state == UIState::Toplevel {
                    Some(Event::ContinueGame)
                } else if matches!(ui_state, UIState::GameSetup(_, _)) {
                    Some(Event::StartGame)
                } else if matches!(ui_state, UIState::LoadScreen(_, _) | UIState::SaveScreen(_)) {
                    Some(Event::SelectMenuOption)
                } else if matches!(ui_state, UIState::RulesSummary(_, _)) {
                    Some(Event::Exit)
                } else if top_level {
                    Some(Event::ScrollDown)
                } else if !animation && !two_digit && !is_skip && !show_suggestions {
                    Some(Event::TwoDigitInit)
                } else {
                    Some(Event::SoftCancel)
                }
            }
            KeyCode::Char(' ') => {
                if rules_summary {
                    Some(Event::ScrollDown)
                } else {
                    (!animation && !two_digit && !is_skip && !show_suggestions)
                        .then_some(Event::TwoDigitInit)
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
    });
    let removes_msg = msg_visible
        && key.map_or(false, |key| match key {
            KeyCode::Esc | KeyCode::Char('q') => true,
            KeyCode::Enter => event.is_some() || !top_level,
            KeyCode::Char('c') => top_level && event.is_some(),
            _ => false,
        });
    Ok((event, removes_msg))
}
