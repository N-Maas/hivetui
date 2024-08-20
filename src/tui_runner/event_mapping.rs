use std::io;

use crossterm::event::{self, KeyCode, KeyEventKind};

use super::UIState;

pub fn pull_key_event() -> io::Result<Option<KeyCode>> {
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
) -> io::Result<Option<Event>> {
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
