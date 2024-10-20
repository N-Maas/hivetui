//! See https://github.com/ratatui-org/ratatui/blob/main/examples/user_input.rs

use ratatui::{
    layout::Rect,
    style::Color,
    widgets::{Block, Paragraph},
    Frame,
};

use super::tui_settings::ColorScheme;

pub struct TextInput {
    /// Current value of the input box
    input: Vec<char>,
    /// Position of cursor in the editor area
    character_index: usize,
    /// Shown if no input is entered yet
    default_text: String,
}

impl TextInput {
    pub fn new(default_text: String) -> Self {
        Self {
            input: Vec::new(),
            character_index: 0,
            default_text,
        }
    }

    pub fn move_cursor_left(&mut self) {
        self.character_index = self.character_index.saturating_sub(1);
    }

    pub fn move_cursor_right(&mut self) {
        if self.character_index < self.input.len() {
            self.character_index += 1;
        }
    }

    pub fn insert_char(&mut self, new_char: char) {
        self.input.insert(self.character_index, new_char);
        self.move_cursor_right();
    }

    pub fn delete_char_left(&mut self) {
        let is_not_cursor_leftmost = self.character_index != 0;
        if is_not_cursor_leftmost {
            self.input.remove(self.character_index - 1);
            self.move_cursor_left();
        }
    }

    pub fn delete_char_right(&mut self) {
        let is_not_cursor_rightmost = self.character_index < self.input.len();
        if is_not_cursor_rightmost {
            self.input.remove(self.character_index);
        }
    }

    pub fn get_text_or_default(&self) -> String {
        let text = self.input.iter().collect::<String>();
        if text.is_empty() {
            self.default_text.clone()
        } else {
            text
        }
    }

    pub fn render(&self, f: &mut Frame, area: Rect, border_color: Color) {
        let text = self.input.iter().collect::<String>();
        let input = if text.is_empty() {
            Paragraph::new(self.default_text.clone()).style(ColorScheme::TEXT_GRAY)
        } else {
            Paragraph::new(text)
        }
        .block(Block::bordered().border_style(border_color));
        f.render_widget(input, area);
        f.set_cursor_position((area.x + self.character_index as u16 + 1, area.y + 1));
    }
}
