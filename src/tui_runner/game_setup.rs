use std::{collections::BTreeMap, iter};

use ratatui::{
    style::Color,
    text::{Line, Span, Text},
    widgets::{Block, Borders, Paragraph},
};

use crate::{
    pieces::{PieceType, Player},
    state::HiveGameState,
    tui_graphics,
};

use super::tui_settings::{ColorScheme, Settings};

#[derive(Debug, Clone)]
pub struct GameSetup {
    white_pieces: BTreeMap<PieceType, u32>,
    black_pieces: Option<BTreeMap<PieceType, u32>>,
}

impl Default for GameSetup {
    fn default() -> Self {
        Self {
            white_pieces: [
                (PieceType::Queen, 1),
                (PieceType::Ant, 3),
                (PieceType::Grasshopper, 3),
                (PieceType::Beetle, 2),
                (PieceType::Spider, 2),
                (PieceType::Ladybug, 0),
                (PieceType::Mosquito, 0),
            ]
            .into_iter()
            .collect(),
            black_pieces: None,
        }
    }
}

fn no_queen<'a, Iter, V: 'a>(it: Iter) -> impl Iterator<Item = (&'a PieceType, V)>
where
    Iter: Iterator<Item = (&'a PieceType, V)>,
{
    it.filter(|&(&p_type, _)| p_type != PieceType::Queen)
}

impl GameSetup {
    pub fn pieces_for_player(&self, player: Player) -> &BTreeMap<PieceType, u32> {
        match player {
            Player::White => &self.white_pieces,
            Player::Black => self.black_pieces.as_ref().unwrap_or(&self.white_pieces),
        }
    }

    pub fn is_symmetric(&self) -> bool {
        self.black_pieces.is_none()
    }

    pub fn num_rows(&self) -> usize {
        self.white_pieces.len() - 1
    }

    pub fn max_index(&self) -> usize {
        let mut val = 1 + self.num_rows();
        if !self.is_symmetric() {
            val += self.num_rows();
        }
        val
    }

    pub fn new_game_state(&self) -> HiveGameState {
        let white_pieces = self.white_pieces.clone();
        match self.black_pieces.as_ref() {
            Some(black_pieces) => HiveGameState::new_asym(white_pieces, black_pieces.clone()),
            None => HiveGameState::new(white_pieces),
        }
    }

    pub fn decrease_at(&mut self, selection: usize, offset: usize) {
        let selection = selection.saturating_sub(offset);
        if selection == 0 && !self.is_symmetric() {
            self.black_pieces = None;
        } else if selection > 0 {
            let mut selection = selection - 1;
            let pieces = if selection < self.num_rows() {
                &mut self.white_pieces
            } else {
                selection -= self.num_rows();
                self.black_pieces.as_mut().expect("selection index invalid")
            };
            let (_, count) = no_queen(pieces.iter_mut()).nth(selection).unwrap();
            *count = count.saturating_sub(1);
        }
    }

    pub fn switched_index(&self, selection: usize, offset: usize) -> usize {
        let index = selection.saturating_sub(offset);
        let new_index = if index > 0 && !self.is_symmetric() {
            if index < self.num_rows() + 1 {
                index + self.num_rows()
            } else {
                index - self.num_rows()
            }
        } else {
            index
        };
        new_index + offset
    }

    pub fn increase_at(&mut self, selection: usize, offset: usize) {
        let selection = selection.saturating_sub(offset);
        if selection == 0 && self.is_symmetric() {
            self.black_pieces = Some(self.white_pieces.clone());
        } else if selection > 0 {
            let mut selection = selection - 1;
            let pieces = if selection < self.num_rows() {
                &mut self.white_pieces
            } else {
                selection -= self.num_rows();
                self.black_pieces.as_mut().expect("selection index invalid")
            };
            let (_, count) = no_queen(pieces.iter_mut()).nth(selection).unwrap();
            *count += 1;
        }
    }

    pub fn render_game_setup(
        &self,
        settings: &Settings,
        selection: usize,
        offset: usize,
    ) -> Paragraph<'static> {
        self.black_pieces
            .as_ref()
            .inspect(|b| assert!(b.len() == self.white_pieces.len()));

        let any_selected = selection >= offset;
        let selection = selection.saturating_sub(offset);
        let mut lines = Vec::new();
        {
            let color = if any_selected && selection == 0 {
                settings.color_scheme.primary()
            } else {
                Color::White
            };
            let mut spans = vec![Span::styled(format!("[{}] ", offset + 1), color)];
            spans.push(Span::raw("Symmetric pieces: "));
            for sym in [true, false] {
                let selected = sym == self.is_symmetric();
                let str = if sym { "yes" } else { "no" };
                if selected {
                    spans.push(Span::styled(format!("<{str}>"), color));
                } else {
                    spans.push(Span::raw(format!(" {str} ")));
                }
            }
            lines.push(Line::from(spans));
            lines.push(Line::raw(""));
            if self.is_symmetric() {
                lines.push(Line::raw(""));
            } else {
                lines.push(Line::raw(" ".repeat(16) + "White [⇆] Black"));
            }
        }
        let pieces_iter = no_queen(self.white_pieces.iter())
            .enumerate()
            .map(|(i, (&p, &count))| (i, p, count, self.black_pieces.as_ref().map(|b| b[&p])));
        for (i, p_type, w_count, b_count) in pieces_iter {
            let is_selected = selection > 0 && (selection - 1) % self.num_rows() == i;
            let color = if is_selected {
                settings.color_scheme.primary()
            } else {
                Color::White
            };
            let p_color = tui_graphics::piece_color(p_type);
            let mut spans = vec![Span::styled(format!("[{}] ", i + offset + 2), color)];
            spans.push(Span::styled(format!("{:^12}", p_type.name()), p_color));
            for (is_white, count) in iter::once((true, w_count)).chain(b_count.map(|c| (false, c)))
            {
                let left_selected = selection < self.num_rows() + 1;
                let is_selected = is_selected && is_white == left_selected;
                if is_selected {
                    spans.push(Span::styled(format!(" <{count}> "), color));
                } else {
                    spans.push(Span::raw(format!("  {count}  ")));
                }
                if is_white {
                    spans.push(Span::raw("     "));
                }
            }
            lines.push(Line::from(spans));
        }
        lines.push(Line::raw(""));
        lines.push(Line::styled("[↲] start the game", ColorScheme::TEXT_GRAY));
        lines.push(Line::styled("[Esc] return", ColorScheme::TEXT_GRAY));
        let text = Text::from(lines);
        Paragraph::new(text).block(Block::default().title("Game Setup").borders(Borders::ALL))
    }

    pub fn into_key_val(self) -> impl Iterator<Item = (String, String)> {
        let it_white = self
            .white_pieces
            .into_iter()
            .map(|(p, count)| ("W".to_string() + p.letter(), count.to_string()));
        let it_black = self.black_pieces.map(|pieces| {
            pieces
                .into_iter()
                .map(|(p, count)| ("W".to_string() + p.letter(), count.to_string()))
        });
        let it_black = it_black.into_iter().flatten();
        it_white.chain(it_black)
    }

    pub fn from_key_val(input: impl Iterator<Item = (String, String)>) -> Result<Self, String> {
        let parse = |input: Vec<(String, String)>| {
            input
                .into_iter()
                .map(|(key, val)| {
                    let piece_t = PieceType::from_letter(&key[1..])
                        .ok_or_else(|| format!("Invalid piece type: {}", &key[1..]))?;
                    let count = val
                        .parse::<u32>()
                        .map_err(|e| format!("Invalid piece count: {e}"))?;
                    Ok((piece_t, count))
                })
                .collect::<Result<_, String>>()
        };

        let (white, black): (Vec<_>, Vec<_>) = input.partition(|(k, _)| k.starts_with('W'));
        let white_pieces: BTreeMap<PieceType, u32> = parse(white)?;
        let black_pieces: BTreeMap<PieceType, u32> = parse(black)?;
        let white_queen_valid = white_pieces.get(&PieceType::Queen) == Some(&1);
        let black_queen_valid =
            black_pieces.is_empty() || black_pieces.get(&PieceType::Queen) == Some(&1);
        if !white_queen_valid || !black_queen_valid {
            return Err("Invalid setup: exactly one queen required".to_string());
        }
        Ok(Self {
            white_pieces,
            black_pieces: (!black_pieces.is_empty()).then_some(black_pieces),
        })
    }
}
