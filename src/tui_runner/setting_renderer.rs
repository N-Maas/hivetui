use super::tui_settings::{ColorScheme, Settings};
use ratatui::{
    text::{Line, Span, Text},
    widgets::{Block, Borders, Paragraph},
};
use std::fmt::Debug;

const PLAYER_PREFIXES: [&str; 2] = ["white player: ", "black player: "];
const PLAYER_TYPES: [&str; 6] = ["human", "beginner", "easy", "normal", "hard", "master"];
const AI_TYPES: [&str; 4] = ["balanced", "aggressive", "defensive", "strategic"];

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SettingSelection {
    General(usize, bool),
    Graphics(usize, bool),
}

impl SettingSelection {
    pub fn unified(&self) -> SelectionUnified {
        match *self {
            SettingSelection::General(i, is_char) => {
                if i < 2 {
                    SelectionUnified::Player(i, is_char)
                } else {
                    SelectionUnified::General(i)
                }
            }
            SettingSelection::Graphics(i, is_char) => {
                if i < 2 {
                    SelectionUnified::Player(i, is_char)
                } else {
                    SelectionUnified::Graphics(i)
                }
            }
        }
    }

    pub fn player_index(&self) -> Option<(usize, bool)> {
        match self.unified() {
            SelectionUnified::Player(i, is_char) => Some((i, is_char)),
            _ => None,
        }
    }

    pub fn index(&self) -> usize {
        match *self {
            SettingSelection::General(i, _) => i,
            SettingSelection::Graphics(i, _) => i,
        }
    }

    pub fn index_mut(&mut self) -> &mut usize {
        self.pair_mut().0
    }

    pub fn pair_mut(&mut self) -> (&mut usize, &mut bool) {
        match self {
            SettingSelection::General(i, is_char) => (i, is_char),
            SettingSelection::Graphics(i, is_char) => (i, is_char),
        }
    }

    pub fn switched(self) -> SettingSelection {
        match self {
            SettingSelection::General(i, is_char) => SettingSelection::Graphics(i, is_char),
            SettingSelection::Graphics(i, is_char) => SettingSelection::General(i, is_char),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SelectionUnified {
    Player(usize, bool),
    General(usize),
    Graphics(usize),
}

trait MenuSetting {
    fn val(&self, state: &mut Settings) -> u8;

    fn increase(&self, state: &mut Settings);

    fn decrease(&self, state: &mut Settings);

    fn get_line(&self, state: &mut Settings, highlight: bool) -> Line<'static>;

    fn get_spans(
        &self,
        state: &mut Settings,
        out: &mut Vec<Span<'static>>,
        highlight: bool,
        at: usize,
    );
}

struct MenuSettingImpl<E, F: Fn(&mut Settings) -> &mut E> {
    prefix: String,
    texts: Vec<&'static str>,
    get_setting_fn: F,
}

impl<E: Copy + TryFrom<u8> + Into<u8>, F: Fn(&mut Settings) -> &mut E> MenuSetting
    for MenuSettingImpl<E, F>
where
    <E as TryFrom<u8>>::Error: Debug,
{
    fn val(&self, state: &mut Settings) -> u8 {
        (*(self.get_setting_fn)(state)).into()
    }

    fn increase(&self, state: &mut Settings) {
        let current_val = self.val(state);
        if usize::from(current_val + 1) < self.texts.len() {
            *(self.get_setting_fn)(state) = E::try_from(current_val + 1).unwrap();
        }
    }

    fn decrease(&self, state: &mut Settings) {
        let current_val = self.val(state);
        if current_val > 0 {
            *(self.get_setting_fn)(state) = E::try_from(current_val - 1).unwrap();
        }
    }

    fn get_line(&self, state: &mut Settings, highlight: bool) -> Line<'static> {
        let mut spans = vec![Span::raw(self.prefix.clone())];
        for i in 0..self.texts.len() {
            self.get_spans(state, &mut spans, highlight, i);
        }
        Line::from(spans)
    }

    fn get_spans(
        &self,
        state: &mut Settings,
        spans: &mut Vec<Span<'static>>,
        highlight: bool,
        at: usize,
    ) {
        let current_val = self.val(state);
        let str = self.texts[at];
        if usize::from(current_val) == at && highlight {
            spans.push(Span::styled(
                format!("<{str}>"),
                state.color_scheme.primary(),
            ));
        } else if usize::from(current_val) == at {
            spans.push(Span::styled("<", ColorScheme::TEXT_GRAY));
            spans.push(Span::raw(str));
            spans.push(Span::styled(">", ColorScheme::TEXT_GRAY));
        } else {
            spans.push(Span::raw(format!(" {str} ")));
        }
    }
}

fn create_menu_setting<
    'a,
    E: Copy + TryFrom<u8> + Into<u8> + 'a,
    F: Fn(&mut Settings) -> &mut E + 'a,
>(
    prefix: &'static str,
    texts: Vec<&'static str>,
    offset: usize,
    get_setting_fn: F,
) -> Box<dyn MenuSetting + 'a>
where
    <E as TryFrom<u8>>::Error: Debug,
{
    Box::new(MenuSettingImpl {
        prefix: format!("{prefix:<width$}", width = offset),
        texts,
        get_setting_fn,
    })
}

pub struct SettingRenderer {
    players: [[Box<dyn MenuSetting>; 2]; 2],
    general: Vec<Box<dyn MenuSetting>>,
    graphic: Vec<Box<dyn MenuSetting>>,
}

impl SettingRenderer {
    fn get(&self, selection: SettingSelection) -> &dyn MenuSetting {
        match selection.unified() {
            SelectionUnified::Player(index, is_char) => {
                let [player_type, ai_type] = &self.players[index];
                if is_char {
                    ai_type.as_ref()
                } else {
                    player_type.as_ref()
                }
            }
            SelectionUnified::General(index) => self.general[index - 2].as_ref(),
            SelectionUnified::Graphics(index) => self.graphic[index - 2].as_ref(),
        }
    }

    pub fn increase(&self, selection: SettingSelection, settings: &mut Settings) {
        self.get(selection).increase(settings);
    }

    pub fn decrease(&self, selection: SettingSelection, settings: &mut Settings) {
        self.get(selection).decrease(settings);
    }

    pub fn max_index(&self, selection: SettingSelection) -> usize {
        match selection {
            SettingSelection::General(_, _) => self.general.len() + 2,
            SettingSelection::Graphics(_, _) => self.graphic.len() + 2,
        }
    }

    pub fn is_ai_setting(&self, selection: SettingSelection) -> bool {
        if let SettingSelection::General(index, _) = selection {
            [0, 1, 3, 4, 5].contains(&index)
        } else {
            selection.index() < 2
        }
    }

    pub fn build() -> Self {
        let general_offset = 24;
        let graphics_offset = 17;
        Self {
            players: [
                [
                    create_menu_setting(PLAYER_PREFIXES[0], PLAYER_TYPES.into(), 0, |state| {
                        &mut state.white_player_type
                    }),
                    create_menu_setting("", AI_TYPES.into(), 0, |state| {
                        &mut state.white_ai_character
                    }),
                ],
                [
                    create_menu_setting(PLAYER_PREFIXES[1], PLAYER_TYPES.into(), 0, |state| {
                        &mut state.black_player_type
                    }),
                    create_menu_setting("", AI_TYPES.into(), 0, |state| {
                        &mut state.black_ai_character
                    }),
                ],
            ],
            general: vec![
                create_menu_setting(
                    "automatic camera moves",
                    vec!["on", "off"],
                    general_offset,
                    |state| &mut state.automatic_camera_moves,
                ),
                create_menu_setting(
                    "automatic AI moves",
                    vec!["on", "off"],
                    general_offset,
                    |state| &mut state.ai_moves,
                ),
                create_menu_setting(
                    "AI assistant level",
                    vec!["1", "2", "3", "4", "5"],
                    general_offset,
                    |state| &mut state.ai_assistant,
                ),
                create_menu_setting(
                    "filter suggested moves",
                    vec!["yes", "no"],
                    general_offset,
                    |state| &mut state.filter_ai_suggestions,
                ),
                create_menu_setting(
                    "available pieces size",
                    vec!["1", "2", "3", "4", "5"],
                    general_offset,
                    |state| &mut state.piece_zoom_level,
                ),
                create_menu_setting(
                    "screen splitting",
                    vec!["auto", "1", "2", "3", "4", "5"],
                    general_offset,
                    |state| &mut state.splitting,
                ),
            ],
            graphic: vec![
                create_menu_setting(
                    "color scheme",
                    vec!["red", "blue", "green", "purple", "gold"],
                    graphics_offset,
                    |state| &mut state.color_scheme,
                ),
                create_menu_setting(
                    "animation speed",
                    vec!["1", "2", "3", "4", "5", "6", "off"],
                    graphics_offset,
                    |state| &mut state.animation_speed,
                ),
                create_menu_setting(
                    "animation style",
                    vec!["blink", "only-ai", "plain", "rainbow"],
                    graphics_offset,
                    |state| &mut state.animation_style,
                ),
                create_menu_setting(
                    "border style",
                    vec!["complete", "partial", "none"],
                    graphics_offset,
                    |state| &mut state.borders_style,
                ),
                create_menu_setting(
                    "movement style",
                    vec!["filled", "transparent", "minimal"],
                    graphics_offset,
                    |state| &mut state.moving_tile_style,
                ),
                create_menu_setting(
                    "filling style",
                    vec!["full", "border", "hybrid"],
                    graphics_offset,
                    |state| &mut state.filling_style,
                ),
                create_menu_setting(
                    "dark tile color",
                    vec!["black", "gray", "dark-blue"],
                    graphics_offset,
                    |state| &mut state.dark_tile_color,
                ),
            ],
        }
    }

    pub fn show_second_row(&self, settings: &mut Settings, index: usize) -> bool {
        assert!(index < 2);
        self.players[index][0].val(settings) != 0
    }

    pub fn render_player_settings(
        &self,
        settings: &mut Settings,
        selection: Option<(usize, bool)>,
    ) -> Text<'static> {
        let mut lines = Vec::new();
        for (i, [p_type, ai_type]) in self.players.iter().enumerate() {
            let (is_selected, is_char) =
                selection.map_or((false, false), |(index, is_char)| (index == i, is_char));
            let position = p_type.val(settings);
            let color = if is_selected {
                settings.color_scheme.primary()
            } else {
                ColorScheme::TEXT_GRAY
            };
            let mut spans = vec![Span::styled(format!("[{}] ", i + 1), color)];
            spans.push(Span::raw(PLAYER_PREFIXES[i]));
            spans.push(Span::raw(PLAYER_TYPES[position as usize]));
            if position > 0 {
                let ai_type = AI_TYPES[ai_type.val(settings) as usize];
                spans.push(Span::raw(format!(" AI ({ai_type})")));
            }
            lines.push(Line::from(spans));

            spans = Vec::new();
            if is_selected {
                spans.push(Span::raw("   "));
                p_type.get_spans(settings, &mut spans, !is_char, 0);
                spans.push(Span::raw(" or AI: "));
                for level in 1..PLAYER_TYPES.len() {
                    p_type.get_spans(settings, &mut spans, !is_char, level);
                }
            }
            lines.push(Line::from(spans));

            spans = Vec::new();
            if is_selected && position != 0 {
                spans.push(Span::raw("   "));
                for j in 0..AI_TYPES.len() {
                    ai_type.get_spans(settings, &mut spans, is_char, j);
                }
            }
            lines.push(Line::from(spans));
        }
        Text::from(lines)
    }

    pub fn render_current_settings(
        &self,
        settings: &mut Settings,
        selection: SettingSelection,
    ) -> Paragraph<'static> {
        match selection {
            SettingSelection::General(_, _) => self.render_general_settings(settings, selection),
            SettingSelection::Graphics(_, _) => self.render_graphic_settings(settings, selection),
        }
    }

    pub fn render_general_settings(
        &self,
        settings: &mut Settings,
        selection: SettingSelection,
    ) -> Paragraph<'static> {
        let index = match selection {
            SettingSelection::General(index, _) => index,
            SettingSelection::Graphics(_, _) => 0,
        };
        let mut text = self.render_settings(settings, &self.general, index);
        text.lines.push(Line::raw(""));
        text.lines.push(Line::styled(
            "[⇆] switch to graphic settings",
            ColorScheme::TEXT_GRAY,
        ));
        Paragraph::new(text).block(
            Block::default()
                .title("General Settings")
                .borders(Borders::ALL),
        )
    }

    pub fn render_graphic_settings(
        &self,
        settings: &mut Settings,
        selection: SettingSelection,
    ) -> Paragraph<'static> {
        let index = match selection {
            SettingSelection::General(_, _) => 0,
            SettingSelection::Graphics(index, _) => index,
        };
        let mut text = self.render_settings(settings, &self.graphic, index);
        text.lines.push(Line::raw(""));
        text.lines.push(Line::styled(
            "[⇆] switch to general settings",
            ColorScheme::TEXT_GRAY,
        ));
        Paragraph::new(text).block(
            Block::default()
                .title("Graphic Settings")
                .borders(Borders::ALL),
        )
    }

    fn render_settings(
        &self,
        settings: &mut Settings,
        list: &[Box<dyn MenuSetting>],
        index: usize,
    ) -> Text<'static> {
        let mut lines = Vec::new();
        for (i, option) in list.iter().enumerate() {
            let i = i + 2;
            let color = if index == i {
                settings.color_scheme.primary()
            } else {
                ColorScheme::TEXT_GRAY
            };
            let mut spans = vec![Span::styled(format!("[{}] ", i + 1), color)];
            spans.extend(option.get_line(settings, index == i));
            lines.push(Line::from(spans));
        }
        Text::from(lines)
    }
}
