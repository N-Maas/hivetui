use crate::ai::Difficulty;
use crate::state::HiveGameState;
use num_enum::{IntoPrimitive, TryFromPrimitive};
use ratatui::{
    style::Color,
    text::{Line, Span, Text},
    widgets::{Block, Borders, Paragraph},
};
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, fmt::Debug, io::Read, iter};

use crate::{
    pieces::{PieceType, Player},
    tui_graphics,
};

#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    Copy,
    Default,
    TryFromPrimitive,
    IntoPrimitive,
    Serialize,
    Deserialize,
)]
#[repr(u8)]
pub enum ZoomLevel {
    Bird = 0,
    Strategical = 1,
    #[default]
    Wider = 2,
    Normal = 3,
    Close = 4,
}

impl ZoomLevel {
    pub fn multiplier(&self) -> f64 {
        match self {
            ZoomLevel::Bird => 2.0,
            ZoomLevel::Strategical => 1.3,
            ZoomLevel::Wider => 1.0,
            ZoomLevel::Normal => 0.7,
            ZoomLevel::Close => 0.5,
        }
    }

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

    fn move_offset(&self) -> f64 {
        self.multiplier() * 12.0
    }
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    Copy,
    Default,
    TryFromPrimitive,
    IntoPrimitive,
    Serialize,
    Deserialize,
)]
#[repr(u8)]
pub enum PlayerType {
    #[default]
    Human = 0,
    AI1 = 1,
    AI2 = 2,
    AI3 = 3,
    AI4 = 4,
    AI5 = 5,
}

impl PlayerType {
    pub fn into_ai_level(self) -> AILevel {
        assert!(self != PlayerType::Human);
        AILevel::try_from(u8::from(self) - 1).unwrap()
    }
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    Copy,
    Default,
    TryFromPrimitive,
    IntoPrimitive,
    Serialize,
    Deserialize,
)]
#[repr(u8)]
pub enum AILevel {
    Beginner = 0,
    Easy = 1,
    #[default]
    Normal = 2,
    Hard = 3,
    Master = 4,
}

impl AILevel {
    pub fn as_difficulty(self) -> Difficulty {
        match self {
            AILevel::Beginner => Difficulty::Easy,
            AILevel::Easy => Difficulty::QuiteEasy,
            AILevel::Normal => Difficulty::Medium,
            AILevel::Hard => Difficulty::Hard,
            AILevel::Master => Difficulty::VeryHard,
        }
    }
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    Copy,
    Default,
    TryFromPrimitive,
    IntoPrimitive,
    Serialize,
    Deserialize,
)]
#[repr(u8)]
pub enum AutomaticCameraMoves {
    #[default]
    On = 0,
    Off = 1,
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    Copy,
    Default,
    TryFromPrimitive,
    IntoPrimitive,
    Serialize,
    Deserialize,
)]
#[repr(u8)]
pub enum AIMoves {
    #[default]
    Automatic = 0,
    Manual = 1,
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    Copy,
    Default,
    TryFromPrimitive,
    IntoPrimitive,
    Serialize,
    Deserialize,
)]
#[repr(u8)]
pub enum FilterAISuggestions {
    #[default]
    Yes = 0,
    No = 1,
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    Copy,
    Default,
    TryFromPrimitive,
    IntoPrimitive,
    Serialize,
    Deserialize,
)]
#[repr(u8)]
pub enum WhiteTilesStyle {
    Full = 0,
    Border = 1,
    #[default]
    Hybrid = 2,
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    Copy,
    Default,
    TryFromPrimitive,
    IntoPrimitive,
    Serialize,
    Deserialize,
)]
#[repr(u8)]
pub enum BordersStyle {
    Complete = 0,
    #[default]
    Partial = 1,
    None = 2,
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    Copy,
    Default,
    TryFromPrimitive,
    IntoPrimitive,
    Serialize,
    Deserialize,
)]
#[repr(u8)]
pub enum ScreenSplitting {
    #[default]
    Auto = 0,
    FarLeft = 1,
    Left = 2,
    Normal = 3,
    Right = 4,
    FarRight = 5,
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    Copy,
    Default,
    TryFromPrimitive,
    IntoPrimitive,
    Serialize,
    Deserialize,
)]
#[repr(u8)]
pub enum AnimationSpeed {
    Slower = 0,
    Slow = 1,
    #[default]
    Normal = 2,
    Fast = 3,
    Faster = 4,
    BlazinglyFast = 5,
    Off = 6,
}

impl AnimationSpeed {
    pub fn map_steps(&self, steps: usize) -> usize {
        self.map_steps_normal(f64::from(u32::try_from(steps).unwrap()))
            .round() as usize
    }

    pub fn map_steps_normal(&self, steps: f64) -> f64 {
        let factor = match self {
            AnimationSpeed::Slower => 2.0,
            AnimationSpeed::Slow => 1.4,
            AnimationSpeed::Normal => 1.0,
            AnimationSpeed::Fast => 0.7,
            AnimationSpeed::Faster => 0.5,
            AnimationSpeed::BlazinglyFast => 0.25,
            AnimationSpeed::Off => panic!("should not play animation"),
        };
        factor * steps
    }

    pub fn map_steps_extreme(&self, steps: f64) -> f64 {
        let factor = match self {
            AnimationSpeed::Slower => 3.0,
            AnimationSpeed::Slow => 1.8,
            AnimationSpeed::Normal => 1.0,
            AnimationSpeed::Fast => 0.35,
            AnimationSpeed::Faster => 0.25,
            AnimationSpeed::BlazinglyFast => 0.125,
            AnimationSpeed::Off => panic!("should not play animation"),
        };
        factor * steps
    }
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    Copy,
    Default,
    TryFromPrimitive,
    IntoPrimitive,
    Serialize,
    Deserialize,
)]
#[repr(u8)]
pub enum MovingTileStyle {
    #[default]
    Filled = 0,
    Tranparent = 1,
    Minimal = 2,
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    Copy,
    Default,
    TryFromPrimitive,
    IntoPrimitive,
    Serialize,
    Deserialize,
)]
#[repr(u8)]
pub enum AnimationStyle {
    #[default]
    Blink = 0,
    BlinkOnlyAi = 1,
    Plain = 2,
    Rainbow = 3,
}

#[derive(
    Debug,
    PartialEq,
    Eq,
    Clone,
    Copy,
    Default,
    TryFromPrimitive,
    IntoPrimitive,
    Serialize,
    Deserialize,
)]
#[repr(u8)]
pub enum ColorScheme {
    #[default]
    Red = 0,
    Blue = 1,
    Green = 2,
    Purple = 3,
}

impl ColorScheme {
    pub const RED: Color = Color::from_u32(0x00E03030);
    pub const ORANGE: Color = Color::from_u32(0x00D89030);
    pub const BLUE: Color = Color::from_u32(0x00308AEA);
    pub const TURQUOISE: Color = Color::from_u32(0x0030D0D0);
    pub const GREEN: Color = Color::from_u32(0x0010D050);
    pub const YELLOW_GREEN: Color = Color::from_u32(0x00A0D030);
    pub const PURPLE: Color = Color::from_u32(0x009030DA);
    pub const PINK: Color = Color::from_u32(0x00D830B0);
    pub const TEXT_GRAY: Color = Color::from_u32(0x00A0A0A0);

    pub fn primary(&self) -> Color {
        match self {
            ColorScheme::Red => Self::RED,
            ColorScheme::Blue => Self::BLUE,
            ColorScheme::Green => Self::GREEN,
            ColorScheme::Purple => Self::PURPLE,
        }
    }

    pub fn secondary(&self) -> Color {
        match self {
            ColorScheme::Red => Self::ORANGE,
            ColorScheme::Blue => Self::TURQUOISE,
            ColorScheme::Green => Self::YELLOW_GREEN,
            ColorScheme::Purple => Self::PINK,
        }
    }

    pub fn extreme_values(&self) -> (u8, u8, u8) {
        match self {
            ColorScheme::Red => (0xFF, 0x30, 0x30),
            ColorScheme::Blue => (0x30, 0x90, 0xEA),
            ColorScheme::Green => (0x30, 0xFF, 0x30),
            ColorScheme::Purple => (0x90, 0x30, 0xEA),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct GraphicsState {
    pub center_x: f64,
    pub center_y: f64,
    pub zoom_level: ZoomLevel,
}

impl GraphicsState {
    pub fn new() -> Self {
        GraphicsState {
            center_x: 0.0,
            center_y: 0.0,
            zoom_level: ZoomLevel::Normal,
        }
    }

    pub fn zoom_in(&mut self) {
        self.zoom_level = self.zoom_level.zoom_in();
    }

    pub fn zoom_out(&mut self) {
        self.zoom_level = self.zoom_level.zoom_out();
    }

    pub fn move_in_step_size(
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

#[derive(Debug, Default, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub struct Settings {
    pub white_player_type: PlayerType,
    pub black_player_type: PlayerType,
    #[serde(default)]
    pub automatic_camera_moves: AutomaticCameraMoves,
    #[serde(default)]
    pub ai_moves: AIMoves,
    #[serde(default)]
    pub ai_assistant: AILevel,
    #[serde(default)]
    pub filter_ai_suggestions: FilterAISuggestions,
    #[serde(default)]
    pub piece_zoom_level: ZoomLevel,
    #[serde(default)]
    pub white_tiles_style: WhiteTilesStyle,
    #[serde(default)]
    pub borders_style: BordersStyle,
    #[serde(default)]
    pub splitting: ScreenSplitting,
    #[serde(default)]
    pub animation_speed: AnimationSpeed,
    #[serde(default)]
    pub animation_style: AnimationStyle,
    #[serde(default)]
    pub moving_tile_style: MovingTileStyle,
    #[serde(default)]
    pub color_scheme: ColorScheme,
}

impl Settings {
    pub fn default_settings() -> Self {
        Self {
            black_player_type: PlayerType::AI2,
            ..Default::default()
        }
    }

    pub fn player_type(&self, player: Player) -> PlayerType {
        match player {
            Player::White => self.white_player_type,
            Player::Black => self.black_player_type,
        }
    }

    pub fn is_ai(&self, player: Player) -> bool {
        self.player_type(player) != PlayerType::Human
    }

    pub fn get_animation_speed(&self, _player: Player) -> AnimationSpeed {
        self.animation_speed
    }

    pub fn should_play_animation(&self, _player: Player) -> bool {
        self.animation_speed != AnimationSpeed::Off
    }

    pub fn from_json<R: Read>(input: R) -> Result<Self, serde_json::Error> {
        serde_json::from_reader(input)
    }

    pub fn to_json(self) -> String {
        // only would fail if the (derived) serialize impl fails
        serde_json::to_string_pretty(&self).unwrap()
    }
}

pub trait MenuSetting {
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

pub fn create_menu_setting<
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

struct MenuSettingImpl<E, F: Fn(&mut Settings) -> &mut E> {
    prefix: String,
    texts: Vec<&'static str>,
    get_setting_fn: F,
}

impl<E: Copy + Into<u8>, F: Fn(&mut Settings) -> &mut E> MenuSettingImpl<E, F> {
    fn val(&self, state: &mut Settings) -> u8 {
        (*(self.get_setting_fn)(state)).into()
    }
}

impl<E: Copy + TryFrom<u8> + Into<u8>, F: Fn(&mut Settings) -> &mut E> MenuSetting
    for MenuSettingImpl<E, F>
where
    <E as TryFrom<u8>>::Error: Debug,
{
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

const PLAYER_PREFIXES: [&str; 2] = ["white player: ", "black player: "];
const PLAYER_TYPES: [&str; 6] = ["human", "beginner", "easy", "normal", "hard", "master"];

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SettingSelection {
    General(usize),
    Graphics(usize),
}

impl SettingSelection {
    pub fn player_index(&self) -> Option<usize> {
        Some(self.index()).filter(|&i| i <= 2)
    }

    pub fn index(&self) -> usize {
        match *self {
            SettingSelection::General(i) => i,
            SettingSelection::Graphics(i) => i,
        }
    }

    pub fn index_mut(&mut self) -> &mut usize {
        match self {
            SettingSelection::General(i) => i,
            SettingSelection::Graphics(i) => i,
        }
    }

    pub fn switched(self) -> SettingSelection {
        match self {
            SettingSelection::General(i) => SettingSelection::Graphics(i),
            SettingSelection::Graphics(i) => SettingSelection::General(i),
        }
    }
}

pub struct SettingRenderer {
    players: [Box<dyn MenuSetting>; 2],
    general: Vec<Box<dyn MenuSetting>>,
    graphic: Vec<Box<dyn MenuSetting>>,
}

impl SettingRenderer {
    pub fn get(&self, selection: SettingSelection) -> &dyn MenuSetting {
        match selection {
            SettingSelection::General(index) => {
                if index < 2 {
                    self.players[index].as_ref()
                } else {
                    self.general[index - 2].as_ref()
                }
            }
            SettingSelection::Graphics(index) => {
                if index < 2 {
                    self.players[index].as_ref()
                } else {
                    self.graphic[index - 2].as_ref()
                }
            }
        }
    }

    pub fn max_index(&self, selection: SettingSelection) -> usize {
        match selection {
            SettingSelection::General(_) => self.general.len() + 2,
            SettingSelection::Graphics(_) => self.graphic.len() + 2,
        }
    }

    pub fn is_ai_setting(&self, selection: SettingSelection) -> bool {
        if let SettingSelection::General(index) = selection {
            [0, 1, 3, 4, 5].contains(&index)
        } else {
            selection.index() <= 1
        }
    }

    pub fn build() -> Self {
        let general_offset = 24;
        let graphics_offset = 17;
        Self {
            players: [
                create_menu_setting(PLAYER_PREFIXES[0], PLAYER_TYPES.into(), 0, |state| {
                    &mut state.white_player_type
                }),
                create_menu_setting(PLAYER_PREFIXES[1], PLAYER_TYPES.into(), 0, |state| {
                    &mut state.black_player_type
                }),
            ],
            general: vec![
                create_menu_setting(
                    "automatic camera moves: ",
                    vec!["on", "off"],
                    general_offset,
                    |state| &mut state.automatic_camera_moves,
                ),
                create_menu_setting(
                    "automatic AI moves: ",
                    vec!["on", "off"],
                    general_offset,
                    |state| &mut state.ai_moves,
                ),
                create_menu_setting(
                    "AI assistant level: ",
                    vec!["1", "2", "3", "4", "5"],
                    general_offset,
                    |state| &mut state.ai_assistant,
                ),
                create_menu_setting(
                    "filter suggested moves: ",
                    vec!["yes", "no"],
                    general_offset,
                    |state| &mut state.filter_ai_suggestions,
                ),
                create_menu_setting(
                    "available pieces size: ",
                    vec!["1", "2", "3", "4", "5"],
                    general_offset,
                    |state| &mut state.piece_zoom_level,
                ),
                create_menu_setting(
                    "screen splitting: ",
                    vec!["auto", "1", "2", "3", "4", "5"],
                    general_offset,
                    |state| &mut state.splitting,
                ),
            ],
            graphic: vec![
                create_menu_setting(
                    "color scheme: ",
                    vec!["red", "blue", "green", "purple"],
                    graphics_offset,
                    |state| &mut state.color_scheme,
                ),
                create_menu_setting(
                    "animation speed: ",
                    vec!["1", "2", "3", "4", "5", "6", "off"],
                    graphics_offset,
                    |state| &mut state.animation_speed,
                ),
                create_menu_setting(
                    "animation style: ",
                    vec!["blink", "only-ai", "plain", "rainbow"],
                    graphics_offset,
                    |state| &mut state.animation_style,
                ),
                create_menu_setting(
                    "border style: ",
                    vec!["complete", "partial", "none"],
                    graphics_offset,
                    |state| &mut state.borders_style,
                ),
                create_menu_setting(
                    "movement style: ",
                    vec!["filled", "transparent", "minimal"],
                    graphics_offset,
                    |state| &mut state.moving_tile_style,
                ),
                create_menu_setting(
                    "filling style: ",
                    vec!["full", "border", "hybrid"],
                    graphics_offset,
                    |state| &mut state.white_tiles_style,
                ),
            ],
        }
    }

    pub fn render_player_settings(
        &self,
        settings: &mut Settings,
        selection: Option<usize>,
    ) -> Text<'static> {
        let mut lines = Vec::new();
        for (i, option) in self.players.iter().enumerate() {
            let is_selected = selection == Some(i);
            let color = if is_selected {
                settings.color_scheme.primary()
            } else {
                Color::White
            };
            let mut spans = vec![Span::styled(format!("[{}] ", i + 1), color)];
            spans.push(Span::raw(PLAYER_PREFIXES[i]));
            let position: u8 = if i == 0 {
                settings.white_player_type.into()
            } else {
                settings.black_player_type.into()
            };
            spans.push(Span::raw(PLAYER_TYPES[position as usize]));
            if position > 0 {
                spans.push(Span::raw(" AI"));
            }
            lines.push(Line::from(spans));

            spans = Vec::new();
            if is_selected {
                spans.push(Span::raw("   "));
                option.get_spans(settings, &mut spans, true, 0);
                spans.push(Span::raw(" or AI: "));
                for level in 1..PLAYER_TYPES.len() {
                    option.get_spans(settings, &mut spans, true, level);
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
            SettingSelection::General(_) => self.render_general_settings(settings, selection),
            SettingSelection::Graphics(_) => self.render_graphic_settings(settings, selection),
        }
    }

    pub fn render_general_settings(
        &self,
        settings: &mut Settings,
        selection: SettingSelection,
    ) -> Paragraph<'static> {
        let index = match selection {
            SettingSelection::General(index) => index,
            SettingSelection::Graphics(_) => 0,
        };
        let mut text = self.render_settings(settings, &self.general, index);
        text.lines.push(Line::raw(""));
        text.lines.push(Line::styled(
            "[⇆] to switch to graphic settings",
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
            SettingSelection::General(_) => 0,
            SettingSelection::Graphics(index) => index,
        };
        let mut text = self.render_settings(settings, &self.graphic, index);
        text.lines.push(Line::raw(""));
        text.lines.push(Line::styled(
            "[⇆] to switch to general settings",
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
                Color::White
            };
            let mut spans = vec![Span::styled(format!("[{}] ", i + 1), color)];
            spans.extend(option.get_line(settings, index == i));
            lines.push(Line::from(spans));
        }
        Text::from(lines)
    }
}

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
        lines.push(Line::styled(
            "[↲] to start the game",
            ColorScheme::TEXT_GRAY,
        ));
        lines.push(Line::styled(
            "[Esc] or [q] to return",
            ColorScheme::TEXT_GRAY,
        ));
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
