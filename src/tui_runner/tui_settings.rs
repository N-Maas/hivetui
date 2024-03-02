use num_enum::{IntoPrimitive, TryFromPrimitive};
use ratatui::{
    style::Color,
    text::{Line, Span, Text},
};
use std::fmt::Debug;

use crate::pieces::Player;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, TryFromPrimitive, IntoPrimitive)]
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum PlayerType {
    #[default]
    Human = 0,
    AI1 = 1,
    AI2 = 2,
    AI3 = 3,
    AI4 = 4,
}

impl PlayerType {
    pub fn into_ai_level(self) -> AILevel {
        assert!(self != PlayerType::Human);
        AILevel::try_from(u8::from(self) - 1).unwrap()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum AILevel {
    BEGINNER = 0,
    EASY = 1,
    #[default]
    NORMAL = 2,
    HARD = 3,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum AIMoves {
    #[default]
    AUTOMATIC = 0,
    MANUAL = 1,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum WhiteTilesStyle {
    Full = 0,
    Border = 1,
    #[default]
    Hybrid = 2,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum BordersStyle {
    Complete = 0,
    #[default]
    Partial = 1,
    None = 2,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum ScreenSplitting {
    FarLeft = 0,
    Left = 1,
    #[default]
    Normal = 2,
    Right = 3,
    FarRight = 4,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, TryFromPrimitive, IntoPrimitive)]
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum MovingTileStyle {
    #[default]
    Filled = 0,
    Tranparent = 1,
    Minimal = 2,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum AnimationStyle {
    #[default]
    Blink = 0,
    Plain = 1,
    BlinkOnlyAi = 2,
    Rainbow = 3,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, TryFromPrimitive, IntoPrimitive)]
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

#[derive(Debug, Default, PartialEq, Clone, Copy)]
pub struct Settings {
    pub white_player_type: PlayerType,
    pub black_player_type: PlayerType,
    pub ai_moves: AIMoves,
    pub ai_assistant: AILevel,
    pub piece_zoom_level: ZoomLevel,
    pub white_tiles_style: WhiteTilesStyle,
    pub borders_style: BordersStyle,
    pub splitting: ScreenSplitting,
    pub animation_speed: AnimationSpeed,
    pub animation_style: AnimationStyle,
    pub moving_tile_style: MovingTileStyle,
    pub color_scheme: ColorScheme,
}

impl Settings {
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
}

pub trait MenuSetting {
    fn increase(&self, state: &mut Settings);

    fn decrease(&self, state: &mut Settings);

    fn get_line(&self, state: &mut Settings, highlight: bool) -> Line<'static>;

    fn get_entry(&self, state: &mut Settings, highlight: bool, at: usize) -> Span<'static>;
}

pub fn create_menu_setting<
    'a,
    E: Copy + TryFrom<u8> + Into<u8> + 'a,
    F: Fn(&mut Settings) -> &mut E + 'a,
>(
    prefix: &'static str,
    texts: Vec<&'static str>,
    get_setting_fn: F,
) -> Box<dyn MenuSetting + 'a>
where
    <E as TryFrom<u8>>::Error: Debug,
{
    Box::new(MenuSettingImpl {
        prefix,
        texts,
        get_setting_fn,
    })
}

struct MenuSettingImpl<E, F: Fn(&mut Settings) -> &mut E> {
    prefix: &'static str,
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
        let mut spans = vec![Span::raw(self.prefix)];
        spans.extend((0..self.texts.len()).map(|i| self.get_entry(state, highlight, i)));
        Line::from(spans)
    }

    fn get_entry(&self, state: &mut Settings, highlight: bool, at: usize) -> Span<'static> {
        let current_val = self.val(state);
        let str = self.texts[at];
        if usize::from(current_val) == at && highlight {
            Span::styled(format!("<{str}>"), state.color_scheme.primary())
        } else if usize::from(current_val) == at {
            Span::raw(format!("<{str}>"))
        } else {
            Span::raw(format!(" {str} "))
        }
    }
}

const PLAYER_PREFIXES: [&'static str; 2] = ["white player: ", "black player: "];
const PLAYER_TYPES: [&'static str; 5] = ["human", "beginner", "easy", "normal", "hard"];

pub fn is_ai_setting(menu_index: usize) -> bool {
    [0, 1, 2, 3].contains(&menu_index)
}

pub fn build_settings() -> Vec<Box<dyn MenuSetting>> {
    vec![
        create_menu_setting(PLAYER_PREFIXES[0], PLAYER_TYPES.into(), |state| {
            &mut state.white_player_type
        }),
        create_menu_setting(PLAYER_PREFIXES[1], PLAYER_TYPES.into(), |state| {
            &mut state.black_player_type
        }),
        create_menu_setting("automatic ai moves: ", vec!["on", "off"], |state| {
            &mut state.ai_moves
        }),
        create_menu_setting("ai assistant level: ", vec!["1", "2", "3", "4"], |state| {
            &mut state.ai_assistant
        }),
        create_menu_setting(
            "screen splitting: ",
            vec!["1", "2", "3", "4", "5"],
            |state| &mut state.splitting,
        ),
        create_menu_setting(
            "available pieces display size: ",
            vec!["1", "2", "3", "4", "5"],
            |state| &mut state.piece_zoom_level,
        ),
        create_menu_setting(
            "white tiles filling style: ",
            vec!["full", "border", "hybrid"],
            |state| &mut state.white_tiles_style,
        ),
        create_menu_setting(
            "border drawing style: ",
            vec!["complete", "partial", "none"],
            |state| &mut state.borders_style,
        ),
        create_menu_setting(
            "animation speed: ",
            vec!["1", "2", "3", "4", "5", "6", "off"],
            |state| &mut state.animation_speed,
        ),
        create_menu_setting(
            "animation style: ",
            vec!["blink", "plain", "blink-only-ai", "rainbow"],
            |state| &mut state.animation_style,
        ),
        create_menu_setting(
            "moving tile style: ",
            vec!["filled", "transparent", "minimal"],
            |state| &mut state.moving_tile_style,
        ),
        create_menu_setting(
            "color scheme: ",
            vec!["red", "blue", "green", "purple"],
            |state| &mut state.color_scheme,
        ),
    ]
}

pub fn render_settings(
    settings: &mut Settings,
    settings_list: &[Box<dyn MenuSetting>],
    menu_index: usize,
) -> Text<'static> {
    let mut lines = Vec::<Line>::new();
    for (i, option) in settings_list.iter().enumerate() {
        let color = if menu_index == i {
            settings.color_scheme.primary()
        } else {
            Color::White
        };
        let mut spans = vec![Span::styled(format!("[{}] ", i + 1), color)];
        if i <= 1 {
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
            if menu_index == i {
                spans.push(Span::raw("    "));
                spans.push(option.get_entry(settings, menu_index == i, 0));
                spans.push(Span::raw(" or AI: "));
                for level in 1..PLAYER_TYPES.len() {
                    spans.push(option.get_entry(settings, menu_index == i, level));
                }
            }
            lines.push(Line::from(spans));
        } else {
            spans.extend(option.get_line(settings, menu_index == i));
            lines.push(Line::from(spans));
        }
        if i == 1 {
            lines.push(Line::raw(""));
        }
    }
    Text::from(lines)
}
