use crate::ai::{Character, Difficulty};
use num_enum::{IntoPrimitive, TryFromPrimitive};
use ratatui::style::Color;
use serde::{Deserialize, Serialize};
use std::{fmt::Debug, io::Read};

use crate::pieces::Player;

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
pub enum AICharacter {
    #[default]
    Balanced = 0,
    Aggressive = 1,
    Defensive = 2,
    Strategic = 3,
}

impl AICharacter {
    pub fn into_character(self) -> Character {
        match self {
            AICharacter::Balanced => Character::Balanced,
            AICharacter::Aggressive => Character::Aggressive,
            AICharacter::Defensive => Character::Defensive,
            AICharacter::Strategic => Character::Strategic,
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
pub enum FillingStyle {
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
pub enum DarkTileColor {
    #[default]
    Black = 0,
    DarkGray = 1,
    DarkBlue = 2,
}

impl DarkTileColor {
    pub fn color(self) -> Color {
        match self {
            DarkTileColor::Black => ColorScheme::SOFT_BLACK,
            DarkTileColor::DarkGray => ColorScheme::DARK_GRAY,
            DarkTileColor::DarkBlue => ColorScheme::DARK_BLUE,
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
    Gold = 4,
}

impl ColorScheme {
    pub const DARK_WHITE: Color = Color::from_u32(0x00DADADA);
    pub const GRAY: Color = Color::from_u32(0x00808080);
    pub const RED: Color = Color::from_u32(0x00E03030);
    pub const ORANGE: Color = Color::from_u32(0x00D89030);
    pub const BLUE: Color = Color::from_u32(0x00308AEA);
    pub const TURQUOISE: Color = Color::from_u32(0x0030D0D0);
    pub const GREEN: Color = Color::from_u32(0x0010D050);
    pub const YELLOW_GREEN: Color = Color::from_u32(0x00A0D030);
    pub const PURPLE: Color = Color::from_u32(0x009030DA);
    pub const PINK: Color = Color::from_u32(0x00D830B0);
    pub const GOLD: Color = Color::from_u32(0x00BABA10);
    pub const SILVER: Color = Color::from_u32(0x009A9AB0);
    pub const TEXT_GRAY: Color = Color::from_u32(0x00A0A0A0);
    pub const TEXT_YELLOW: Color = Color::from_u32(0x00D8D830);
    pub const DARK_GRAY: Color = Color::from_u32(0x00464646);
    pub const SOFT_BLACK: Color = Color::from_u32(0x002A2A2A);
    pub const DARK_BLUE: Color = Color::from_u32(0x00303090);

    pub fn primary(&self) -> Color {
        match self {
            ColorScheme::Red => Self::RED,
            ColorScheme::Blue => Self::BLUE,
            ColorScheme::Green => Self::GREEN,
            ColorScheme::Purple => Self::PURPLE,
            ColorScheme::Gold => Self::GOLD,
        }
    }

    pub fn secondary(&self) -> Color {
        match self {
            ColorScheme::Red => Self::ORANGE,
            ColorScheme::Blue => Self::TURQUOISE,
            ColorScheme::Green => Self::YELLOW_GREEN,
            ColorScheme::Purple => Self::PINK,
            ColorScheme::Gold => Self::SILVER,
        }
    }

    pub fn extreme_values(&self) -> (u8, u8, u8) {
        match self {
            ColorScheme::Red => (0xFF, 0x30, 0x30),
            ColorScheme::Blue => (0x30, 0x90, 0xEA),
            ColorScheme::Green => (0x30, 0xFF, 0x30),
            ColorScheme::Purple => (0x90, 0x30, 0xEA),
            ColorScheme::Gold => (0xE0, 0xE0, 0x30),
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
    pub show_tutorial: bool,
    pub white_player_type: PlayerType,
    pub black_player_type: PlayerType,
    #[serde(default)]
    pub white_ai_character: AICharacter,
    #[serde(default)]
    pub black_ai_character: AICharacter,
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
    pub filling_style: FillingStyle,
    #[serde(default)]
    pub dark_tile_color: DarkTileColor,
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
            show_tutorial: true,
            black_player_type: PlayerType::AI1,
            ..Default::default()
        }
    }

    pub fn player_type(&self, player: Player) -> PlayerType {
        match player {
            Player::White => self.white_player_type,
            Player::Black => self.black_player_type,
        }
    }

    pub fn ai_character(&self, player: Player) -> AICharacter {
        match player {
            Player::White => self.white_ai_character,
            Player::Black => self.black_ai_character,
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
