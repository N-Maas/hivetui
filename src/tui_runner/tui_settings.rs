use num_enum::{IntoPrimitive, TryFromPrimitive};
use ratatui::text::{Line, Span};
use std::fmt::Debug;

use crate::pieces::Player;

use super::RED;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default, TryFromPrimitive, IntoPrimitive)]
#[repr(u8)]
pub enum ZoomLevel {
    Bird = 0,
    Strategical = 1,
    Wider = 2,
    #[default]
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct GraphicsState {
    pub center_x: f64,
    pub center_y: f64,
    pub zoom_level: ZoomLevel,
    pub piece_zoom_level: ZoomLevel,
    pub white_tiles_style: WhiteTilesStyle,
    pub borders_style: BordersStyle,
    pub splitting: ScreenSplitting,
    pub animation_speed: AnimationSpeed,
    pub animation_style: AnimationStyle,
    pub moving_tile_style: MovingTileStyle,
}

impl GraphicsState {
    pub fn new() -> Self {
        GraphicsState {
            center_x: 0.0,
            center_y: 0.0,
            zoom_level: ZoomLevel::default(),
            piece_zoom_level: ZoomLevel::Wider,
            white_tiles_style: WhiteTilesStyle::default(),
            borders_style: BordersStyle::default(),
            splitting: ScreenSplitting::default(),
            animation_speed: AnimationSpeed::default(),
            animation_style: AnimationStyle::default(),
            moving_tile_style: MovingTileStyle::default(),
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

    pub fn get_animation_speed(&self, _player: Player) -> AnimationSpeed {
        self.animation_speed
    }

    pub fn should_play_animation(&self, _player: Player) -> bool {
        self.animation_speed != AnimationSpeed::Off
    }
}

pub trait MenuSetting {
    fn increase(&self, state: &mut GraphicsState);

    fn decrease(&self, state: &mut GraphicsState);

    fn get_line(&self, state: &mut GraphicsState, highlight: bool) -> Line;
}

pub fn create_menu_setting<
    'a,
    E: Copy + TryFrom<u8> + Into<u8> + 'a,
    F: Fn(&mut GraphicsState) -> &mut E + 'a,
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

struct MenuSettingImpl<E, F: Fn(&mut GraphicsState) -> &mut E> {
    prefix: &'static str,
    texts: Vec<&'static str>,
    get_setting_fn: F,
}

impl<E: Copy + Into<u8>, F: Fn(&mut GraphicsState) -> &mut E> MenuSettingImpl<E, F> {
    fn val(&self, state: &mut GraphicsState) -> u8 {
        (*(self.get_setting_fn)(state)).into()
    }
}

impl<E: Copy + TryFrom<u8> + Into<u8>, F: Fn(&mut GraphicsState) -> &mut E> MenuSetting
    for MenuSettingImpl<E, F>
where
    <E as TryFrom<u8>>::Error: Debug,
{
    fn increase(&self, state: &mut GraphicsState) {
        let current_val = self.val(state);
        if usize::from(current_val + 1) < self.texts.len() {
            *(self.get_setting_fn)(state) = E::try_from(current_val + 1).unwrap();
        }
    }

    fn decrease(&self, state: &mut GraphicsState) {
        let current_val = self.val(state);
        if current_val > 0 {
            *(self.get_setting_fn)(state) = E::try_from(current_val - 1).unwrap();
        }
    }

    fn get_line(&self, state: &mut GraphicsState, highlight: bool) -> Line {
        let current_val = self.val(state);
        let mut spans = vec![Span::raw(self.prefix)];
        spans.extend(self.texts.iter().enumerate().map(|(i, &str)| {
            if usize::from(current_val) == i && highlight {
                Span::styled(format!("<{str}>"), RED)
            } else if usize::from(current_val) == i {
                Span::raw(format!("<{str}>"))
            } else {
                Span::raw(format!(" {str} "))
            }
        }));
        Line::from(spans)
    }
}

pub fn build_settings() -> Vec<Box<dyn MenuSetting>> {
    vec![
        create_menu_setting(
            "screen splitting (left to right): ",
            vec!["1", "2", "3", "4", "5"],
            |g_state| &mut g_state.splitting,
        ),
        create_menu_setting(
            "available pieces display size: ",
            vec!["1", "2", "3", "4", "5"],
            |g_state| &mut g_state.piece_zoom_level,
        ),
        create_menu_setting(
            "white tiles filling style: ",
            vec!["full", "border", "hybrid"],
            |g_state| &mut g_state.white_tiles_style,
        ),
        create_menu_setting(
            "border drawing style: ",
            vec!["complete", "partial", "none"],
            |g_state| &mut g_state.borders_style,
        ),
        create_menu_setting(
            "animation speed: ",
            vec!["1", "2", "3", "4", "5", "6", "off"],
            |g_state| &mut g_state.animation_speed,
        ),
        create_menu_setting(
            "animation style: ",
            vec!["blink", "plain", "blink-only-ai", "rainbow"],
            |g_state| &mut g_state.animation_style,
        ),
        create_menu_setting(
            "moving tile style: ",
            vec!["filled", "transparent", "minimal"],
            |g_state| &mut g_state.moving_tile_style,
        ),
    ]
}
