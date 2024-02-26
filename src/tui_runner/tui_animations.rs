use ratatui::{style::Color, widgets::canvas::Context};
use tgp_board::{open_board::OpenIndex, Board};

use crate::{
    pieces::{PieceType, Player},
    state::HiveGameState,
    tui_graphics,
};

use super::{
    tui_rendering::{self, translate_index, DARK_WHITE, ORANGE, RED},
    tui_settings::{GraphicsState, MovingTileStyle},
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Layer {
    Borders,
    Interiors,
    Pieces,
    Selection,
    Final,
}

pub trait AnimationEffect {
    fn total_steps(&self) -> usize;

    fn draw(
        &self,
        ctx: &mut Context<'_>,
        graphics_state: &GraphicsState,
        step: usize,
        layer: Layer,
    );
}

pub struct BaseEffect<F: Fn(&mut Context<'_>, &GraphicsState, f64, (f64, f64))> {
    x_start: f64,
    y_start: f64,
    x_end: f64,
    y_end: f64,
    layer: Layer,
    total_steps: usize,
    disabled: bool,
    draw_fn: F,
}

impl<F: Fn(&mut Context<'_>, &GraphicsState, f64, (f64, f64))> BaseEffect<F> {
    pub fn new(steps: usize, layer: Layer, start: (f64, f64), end: (f64, f64), draw_fn: F) -> Self {
        Self {
            x_start: start.0,
            y_start: start.1,
            x_end: end.0,
            y_end: end.1,
            layer,
            total_steps: steps,
            disabled: false,
            draw_fn,
        }
    }

    pub fn new_static(steps: usize, layer: Layer, x: f64, y: f64, draw_fn: F) -> Self {
        Self {
            x_start: x,
            y_start: y,
            x_end: x,
            y_end: y,
            layer,
            total_steps: steps,
            disabled: false,
            draw_fn,
        }
    }

    fn disabled(mut self) -> Self {
        self.disabled = true;
        self.total_steps = 0;
        self
    }
}

impl<F: Fn(&mut Context<'_>, &GraphicsState, f64, (f64, f64))> AnimationEffect for BaseEffect<F> {
    fn total_steps(&self) -> usize {
        self.total_steps
    }

    fn draw(
        &self,
        ctx: &mut Context<'_>,
        graphics_state: &GraphicsState,
        step: usize,
        layer: Layer,
    ) {
        assert!(step <= self.total_steps);
        if layer == self.layer && !self.disabled {
            let ratio = step as f64 / self.total_steps as f64;
            let x = (1.0 - ratio) * self.x_start + ratio * self.x_end;
            let y = (1.0 - ratio) * self.y_start + ratio * self.y_end;
            (self.draw_fn)(ctx, graphics_state, ratio, (x, y));
        }
    }
}

pub struct CombinedEffect<A: AnimationEffect, B: AnimationEffect> {
    a: A,
    b: B,
}

impl<A: AnimationEffect, B: AnimationEffect> CombinedEffect<A, B> {
    pub fn new(a: A, b: B) -> Self {
        Self { a, b }
    }
}

impl<A: AnimationEffect, B: AnimationEffect> AnimationEffect for CombinedEffect<A, B> {
    fn total_steps(&self) -> usize {
        usize::max(self.a.total_steps(), self.b.total_steps())
    }

    fn draw(
        &self,
        ctx: &mut Context<'_>,
        graphics_state: &GraphicsState,
        step: usize,
        layer: Layer,
    ) {
        self.a.draw(ctx, graphics_state, step, layer);
        ctx.layer();
        self.b.draw(ctx, graphics_state, step, layer);
    }
}

pub struct ChainedEffect<A: AnimationEffect, B: AnimationEffect> {
    a: A,
    b: B,
}

impl<A: AnimationEffect, B: AnimationEffect> ChainedEffect<A, B> {
    pub fn new(a: A, b: B) -> Self {
        Self { a, b }
    }
}

impl<A: AnimationEffect, B: AnimationEffect> AnimationEffect for ChainedEffect<A, B> {
    fn total_steps(&self) -> usize {
        self.a.total_steps() + self.b.total_steps()
    }

    fn draw(
        &self,
        ctx: &mut Context<'_>,
        graphics_state: &GraphicsState,
        step: usize,
        layer: Layer,
    ) {
        if step <= self.a.total_steps() {
            self.a.draw(ctx, graphics_state, step, layer);
        }
        if step > self.a.total_steps() {
            self.b
                .draw(ctx, graphics_state, step - self.a.total_steps(), layer);
        }
    }
}

impl AnimationEffect for Box<dyn AnimationEffect> {
    fn total_steps(&self) -> usize {
        self.as_ref().total_steps()
    }

    fn draw(
        &self,
        ctx: &mut Context<'_>,
        graphics_state: &GraphicsState,
        step: usize,
        layer: Layer,
    ) {
        self.as_ref().draw(ctx, graphics_state, step, layer)
    }
}

pub struct Animation {
    effect: Box<dyn AnimationEffect>,
    temporary_state: Box<dyn Fn(&HiveGameState, usize) -> Option<HiveGameState>>,
    current_step: usize,
    total_steps: usize,
}

impl Animation {
    pub fn new<A: AnimationEffect + 'static>(a: A) -> Self {
        let total_steps = a.total_steps();
        Self {
            effect: Box::new(a),
            temporary_state: Box::new(|_, _| None),
            current_step: 0,
            total_steps,
        }
    }

    pub fn with_state<
        A: AnimationEffect + 'static,
        F: Fn(&HiveGameState, usize) -> Option<HiveGameState> + 'static,
    >(
        a: A,
        state_fn: F,
    ) -> Self {
        let total_steps = a.total_steps();
        Self {
            effect: Box::new(a),
            temporary_state: Box::new(state_fn),
            current_step: 0,
            total_steps,
        }
    }

    pub fn into_effect(self) -> impl AnimationEffect {
        self.effect
    }

    pub fn is_finished(&self) -> bool {
        self.current_step >= self.total_steps
    }

    pub fn next_step(&mut self) {
        self.current_step = usize::min(self.current_step + 1, self.total_steps);
    }

    pub fn get_temporary_state(&self, state: &HiveGameState) -> Option<HiveGameState> {
        (self.temporary_state)(state, self.current_step)
    }

    pub fn draw(&self, ctx: &mut Context<'_>, graphics_state: &GraphicsState, layer: Layer) {
        ctx.layer();
        self.effect
            .draw(ctx, graphics_state, self.current_step, layer);
    }
}

pub fn mark_field(
    steps: usize,
    index: OpenIndex,
    color: Color,
) -> BaseEffect<impl Fn(&mut Context<'_>, &GraphicsState, f64, (f64, f64))> {
    let (x, y) = translate_index(index);
    BaseEffect::new_static(
        steps,
        Layer::Interiors,
        x,
        y,
        move |ctx, _g, _ratio, (x, y)| {
            tui_graphics::draw_interior_hex_border(ctx, x, y, 0.0, 0.0, color)
        },
    )
}

pub fn blink_field(
    steps: usize,
    index: OpenIndex,
    color_start: (u8, u8, u8),
    color_end: (u8, u8, u8),
) -> impl AnimationEffect {
    let (x, y) = translate_index(index);
    BaseEffect::new_static(
        steps,
        Layer::Interiors,
        x,
        y,
        move |ctx, _g, ratio, (x, y)| {
            let r = ((1.0 - ratio) * f64::from(color_start.0) + ratio * f64::from(color_end.0))
                .round() as u32;
            let g = ((1.0 - ratio) * f64::from(color_start.1) + ratio * f64::from(color_end.1))
                .round() as u32;
            let b = ((1.0 - ratio) * f64::from(color_start.2) + ratio * f64::from(color_end.2))
                .round() as u32;
            let color = Color::from_u32((r << 16) | (g << 8) | b);
            tui_graphics::draw_interior_hex_border(ctx, x, y, 0.0, ratio * 10.0, color)
        },
    )
}

pub fn blink_field_default(steps: usize, index: OpenIndex) -> impl AnimationEffect {
    blink_field(steps, index, (0xFF, 0x30, 0x30), (0x70, 0x70, 0x70))
}

pub fn flying_piece(
    steps: usize,
    piece_t: PieceType,
    start: OpenIndex,
    end: OpenIndex,
    color_border: Color,
    draw_border: bool,
    color_interior: Color,
    draw_interior: bool,
) -> impl AnimationEffect {
    let start = translate_index(start);
    let end = translate_index(end);
    BaseEffect::new(
        steps,
        Layer::Final,
        start,
        end,
        move |ctx, g, _ratio, (x, y)| {
            if draw_interior {
                tui_rendering::draw_interior(ctx, g, x, y, color_interior);
                ctx.layer();
            }
            tui_graphics::draw_piece(ctx, piece_t, x, y, g.zoom_level.multiplier());
            if draw_border {
                ctx.layer();
                tui_graphics::draw_interior_hex_border(ctx, x, y, 0.0, 0.0, color_border);
            }
        },
    )
}

pub fn build_blink_animation(
    graphics_state: &GraphicsState,
    player: Player,
    index: OpenIndex,
    short: bool,
) -> Animation {
    let base_steps = if short { 20 } else { 30 };
    let steps = graphics_state
        .get_animation_speed(player)
        .map_steps(base_steps);
    match graphics_state.animation_style {
        super::tui_settings::AnimationStyle::Blink => {
            Animation::new(blink_field_default(steps, index))
        }
        super::tui_settings::AnimationStyle::Plain => {
            Animation::new(mark_field((steps + 2) / 3, index, RED))
        }
        super::tui_settings::AnimationStyle::BlinkOnlyAi => todo!(),
        super::tui_settings::AnimationStyle::Rainbow => todo!(),
    }
}

pub fn build_complete_piece_move_animation(
    graphics_state: &GraphicsState,
    piece_t: PieceType,
    player: Player,
    start: OpenIndex,
    target: OpenIndex,
) -> Animation {
    let speed = graphics_state.get_animation_speed(player);
    let (x1, y1) = translate_index(start);
    let (x2, y2) = translate_index(target);
    let distance = f64::sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1));
    let fly_steps = speed.map_steps_normal(5.0) + 2.5 * speed.map_steps_extreme(distance.sqrt());
    let fly_steps = fly_steps.round() as usize;

    let color = match player {
        Player::White => DARK_WHITE,
        Player::Black => Color::from_u32(0),
    };
    let flying = flying_piece(
        fly_steps,
        piece_t,
        start,
        target,
        RED,
        graphics_state.moving_tile_style != MovingTileStyle::Minimal,
        color,
        graphics_state.moving_tile_style == MovingTileStyle::Filled,
    );
    let mark = mark_field(fly_steps, target, ORANGE);
    let blink = build_blink_animation(graphics_state, player, target, true);
    Animation::with_state(
        ChainedEffect::new(CombinedEffect::new(flying, mark), blink.into_effect()),
        move |state, step| {
            // remove the piece from the target field during the flight
            let content = state.board().get(target);
            content
                .filter(|c| step <= fly_steps && !c.is_empty())
                .and_then(|_| {
                    let mut cloned = state.clone();
                    cloned.remove_piece(target);
                    Some(cloned)
                })
        },
    )
}
