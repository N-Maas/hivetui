use ratatui::{style::Color, widgets::canvas::Context};
use tgp_board::open_board::OpenIndex;

use crate::tui_graphics;

use super::{translate_index, RED};

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

    fn draw(&self, ctx: &mut Context<'_>, step: usize, layer: Layer);
}

pub struct BaseEffect<F: Fn(&mut Context<'_>, f64, (f64, f64))> {
    x_start: f64,
    y_start: f64,
    x_end: f64,
    y_end: f64,
    layer: Layer,
    total_steps: usize,
    draw_fn: F,
}

impl<F: Fn(&mut Context<'_>, f64, (f64, f64))> BaseEffect<F> {
    pub fn new(steps: usize, layer: Layer, start: (f64, f64), end: (f64, f64), draw_fn: F) -> Self {
        Self {
            x_start: start.0,
            y_start: start.1,
            x_end: end.0,
            y_end: end.1,
            layer,
            total_steps: steps,
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
            draw_fn,
        }
    }
}

impl<F: Fn(&mut Context<'_>, f64, (f64, f64))> AnimationEffect for BaseEffect<F> {
    fn total_steps(&self) -> usize {
        self.total_steps
    }

    fn draw(&self, ctx: &mut Context<'_>, step: usize, layer: Layer) {
        assert!(step <= self.total_steps);
        if layer == self.layer {
            let ratio = step as f64 / self.total_steps as f64;
            let x = (1.0 - ratio) * self.x_start + ratio * self.x_end;
            let y = (1.0 - ratio) * self.y_start + ratio * self.y_end;
            (self.draw_fn)(ctx, ratio, (x, y));
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

    fn draw(&self, ctx: &mut Context<'_>, step: usize, layer: Layer) {
        self.a.draw(ctx, step, layer);
        self.b.draw(ctx, step, layer);
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

    fn draw(&self, ctx: &mut Context<'_>, step: usize, layer: Layer) {
        if step <= self.a.total_steps() {
            self.a.draw(ctx, step, layer);
        }
        if step >= self.a.total_steps() {
            self.b.draw(ctx, step - self.a.total_steps(), layer);
        }
    }
}

pub struct Animation {
    effect: Box<dyn AnimationEffect>,
    current_step: usize,
    total_steps: usize,
}

impl Animation {
    pub fn new<A: AnimationEffect + 'static>(a: A) -> Self {
        let total_steps = a.total_steps();
        Self {
            effect: Box::new(a),
            current_step: 0,
            total_steps,
        }
    }

    pub fn is_finished(&self) -> bool {
        self.current_step >= self.total_steps
    }

    pub fn next_step(&mut self) {
        self.current_step = usize::min(self.current_step + 1, self.total_steps);
    }

    pub fn draw(&self, ctx: &mut Context<'_>, layer: Layer) {
        ctx.layer();
        self.effect.draw(ctx, self.current_step, layer);
    }
}

pub fn blink_field_effect(
    steps: usize,
    index: OpenIndex,
    color_start: (u8, u8, u8),
    color_end: (u8, u8, u8),
) -> impl AnimationEffect {
    let (x, y) = translate_index(index);
    BaseEffect::new_static(steps, Layer::Interiors, x, y, move |ctx, ratio, (x, y)| {
        let r = ((1.0 - ratio) * f64::from(color_start.0) + ratio * f64::from(color_end.0)).round()
            as u32;
        let g = ((1.0 - ratio) * f64::from(color_start.1) + ratio * f64::from(color_end.1)).round()
            as u32;
        let b = ((1.0 - ratio) * f64::from(color_start.2) + ratio * f64::from(color_end.2)).round()
            as u32;
        let color = Color::from_u32((r << 16) | (g << 8) | b);
        tui_graphics::draw_interior_hex_border(ctx, x, y, 1.0, ratio * 10.0, color)
    })
}

pub fn blink_field_default(steps: usize, index: OpenIndex) -> impl AnimationEffect {
    return blink_field_effect(steps, index, (0xFF, 0x30, 0x30), (0x70, 0x70, 0x70));
}
