use ratatui::{
    style::Color,
    widgets::canvas::{Context, Points},
};
use tgp_board::{open_board::OpenIndex, Board};

use crate::{
    pieces::{PieceType, Player},
    state::HiveGameState,
    tui_graphics,
};

use super::{
    tui_rendering::{self, translate_index},
    tui_settings::{
        AnimationStyle, ColorScheme, FillingStyle, GraphicsState, MovingTileStyle, Settings,
    },
};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Layer {
    Borders,
    /// current level of board
    Interiors(usize),
    /// current level of board
    Pieces(usize),
    Selection,
    Final,
}

pub trait AnimationEffect {
    fn total_steps(&self) -> usize;

    fn draw(
        &self,
        ctx: &mut Context<'_>,
        anim_ctx: AnimationContext<'_>,
        step: usize,
        layer: Layer,
    );
}

pub struct BaseEffect<F: Fn(&mut Context<'_>, AnimationContext<'_>, f64, (f64, f64))> {
    x_start: f64,
    y_start: f64,
    x_end: f64,
    y_end: f64,
    layer: Layer,
    total_steps: usize,
    disabled: bool,
    draw_fn: F,
}

impl<F: Fn(&mut Context<'_>, AnimationContext<'_>, f64, (f64, f64))> BaseEffect<F> {
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
        Self::new(steps, layer, (x, y), (x, y), draw_fn)
    }

    pub fn new_anchored<'a>(
        steps: usize,
        layer: Layer,
        relative_x: f64,
        relative_y: f64,
        draw_fn: F,
    ) -> BaseEffect<impl Fn(&mut Context<'_>, AnimationContext<'_>, f64, (f64, f64)) + 'a>
    where
        F: 'a,
    {
        BaseEffect::new_static(steps, layer, 0.0, 0.0, move |ctx, anim_ctx, ratio, _| {
            let [x1, x2] = anim_ctx.x_bounds;
            let [y1, y2] = anim_ctx.y_bounds;
            draw_fn(
                ctx,
                anim_ctx,
                ratio,
                (x1 + relative_x * (x2 - x1), y1 + relative_y * (y2 - y1)),
            )
        })
    }

    // fn disabled(mut self) -> Self {
    //     self.disabled = true;
    //     self.total_steps = 0;
    //     self
    // }
}

impl<F: Fn(&mut Context<'_>, AnimationContext<'_>, f64, (f64, f64))> AnimationEffect
    for BaseEffect<F>
{
    fn total_steps(&self) -> usize {
        self.total_steps
    }

    fn draw(
        &self,
        ctx: &mut Context<'_>,
        anim_ctx: AnimationContext<'_>,
        step: usize,
        layer: Layer,
    ) {
        assert!(step <= self.total_steps);
        if layer == self.layer && !self.disabled {
            let ratio = step as f64 / self.total_steps as f64;
            let x = (1.0 - ratio) * self.x_start + ratio * self.x_end;
            let y = (1.0 - ratio) * self.y_start + ratio * self.y_end;
            (self.draw_fn)(ctx, anim_ctx, ratio, (x, y));
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
        anim_ctx: AnimationContext<'_>,
        step: usize,
        layer: Layer,
    ) {
        self.a.draw(ctx, anim_ctx, step, layer);
        ctx.layer();
        self.b.draw(ctx, anim_ctx, step, layer);
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
        anim_ctx: AnimationContext<'_>,
        step: usize,
        layer: Layer,
    ) {
        if step <= self.a.total_steps() {
            self.a.draw(ctx, anim_ctx, step, layer);
        }
        if step > self.a.total_steps() {
            self.b
                .draw(ctx, anim_ctx, step - self.a.total_steps(), layer);
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
        anim_ctx: AnimationContext<'_>,
        step: usize,
        layer: Layer,
    ) {
        self.as_ref().draw(ctx, anim_ctx, step, layer)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AnimationContext<'a> {
    pub graphics_state: &'a GraphicsState,
    pub x_bounds: [f64; 2],
    pub y_bounds: [f64; 2],
}

pub struct Animation {
    effect: Box<dyn AnimationEffect>,
    temporary_state: Box<dyn Fn(&HiveGameState, usize) -> Option<HiveGameState>>,
    pub current_step: usize,
    pub total_steps: usize,
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

    pub fn draw(&self, ctx: &mut Context<'_>, anim_ctx: AnimationContext<'_>, layer: Layer) {
        ctx.layer();
        self.effect.draw(ctx, anim_ctx, self.current_step, layer);
    }
}

pub fn mark_field(
    steps: usize,
    index: OpenIndex,
    color: Color,
    level: usize,
) -> BaseEffect<impl Fn(&mut Context<'_>, AnimationContext<'_>, f64, (f64, f64))> {
    let (x, y) = translate_index(index);
    BaseEffect::new_static(
        steps,
        Layer::Selection,
        x,
        y,
        move |ctx, _g, _ratio, (x, y)| {
            tui_graphics::draw_interior_hex_border_lvl(ctx, x, y, 0.0, 0.0, color, level)
        },
    )
}

pub fn blink_field(
    steps: usize,
    index: OpenIndex,
    color_start: (u8, u8, u8),
    color_end: (u8, u8, u8),
    level: usize,
) -> impl AnimationEffect {
    let (x, y) = translate_index(index);
    BaseEffect::new_static(
        steps,
        Layer::Interiors(usize::min(level, 2)),
        x,
        y,
        move |ctx, _g, ratio, (x, y)| {
            // make it a bit less gray
            let c_ratio = ratio - f64::min(ratio, 0.1);
            let r = ((1.0 - c_ratio) * f64::from(color_start.0) + c_ratio * f64::from(color_end.0))
                .round() as u32;
            let g = ((1.0 - c_ratio) * f64::from(color_start.1) + c_ratio * f64::from(color_end.1))
                .round() as u32;
            let b = ((1.0 - c_ratio) * f64::from(color_start.2) + c_ratio * f64::from(color_end.2))
                .round() as u32;
            let color = Color::from_u32((r << 16) | (g << 8) | b);
            tui_graphics::draw_interior_hex_border_lvl(ctx, x, y, 0.0, ratio * 10.0, color, level)
        },
    )
}

pub fn blink_field_default(
    settings: &Settings,
    steps: usize,
    index: OpenIndex,
    level: usize,
) -> impl AnimationEffect {
    let start = settings.color_scheme.extreme_values();
    blink_field(steps, index, start, (0x70, 0x70, 0x70), level)
}

pub fn rainbow_field(
    steps: usize,
    length: usize,
    index: OpenIndex,
    level: usize,
) -> impl AnimationEffect {
    let colors = [
        Color::from_u32(0x00E00040),
        Color::from_u32(0x00DA9000),
        Color::from_u32(0x00D0D000),
        Color::from_u32(0x0000C865),
        Color::from_u32(0x000065EE),
        Color::from_u32(0x007500DA),
    ];
    let (x, y) = translate_index(index);
    BaseEffect::new_static(
        steps,
        Layer::Interiors(usize::min(level, 2)),
        x,
        y,
        move |ctx, _g, ratio, (x, y)| {
            let progress = ratio * (length as f64);
            for offset_mult in 0..=40 {
                let offset = 0.25 * (offset_mult as f64);
                let local_progress = progress + offset / 3.0;
                assert!(local_progress >= 0.0);
                let index = f64::floor(local_progress) as usize % colors.len();
                tui_graphics::draw_interior_hex_border_lvl(
                    ctx,
                    x,
                    y,
                    offset,
                    0.0,
                    colors[index],
                    level,
                );
            }
        },
    )
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
    style: FillingStyle,
) -> impl AnimationEffect {
    let start = translate_index(start);
    let end = translate_index(end);
    BaseEffect::new(
        steps,
        Layer::Final,
        start,
        end,
        move |ctx, anim_ctx, _ratio, (x, y)| {
            let zoom = anim_ctx.graphics_state.zoom_level.multiplier();
            if draw_interior {
                tui_rendering::board::draw_interior(ctx, style, x, y, color_interior);
                ctx.layer();
            }
            tui_graphics::draw_piece(ctx, piece_t, x, y, zoom);
            if draw_border {
                ctx.layer();
                tui_graphics::draw_interior_hex_border(ctx, x, y, 0.0, 0.0, color_border);
            }
        },
    )
}

pub fn loader(settings: &Settings, steps: usize) -> impl AnimationEffect {
    let secondary_color = settings.color_scheme.secondary();
    BaseEffect::new_anchored(
        steps,
        Layer::Final,
        0.5,
        0.05,
        move |ctx, anim_ctx, ratio, (x, y)| {
            let zoom = anim_ctx.graphics_state.zoom_level.multiplier();
            let x_range = zoom * 13.5;
            let y_range = zoom * 2.0;
            let x_small = zoom * 10.0;
            let inner_limits = (ratio * 2.0 * x_range, ratio * 2.0 * x_range + x_small);

            let draw_the_points = |ctx: &mut Context<'_>, inner| {
                let mut points = Vec::new();
                for x_index in 0..=30 {
                    let x_diff = x_index as f64 * 2.0 * x_range / 30.0;
                    let is_inner = (x_diff >= inner_limits.0 && x_diff <= inner_limits.1)
                        || 2.0 * x_range + x_diff <= inner_limits.1;
                    if is_inner == inner {
                        for y_index in 0..=10 {
                            let y_diff = y_index as f64 * 2.0 * y_range / 10.0;
                            points.push((x - x_range + x_diff, y - y_range + y_diff));
                        }
                    }
                }
                ctx.draw(&Points {
                    coords: &points,
                    color: if inner {
                        secondary_color
                    } else {
                        Color::from_u32(0x00A0A0A0)
                    },
                });
            };
            draw_the_points(ctx, false);
            ctx.layer();
            draw_the_points(ctx, true);
        },
    )
}

pub fn build_blink_animation(
    settings: &Settings,
    player: Player,
    index: OpenIndex,
    short: bool,
    level: usize,
) -> Animation {
    let primary_color = settings.color_scheme.primary();
    let base_steps = if short { 20 } else { 30 };
    let steps = settings.get_animation_speed(player).map_steps(base_steps);
    match settings.animation_style {
        AnimationStyle::Blink => Animation::new(blink_field_default(settings, steps, index, level)),
        AnimationStyle::Plain => {
            Animation::new(mark_field((steps + 2) / 3, index, primary_color, level))
        }
        AnimationStyle::BlinkOnlyAi => {
            if settings.is_ai(player) {
                Animation::new(blink_field_default(settings, steps, index, level))
            } else {
                Animation::new(mark_field((steps + 1) / 2, index, primary_color, level))
            }
        }
        AnimationStyle::Rainbow => {
            let length = if short { 6 } else { 9 };
            Animation::new(rainbow_field(3 * steps / 2 + 2, length, index, level))
        }
    }
}

pub fn build_complete_piece_move_animation(
    settings: &Settings,
    piece_t: PieceType,
    player: Player,
    start: OpenIndex,
    target: OpenIndex,
    target_level: usize,
) -> Animation {
    let speed = settings.get_animation_speed(player);
    let (x1, y1) = translate_index(start);
    let (x2, y2) = translate_index(target);
    let distance = f64::sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1));
    let fly_steps = speed.map_steps_normal(5.0) + 2.5 * speed.map_steps_extreme(distance.sqrt());
    let fly_steps = fly_steps.round() as usize;

    let color = match player {
        Player::White => ColorScheme::DARK_WHITE,
        Player::Black => settings.dark_tile_color.color(),
    };
    let flying = flying_piece(
        fly_steps,
        piece_t,
        start,
        target,
        settings.color_scheme.primary(),
        settings.moving_tile_style != MovingTileStyle::Minimal,
        color,
        settings.moving_tile_style == MovingTileStyle::Filled,
        settings.filling_style,
    );
    let mark = mark_field(fly_steps, target, settings.color_scheme.secondary(), 0);
    let blink = build_blink_animation(settings, player, target, true, target_level);
    Animation::with_state(
        ChainedEffect::new(CombinedEffect::new(flying, mark), blink.into_effect()),
        move |state, step| {
            // remove the piece from the target field during the flight
            let content = state.board().get(target);
            content
                .filter(|c| step <= fly_steps && !c.is_empty())
                .map(|_| {
                    let mut cloned = state.clone();
                    cloned.remove_piece(target);
                    cloned
                })
        },
    )
}
