use ratatui::{
    style::Color,
    widgets::canvas::{Circle, Context, Line, Points},
};
use tgp_board::structures::directions::HexaDirection;

use crate::pieces::PieceType;

const HEX_BORDER_POINTS: [(f64, f64); 7] = [
    (-7.0, 12.0),
    (7.0, 12.0),
    (14.0, 0.0),
    (7.0, -12.0),
    (-7.0, -12.0),
    (-14.0, 0.0),
    (-7.0, 12.0),
];
const HEX_BORDER_ORIENTATIONS: [HexaDirection; 6] = [
    HexaDirection::Up,
    HexaDirection::UpRight,
    HexaDirection::DownRight,
    HexaDirection::Down,
    HexaDirection::DownLeft,
    HexaDirection::UpLeft,
];

fn interior_border_points(offset: f64) -> [(f64, f64); 7] {
    let (x, y) = HEX_BORDER_POINTS[0];
    let p0 = (x + offset, y - offset);
    let (x, y) = HEX_BORDER_POINTS[1];
    let p1 = (x - offset, y - offset);
    let (x, y) = HEX_BORDER_POINTS[2];
    let p2 = (x - offset, y);
    let (x, y) = HEX_BORDER_POINTS[3];
    let p3 = (x - offset, y + offset);
    let (x, y) = HEX_BORDER_POINTS[4];
    let p4 = (x + offset, y + offset);
    let (x, y) = HEX_BORDER_POINTS[5];
    let p5 = (x + offset, y);
    let (x, y) = HEX_BORDER_POINTS[6];
    let p6 = (x + offset * 0.7, y - offset);
    [p0, p1, p2, p3, p4, p5, p6]
}

pub fn draw_hex_border(ctx: &mut Context<'_>, x_mid: f64, y_mid: f64) {
    draw_hex_border_impl(
        ctx,
        x_mid,
        y_mid,
        &HEX_BORDER_POINTS,
        &HEX_BORDER_ORIENTATIONS,
        Color::DarkGray,
        1.0,
    );
}

pub fn draw_restricted_hex_border(
    ctx: &mut Context<'_>,
    x_mid: f64,
    y_mid: f64,
    orientations: &[HexaDirection],
) {
    draw_hex_border_impl(
        ctx,
        x_mid,
        y_mid,
        &HEX_BORDER_POINTS,
        &orientations,
        Color::DarkGray,
        1.0,
    );
}

pub fn draw_interior_hex_border_lvl(
    ctx: &mut Context<'_>,
    x_mid: f64,
    y_mid: f64,
    offset: f64,
    width: f64,
    color: Color,
    level: usize,
) {
    match level {
        0 => draw_interior_hex_border(ctx, x_mid, y_mid, offset, width, color),
        1 => draw_small_interior_hex_border(ctx, x_mid, y_mid, offset, width, color),
        _ => draw_tiny_interior_hex_border(ctx, x_mid, y_mid, offset, width, color),
    };
}

pub fn draw_interior_hex_border(
    ctx: &mut Context<'_>,
    x_mid: f64,
    y_mid: f64,
    offset: f64,
    width: f64,
    color: Color,
) {
    assert!(width >= 0.0);
    let mut current = 0.0;
    while current <= width {
        draw_hex_border_impl(
            ctx,
            x_mid,
            y_mid,
            &interior_border_points(offset + current),
            &HEX_BORDER_ORIENTATIONS,
            color,
            1.0,
        );
        current += 0.25;
    }
}

pub fn draw_small_interior_hex_border(
    ctx: &mut Context<'_>,
    x_mid: f64,
    y_mid: f64,
    offset: f64,
    width: f64,
    color: Color,
) {
    assert!(width >= 0.0);
    let shift = 0.3 * 12.0;
    let mut current = 0.0;
    while current <= width {
        draw_hex_border_impl(
            ctx,
            x_mid,
            y_mid + shift,
            &interior_border_points(offset + current),
            &HEX_BORDER_ORIENTATIONS,
            color,
            0.7,
        );
        current += 0.25;
    }
}

pub fn draw_tiny_interior_hex_border(
    ctx: &mut Context<'_>,
    x_mid: f64,
    y_mid: f64,
    offset: f64,
    width: f64,
    color: Color,
) {
    assert!(width >= 0.0);
    let shift = 0.45 * 12.0;
    let mut current = 0.0;
    while current <= width {
        draw_hex_border_impl(
            ctx,
            x_mid,
            y_mid + shift,
            &interior_border_points(offset + current),
            &HEX_BORDER_ORIENTATIONS,
            color,
            0.55,
        );
        current += 0.25;
    }
}

fn draw_hex_border_impl(
    ctx: &mut Context<'_>,
    x_mid: f64,
    y_mid: f64,
    points: &[(f64, f64); 7],
    orientations: &[HexaDirection],
    color: Color,
    scale: f64,
) {
    for i in 0..(points.len() - 1) {
        if orientations.contains(&HEX_BORDER_ORIENTATIONS[i]) {
            let (x1, y1) = points[i];
            let (x2, y2) = points[i + 1];
            ctx.draw(&Line {
                x1: x_mid + scale * x1,
                y1: y_mid + scale * y1,
                x2: x_mid + scale * x2,
                y2: y_mid + scale * y2,
                color,
            });
        }
    }
}

pub fn get_rectangle_points(
    points: &mut Vec<(f64, f64)>,
    x_mid: f64,
    y_mid: f64,
    x_start: i32,
    x_end: i32,
    y_start: i32,
    y_end: i32,
) {
    for x in x_start..=x_end {
        points.push((x_mid + f64::from(x), y_mid + f64::from(y_end)));
        if x < x_end {
            points.push((x_mid + f64::from(x) + 0.5, y_mid + f64::from(y_end)));
        }
        for y in y_start..y_end {
            points.push((x_mid + f64::from(x), y_mid + f64::from(y)));
            points.push((x_mid + f64::from(x), y_mid + f64::from(y) + 0.5));
            if x < x_end {
                points.push((x_mid + f64::from(x) + 0.5, y_mid + f64::from(y)));
                points.push((x_mid + f64::from(x) + 0.5, y_mid + f64::from(y) + 0.5));
            }
        }
    }
}

pub fn fill_rectangle(
    ctx: &mut Context<'_>,
    color: Color,
    x_mid: f64,
    y_mid: f64,
    x_start: i32,
    x_end: i32,
    y_start: i32,
    y_end: i32,
) {
    let mut points = Vec::<(f64, f64)>::new();
    get_rectangle_points(&mut points, x_mid, y_mid, x_start, x_end, y_start, y_end);
    ctx.draw(&Points {
        coords: &points,
        color,
    });
}

pub fn draw_hex_interior(
    ctx: &mut Context<'_>,
    x_mid: f64,
    y_mid: f64,
    color: Color,
    leave_circle: bool,
    scale: f64,
) {
    let mut points = Vec::<(f64, f64)>::new();
    get_rectangle_points(&mut points, x_mid, y_mid, -7, 7, -10, 10);
    if leave_circle {
        points.retain(|(x, y)| {
            let x = x - x_mid;
            let y = y - y_mid;
            x * x + y * y > 40.0
        });
    }

    let orthogonals = [(-12.0, 7.0), (-12.0, -7.0), (12.0, 7.0), (12.0, -7.0)];
    let bases = [(-13.0, 0.0), (-13.0, 0.0), (13.0, 0.0), (13.0, 0.0)];
    let starts = [(-13, 12), (-13, 0), (7, 12), (7, 0)];
    for (((o_x, o_y), (b_x, b_y)), (s_x, s_y)) in orthogonals
        .into_iter()
        .zip(bases.into_iter())
        .zip(starts.into_iter())
    {
        for x in s_x..(s_x + 7) {
            for y in (s_y - 12)..s_y {
                let x = f64::from(x);
                let y = f64::from(y);
                for (x, y) in [(x, y), (x + 0.5, y), (x, y + 0.5), (x + 0.5, y + 0.5)] {
                    if x == b_x {
                        continue;
                    }
                    let vec = ((x - b_x), (y - b_y));

                    let inner_product = o_x * vec.0 + o_y * vec.1;
                    let normalisation = (vec.0 * vec.0 + vec.1 * vec.1).sqrt();
                    if inner_product <= -0.5 * normalisation {
                        points.push((x_mid + x, y_mid + y));
                    }
                }
            }
        }
    }
    if scale != 1.0 {
        for (x, y) in points.iter_mut() {
            let x_val = *x - x_mid;
            let y_val = *y - y_mid;
            *x = x_mid + scale * x_val;
            *y = y_mid + scale * y_val;
        }
    }
    ctx.draw(&Points {
        coords: &points,
        color,
    });
}

pub fn piece_color(piece_t: PieceType) -> Color {
    match piece_t {
        PieceType::Queen => Color::from_u32(0x00D0D010),
        PieceType::Ant => Color::from_u32(0x0062A2F4),
        PieceType::Spider => Color::from_u32(0x009A4800),
        PieceType::Grasshopper => Color::from_u32(0x000DD084),
        PieceType::Beetle => Color::from_u32(0x009A009A),
    }
}

pub fn draw_piece(ctx: &mut Context<'_>, piece_t: PieceType, x_mid: f64, y_mid: f64, zoom: f64) {
    match piece_t {
        PieceType::Queen => draw_queen(ctx, x_mid, y_mid, zoom),
        PieceType::Ant => draw_ant(ctx, x_mid, y_mid, zoom),
        PieceType::Spider => draw_spider(ctx, x_mid, y_mid, zoom),
        PieceType::Grasshopper => draw_grasshopper(ctx, x_mid, y_mid, zoom),
        PieceType::Beetle => draw_beetle(ctx, x_mid, y_mid, zoom),
    }
}

pub fn to_bottom_offset(piece_t: PieceType) -> f64 {
    match piece_t {
        PieceType::Queen => 2.2,
        PieceType::Ant => 1.6,
        PieceType::Spider => 2.4,
        PieceType::Grasshopper => 1.2,
        PieceType::Beetle => 1.6,
    }
}

pub fn draw_small_piece(
    ctx: &mut Context<'_>,
    piece_t: PieceType,
    x_mid: f64,
    y_mid: f64,
    zoom: f64,
) {
    match piece_t {
        PieceType::Beetle => draw_small_beetle(ctx, x_mid, y_mid, zoom),
        _ => panic!("this piece can not be stacked: {:?}", piece_t),
    }
}

pub fn draw_tiny_piece(
    ctx: &mut Context<'_>,
    piece_t: PieceType,
    x_mid: f64,
    y_mid: f64,
    zoom: f64,
) {
    match piece_t {
        PieceType::Beetle => draw_tiny_beetle(ctx, x_mid, y_mid, zoom),
        _ => panic!("this piece can not be stacked: {:?}", piece_t),
    }
}

pub fn draw_queen(ctx: &mut Context<'_>, x_mid: f64, y_mid: f64, zoom: f64) {
    let color = piece_color(PieceType::Queen);
    fill_rectangle(ctx, color, x_mid, y_mid, -1, 1, 3, 4);
    if zoom <= 0.65 {
        ctx.draw(&Points {
            coords: &[(x_mid - 0.5, y_mid + 1.5), (x_mid + 0.5, y_mid + 1.5)],
            color,
        });
    }
    fill_rectangle(ctx, color, x_mid, y_mid, -3, 3, -1, 0);
    fill_rectangle(ctx, color, x_mid, y_mid, -3, 3, -4, -3);
    // antennas
    ctx.draw(&Line {
        x1: x_mid - 1.5,
        y1: y_mid + 4.0,
        x2: x_mid - 2.5,
        y2: y_mid + 6.0,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 1.5,
        y1: y_mid + 4.0,
        x2: x_mid + 2.5,
        y2: y_mid + 6.0,
        color,
    });
    // middle feet
    ctx.draw(&Line {
        x1: x_mid - 3.5,
        y1: y_mid - 1.0,
        x2: x_mid - 6.0,
        y2: y_mid - 3.0,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 3.5,
        y1: y_mid - 1.0,
        x2: x_mid + 6.0,
        y2: y_mid - 3.0,
        color,
    });
    // back feet
    ctx.draw(&Line {
        x1: x_mid - 3.5,
        y1: y_mid - 5.0,
        x2: x_mid - 5.0,
        y2: y_mid - 6.5,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 3.5,
        y1: y_mid - 5.0,
        x2: x_mid + 5.0,
        y2: y_mid - 6.5,
        color,
    });
    // wings
    ctx.draw(&Circle {
        x: x_mid - 5.5,
        y: y_mid + 2.0,
        radius: 1.2,
        color,
    });
    ctx.draw(&Circle {
        x: x_mid - 7.0,
        y: y_mid + 3.5,
        radius: 1.5,
        color,
    });
    if zoom <= 0.55 {
        fill_rectangle(ctx, color, x_mid, y_mid, -7, -7, 3, 4);
    }
    ctx.draw(&Circle {
        x: x_mid + 5.5,
        y: y_mid + 2.0,
        radius: 1.2,
        color,
    });
    ctx.draw(&Circle {
        x: x_mid + 7.0,
        y: y_mid + 3.5,
        radius: 1.5,
        color,
    });
    if zoom <= 0.55 {
        fill_rectangle(ctx, color, x_mid, y_mid, 7, 7, 3, 4);
    }
}

pub fn draw_ant(ctx: &mut Context<'_>, x_mid: f64, y_mid: f64, _zoom: f64) {
    let color = piece_color(PieceType::Ant);
    fill_rectangle(ctx, color, x_mid, y_mid, -1, 1, 5, 7);
    fill_rectangle(ctx, color, x_mid, y_mid, 0, 0, 4, 4);
    fill_rectangle(ctx, color, x_mid, y_mid, -1, 1, 0, 3);
    fill_rectangle(ctx, color, x_mid, y_mid, 0, 0, -1, -1);
    fill_rectangle(ctx, color, x_mid, y_mid, -2, 2, -7, -2);
    // front and middle feet
    ctx.draw(&Line {
        x1: x_mid - 1.5,
        y1: y_mid + 2.5,
        x2: x_mid - 5.5,
        y2: y_mid + 7.0,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 1.5,
        y1: y_mid + 2.5,
        x2: x_mid + 5.5,
        y2: y_mid + 7.0,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid - 1.5,
        y1: y_mid + 2.5,
        x2: x_mid - 5.5,
        y2: y_mid - 2.0,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 1.5,
        y1: y_mid + 2.5,
        x2: x_mid + 5.5,
        y2: y_mid - 2.0,
        color,
    });
    // back feet
    ctx.draw(&Line {
        x1: x_mid - 2.0,
        y1: y_mid - 2.0,
        x2: x_mid - 5.5,
        y2: y_mid - 6.5,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 2.0,
        y1: y_mid - 2.0,
        x2: x_mid + 5.5,
        y2: y_mid - 6.5,
        color,
    });
}

pub fn draw_spider(ctx: &mut Context<'_>, x_mid: f64, y_mid: f64, _zoom: f64) {
    let color = piece_color(PieceType::Spider);
    fill_rectangle(ctx, color, x_mid, y_mid, -2, 2, -1, 2);
    ctx.draw(&Circle {
        x: x_mid,
        y: y_mid + 0.5,
        radius: 2.0,
        color,
    });
    fill_rectangle(ctx, color, x_mid, y_mid, -1, 1, -4, -2);
    ctx.draw(&Circle {
        x: x_mid,
        y: y_mid - 3.5,
        radius: 1.5,
        color,
    });
    // mouth
    ctx.draw(&Line {
        x1: x_mid - 1.0,
        y1: y_mid + 2.5,
        x2: x_mid - 1.5,
        y2: y_mid + 4.0,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 1.0,
        y1: y_mid + 2.5,
        x2: x_mid + 1.5,
        y2: y_mid + 4.0,
        color,
    });
    // front feet
    ctx.draw(&Line {
        x1: x_mid - 2.0,
        y1: y_mid + 2.0,
        x2: x_mid - 5.5,
        y2: y_mid + 6.5,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 2.0,
        y1: y_mid + 2.0,
        x2: x_mid + 5.5,
        y2: y_mid + 6.5,
        color,
    });
    // middle feet
    ctx.draw(&Line {
        x1: x_mid - 2.0,
        y1: y_mid + 1.5,
        x2: x_mid - 6.0,
        y2: y_mid + 3.0,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 2.0,
        y1: y_mid + 1.5,
        x2: x_mid + 6.0,
        y2: y_mid + 3.0,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid - 2.0,
        y1: y_mid - 0.5,
        x2: x_mid - 6.0,
        y2: y_mid - 2.0,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 2.0,
        y1: y_mid - 0.5,
        x2: x_mid + 6.0,
        y2: y_mid - 2.0,
        color,
    });
    // back feet
    ctx.draw(&Line {
        x1: x_mid - 2.0,
        y1: y_mid - 1.5,
        x2: x_mid - 5.5,
        y2: y_mid - 6.0,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 2.0,
        y1: y_mid - 1.5,
        x2: x_mid + 5.5,
        y2: y_mid - 6.0,
        color,
    });
}

pub fn draw_beetle(ctx: &mut Context<'_>, x_mid: f64, y_mid: f64, _zoom: f64) {
    let color = piece_color(PieceType::Beetle);
    fill_rectangle(ctx, color, x_mid, y_mid, -1, 1, 3, 5);
    fill_rectangle(ctx, color, x_mid, y_mid, -2, 2, -1, 2);
    fill_rectangle(ctx, color, x_mid, y_mid, -3, 3, -6, -1);
    ctx.draw(&Circle {
        x: x_mid,
        y: y_mid - 4.5,
        radius: 2.5,
        color,
    });
    // mouth
    fill_rectangle(ctx, color, x_mid, y_mid, -3, -2, 5, 8);
    fill_rectangle(ctx, color, x_mid, y_mid, 2, 3, 5, 8);
    // front feet
    ctx.draw(&Line {
        x1: x_mid - 2.0,
        y1: y_mid,
        x2: x_mid - 5.0,
        y2: y_mid + 3.0,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 2.0,
        y1: y_mid,
        x2: x_mid + 5.0,
        y2: y_mid + 3.0,
        color,
    });
    // middle feet
    ctx.draw(&Line {
        x1: x_mid - 3.0,
        y1: y_mid - 0.5,
        x2: x_mid - 6.0,
        y2: y_mid - 3.0,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 3.0,
        y1: y_mid - 0.5,
        x2: x_mid + 6.0,
        y2: y_mid - 3.0,
        color,
    });
    // back feet
    ctx.draw(&Line {
        x1: x_mid - 3.0,
        y1: y_mid - 3.0,
        x2: x_mid - 6.0,
        y2: y_mid - 6.0,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 3.0,
        y1: y_mid - 3.0,
        x2: x_mid + 6.0,
        y2: y_mid - 6.0,
        color,
    });
}

pub fn draw_small_beetle(ctx: &mut Context<'_>, x_mid: f64, y_mid: f64, _zoom: f64) {
    let color = piece_color(PieceType::Beetle);
    fill_rectangle(ctx, color, x_mid, y_mid, -1, 1, 6, 8);
    fill_rectangle(ctx, color, x_mid, y_mid, -2, 2, 3, 5);
    fill_rectangle(ctx, color, x_mid, y_mid, -3, 3, -1, 3);
    ctx.draw(&Circle {
        x: x_mid,
        y: y_mid + 0.5,
        radius: 2.3,
        color,
    });
    // mouth
    fill_rectangle(ctx, color, x_mid, y_mid, -3, -2, 8, 10);
    fill_rectangle(ctx, color, x_mid, y_mid, 2, 3, 8, 10);
    // front feet
    ctx.draw(&Line {
        x1: x_mid - 2.0,
        y1: y_mid + 4.0,
        x2: x_mid - 4.5,
        y2: y_mid + 6.5,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 2.0,
        y1: y_mid + 4.0,
        x2: x_mid + 5.0,
        y2: y_mid + 6.5,
        color,
    });
    // middle feet
    ctx.draw(&Line {
        x1: x_mid - 3.0,
        y1: y_mid + 3.5,
        x2: x_mid - 5.5,
        y2: y_mid + 1.5,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 3.0,
        y1: y_mid + 3.5,
        x2: x_mid + 5.5,
        y2: y_mid + 1.5,
        color,
    });
    // back feet
    ctx.draw(&Line {
        x1: x_mid - 3.0,
        y1: y_mid + 1.0,
        x2: x_mid - 5.5,
        y2: y_mid - 1.0,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 3.0,
        y1: y_mid + 1.0,
        x2: x_mid + 5.5,
        y2: y_mid - 1.0,
        color,
    });
}

pub fn draw_tiny_beetle(ctx: &mut Context<'_>, x_mid: f64, y_mid: f64, _zoom: f64) {
    let color = piece_color(PieceType::Beetle);
    fill_rectangle(ctx, color, x_mid, y_mid, -1, 1, 6, 8);
    fill_rectangle(ctx, color, x_mid, y_mid, -2, 2, 3, 6);
    ctx.draw(&Circle {
        x: x_mid,
        y: y_mid + 3.7,
        radius: 1.5,
        color,
    });
    // mouth
    fill_rectangle(ctx, color, x_mid, y_mid, -2, -1, 8, 10);
    fill_rectangle(ctx, color, x_mid, y_mid, 1, 2, 8, 10);
    // front feet
    ctx.draw(&Line {
        x1: x_mid - 1.5,
        y1: y_mid + 6.5,
        x2: x_mid - 4.0,
        y2: y_mid + 7.0,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 1.5,
        y1: y_mid + 6.5,
        x2: x_mid + 4.0,
        y2: y_mid + 7.0,
        color,
    });
    // middle feet
    ctx.draw(&Line {
        x1: x_mid - 2.0,
        y1: y_mid + 6.0,
        x2: x_mid - 4.5,
        y2: y_mid + 5.0,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 2.0,
        y1: y_mid + 6.0,
        x2: x_mid + 4.5,
        y2: y_mid + 5.0,
        color,
    });
    // back feet
    ctx.draw(&Line {
        x1: x_mid - 2.0,
        y1: y_mid + 4.5,
        x2: x_mid - 4.5,
        y2: y_mid + 2.5,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 2.0,
        y1: y_mid + 4.5,
        x2: x_mid + 4.5,
        y2: y_mid + 2.5,
        color,
    });
}

pub fn draw_grasshopper(ctx: &mut Context<'_>, x_mid: f64, y_mid: f64, _zoom: f64) {
    let color = piece_color(PieceType::Grasshopper);
    fill_rectangle(ctx, color, x_mid, y_mid, -1, 1, 6, 8);
    fill_rectangle(ctx, color, x_mid, y_mid, -2, 2, -7, 6);
    ctx.draw(&Circle {
        x: x_mid,
        y: y_mid - 6.0,
        radius: 1.8,
        color,
    });
    // antennas
    ctx.draw(&Line {
        x1: x_mid - 0.5,
        y1: y_mid + 8.0,
        x2: x_mid - 2.5,
        y2: y_mid + 9.0,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 0.5,
        y1: y_mid + 8.0,
        x2: x_mid + 2.5,
        y2: y_mid + 9.0,
        color,
    });
    // back foot left
    ctx.draw(&Line {
        x1: x_mid - 2.0,
        y1: y_mid + 0.5,
        x2: x_mid - 5.0,
        y2: y_mid + 2.0,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid - 2.0,
        y1: y_mid + 0.15,
        x2: x_mid - 5.0,
        y2: y_mid + 1.65,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid - 2.0,
        y1: y_mid - 0.25,
        x2: x_mid - 5.0,
        y2: y_mid + 1.25,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid - 5.0,
        y1: y_mid + 1.5,
        x2: x_mid - 6.0,
        y2: y_mid - 7.0,
        color,
    });
    // back foot right
    ctx.draw(&Line {
        x1: x_mid + 2.0,
        y1: y_mid + 0.5,
        x2: x_mid + 5.0,
        y2: y_mid + 2.0,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 2.0,
        y1: y_mid + 0.15,
        x2: x_mid + 5.0,
        y2: y_mid + 1.65,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 2.0,
        y1: y_mid - 0.25,
        x2: x_mid + 5.0,
        y2: y_mid + 1.25,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 5.0,
        y1: y_mid + 1.5,
        x2: x_mid + 6.0,
        y2: y_mid - 7.0,
        color,
    });
    // front feet
    ctx.draw(&Line {
        x1: x_mid - 2.5,
        y1: y_mid + 5.0,
        x2: x_mid - 5.0,
        y2: y_mid + 8.0,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 2.5,
        y1: y_mid + 5.0,
        x2: x_mid + 5.0,
        y2: y_mid + 8.0,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid - 2.5,
        y1: y_mid + 5.0,
        x2: x_mid - 5.5,
        y2: y_mid + 4.5,
        color,
    });
    ctx.draw(&Line {
        x1: x_mid + 2.5,
        y1: y_mid + 5.0,
        x2: x_mid + 5.5,
        y2: y_mid + 4.5,
        color,
    });
}
