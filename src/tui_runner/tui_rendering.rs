use super::{
    tui_animations::{AnimationContext, Layer},
    tui_settings::{
        BordersStyle, ColorScheme, GameSetup, GraphicsState, ScreenSplitting, SettingRenderer,
        SettingSelection, Settings, WhiteTilesStyle,
    },
    AIResult, AIState, AnimationState, UIState,
};
use crate::{
    pieces::{PieceType, Player},
    state::{HiveBoard, HiveContent, HiveContext, HiveGameState, HiveResult},
    tui_graphics::{self, piece_color},
    tui_runner::tui_settings::FilterAISuggestions,
};
use core::slice;
use ratatui::{
    layout::{Alignment, Constraint, Layout},
    prelude::{CrosstermBackend, Terminal},
    style::Color,
    text::{Line, Span, Text},
    widgets::{
        canvas::{Canvas, Context},
        Block, Borders, Clear, Paragraph, Wrap,
    },
};
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    io::{self, Stdout},
    iter,
};
use tgp_ai::RatingType;
use tgp_board::{
    open_board::OpenIndex,
    structures::directions::{DirectionEnumerable, HexaDirection},
    Board,
};

#[derive(Clone, Copy)]
pub struct AllState<'a> {
    pub game_state: &'a HiveGameState,
    pub settings: Settings,
    pub board_annotations: &'a HashMap<OpenIndex, usize>,
    pub piece_annotations: &'a HashMap<PieceType, usize>,
    pub ui_state: UIState,
    pub animation_state: &'a AnimationState,
    pub ai_state: &'a AIState,
    pub menu_selection: SettingSelection,
    pub graphics_state: GraphicsState,
}

pub const DARK_WHITE: Color = Color::from_u32(0x00DADADA);

const MENU_HELP_LONG: &'static str = "\
    This is a TUI version of the board game Hive. \
    Hive is a chess-like game where both players place and move pieces \
    that correspond to insects. More information is available in the \
    rules summary (press [h] or [r]).\n\
    \n\
    Everything is controlled with the keyboard. Keys are mapped to \
    specific actions (which one depends on whether you are in the menu or \
    in-game). The general rule is: an action marked with [x] is selected by \
    pressing x. In addition, [Esc] and [↲/Space] are used for general menu \
    navigation.\n\
    \n\
    [↑↓] or [1-9] to select a specific setting\n\
    [←→] to change the current setting
";

const MENU_HELP_SHORT: &'static str = "\
    This is a TUI version of the board game Hive \
    (press [h] or [r] for rules).\
    Everything is controlled with the keyboard. The general rule is: \
    an action marked with [x] is selected by pressing x.\n\
    \n\
    [↑↓] or [1-9] to select a specific setting\n\
    [←→] to change the current setting
";

const IN_GAME_HELP_LONG: &'static str = "\
    press a number to select a move\n   \
    (two digits: press [Space] or [↲] first)\n\
    \n\
    [↑↓←→] or [wasd] to move the screen\n\
    [+-] or [PageDown PageUp] for zooming\n\
    [⇆] to switch the displayed pieces\n\
    \n\
    [h] to show AI suggested moves\n   \
    (for human players: AI assistant)\n\
    [u/z] to undo a move\n\
    [r/y] to redo a move\n\
    [c/BackSp] to cancel AI moves/animations\n\
    [n] to start the AI move (if not automatic)\n\
    \n\
    [q/Esc] to get back to the menu\
";

const IN_GAME_HELP_SHORT: &'static str = "\
    press a number to select a move\n   \
    (two digits: press [Space] or [↲] first)\n\
    \n\
    [↑↓←→] or [wasd] to move the screen\n\
    [+-] or [PageDown PageUp] for zooming\n\
    [⇆] to switch the displayed pieces\n\
    [h] to show AI suggested moves\n\
    [u]ndo or [r]edo a move\n\
    [c/BackSp] to cancel AI moves/animations\n\
    [n] to start the AI move (if not automatic)\n\
    [q/Esc] to get back to the menu\
";

const INTRODUCTION_BEFORE_QUEEN: &'static str = "\
    Hive is a chess-like game played with hexagonal pieces. \
    Unlike chess, there is no explicit board. Instead, the board \
    is implicitely represented by the placed pieces. At the start \
    of the game, the board is empty. As in chess, both players have \
    alternating turns. At your turn, you can either place a new piece \
    or move an already placed piece. \
    Victory is achieved by surrounding the enemy \
";
const INTRODUCTION_AFTER_QUEEN: &'static str = " with six pieces (can be friend or enemy).";

const PLACEMENT: &'static str = "\
    Any available piece can be placed at a position that is not yet \
    occupied. The position must be adjacent to one of your own pieces \
    and must not be adjacent to an enemy piece.\
";

const MOVEMENT_BEFORE_QUEEN: &'static str = "\
    Moving a piece is only possible if you have already placed your \
";

const MOVEMENT_AFTER_QUEEN: &'static str = ". \
    You can move pieces that have at least one valid destination according \
    to their specific movement rules. However, the movement must obey the \
    one hive rule: at any point in time, all pieces must be connected to \
    each other. A move that would split the hive (even only temporarily \
    during the movement) is thus forbidden. Also, sufficient space must \
    be available: you can not move to an adjacent position if both the \
    next left and the next right position are occupied, since the piece \
    does not \"fit\" through the bottleneck.\
";

const QUEEN: [&'static str; 3] = [
    "The most important piece, since you loose the game if the ",
    " is surrounded. She must be placed within your first four turns. \
    Once placed, the ",
    " can move to any adjacent non-occupied position.",
];

const ANT: [&'static str; 2] = [
    "Flexible pieces that travel the edge of the hive. The ",
    " can move to any position that is reachable via a non-occupied path.",
];

const SPIDER: [&'static str; 4] = [
    "Similar to ",
    ", ",
    " crawl along the hive. However, the ",
    " is much more restricted: it must move exactly three steps.",
];

const GRASSHOPPER: [&'static str; 2] = [
    "Less flexible, but ignores obstacles. The ",
    " jumps over adjacent pieces (at least one!) in a straight line, \
    moving to the first non-occupied position in the line.",
];

const BEETLE: [&'static str; 5] = [
    "Slow but powerful. The ",
    " moves only one position at a time. However, he has a super-power: the ",
    " can crawl atop another piece and travel along the top of the hive. \
    The piece below the ",
    " is blocked and unable to move. Also, the position now counts as \
    belonging to the ",
    " player for the purpose of placing new pieces (i.e., you can place \
    a piece next to it even if the piece below is not yours).",
];

const REMARKS: &'static str = "\
    Notes: This is a summary, not a comprehensive explanation of the \
    rules. Please refer to the official rules of Hive instead.\
";

fn build_help_text(settings: &Settings) -> Text<'static> {
    // first a few helpers
    let insert = |start, val, end, color| {
        Line::from(vec![
            Span::raw(start),
            Span::styled(val, color),
            Span::raw(end),
        ])
    };
    let combine = |left: Line<'static>, right: Line<'static>| {
        let mut spans = left.spans;
        spans.extend(right.spans);
        Line::from(spans)
    };
    let piece_text = |mut string_iter: slice::Iter<&'static str>, p_type: PieceType| {
        let name = p_type.name();
        let mut line = insert(
            "",
            format!("{}: ", name),
            *string_iter.next().unwrap(),
            piece_color(p_type),
        );
        for str in string_iter {
            line = combine(
                line,
                insert("", String::from(name), str, piece_color(p_type)),
            );
        }
        line
    };

    // construct the text
    let primary = settings.color_scheme.primary();
    let spider = piece_color(PieceType::Spider);
    let ant = piece_color(PieceType::Ant);
    let lines = vec![
        Line::styled("Summary of Rules", primary).alignment(Alignment::Center),
        Line::raw(""),
        insert(
            INTRODUCTION_BEFORE_QUEEN,
            String::from("Bee Queen"),
            INTRODUCTION_AFTER_QUEEN,
            piece_color(PieceType::Queen),
        ),
        Line::raw(""),
        insert("", String::from("Placing a piece: "), PLACEMENT, primary),
        Line::raw(""),
        combine(
            Line::from(Span::styled("Moving a piece: ", primary)),
            insert(
                MOVEMENT_BEFORE_QUEEN,
                String::from("Bee Queen"),
                MOVEMENT_AFTER_QUEEN,
                piece_color(PieceType::Queen),
            ),
        ),
        Line::raw(""),
        piece_text(QUEEN.iter(), PieceType::Queen),
        Line::raw(""),
        piece_text(ANT.iter(), PieceType::Ant),
        Line::raw(""),
        combine(
            combine(
                insert("", String::from("Spider: "), SPIDER[0], spider),
                insert("", String::from("Ants"), SPIDER[1], ant),
            ),
            combine(
                insert("", String::from("Spiders"), SPIDER[2], spider),
                insert("", String::from("Spider"), SPIDER[3], spider),
            ),
        ),
        Line::raw(""),
        piece_text(GRASSHOPPER.iter(), PieceType::Grasshopper),
        Line::raw(""),
        piece_text(BEETLE.iter(), PieceType::Beetle),
        Line::raw(""),
        Line::raw(""),
        Line::raw(REMARKS),
    ];
    Text::from(lines)
}

pub fn render(
    terminal: &mut Terminal<CrosstermBackend<Stdout>>,
    setting_renderer: &SettingRenderer,
    state: AllState<'_>,
    // we don't actually mutate anything, this is just an API limitation
    settings: &mut Settings,
    game_setup: &GameSetup,
) -> io::Result<([f64; 2], [f64; 2])> {
    let mut output_bound = ([0.0, 0.0], [0.0, 0.0]);
    terminal.draw(|frame| {
        let area = frame.size();
        let menu_contraint = match state.settings.splitting {
            ScreenSplitting::Auto => {
                let cutoff_low = 125;
                let cutoff_high = 250;
                let max_bonus = 13;
                let added = max_bonus
                    * u16::min(area.width, cutoff_high).saturating_sub(cutoff_low)
                    / (cutoff_high - cutoff_low);
                Constraint::Max(57 + added)
            }
            ScreenSplitting::FarLeft => Constraint::Percentage(50),
            ScreenSplitting::Left => Constraint::Percentage(42),
            ScreenSplitting::Normal => Constraint::Percentage(36),
            ScreenSplitting::Right => Constraint::Percentage(30),
            ScreenSplitting::FarRight => Constraint::Percentage(25),
        };
        let splitted_top_level = Layout::horizontal(vec![
            Constraint::Fill(1),
            Constraint::Max(25),
            menu_contraint,
        ])
        .split(area);
        let splitted_in_game =
            Layout::horizontal(vec![Constraint::Fill(1), menu_contraint]).split(area);

        let splitted_layout = if state.ui_state == UIState::Toplevel {
            splitted_top_level
        } else {
            splitted_in_game.clone()
        };
        let canvas_area = splitted_in_game[0];
        let &menu_area = splitted_layout.last().unwrap();
        // the rules summary
        if let UIState::RulesSummary(scroll) = state.ui_state {
            let split_top_line =
                Layout::vertical(vec![Constraint::Max(3), Constraint::Fill(1)]).split(canvas_area);
            let top_line = Line::raw("[ws] or [Space] to scroll, [Esc] or [r/h] to return ")
                .alignment(Alignment::Right);
            let paragraph = Paragraph::new(top_line)
                .block(Block::default().borders(Borders::BOTTOM.complement()));
            frame.render_widget(paragraph, split_top_line[0]);

            let text = build_help_text(settings);
            let paragraph = Paragraph::new(text)
                .block(Block::default().borders(Borders::TOP.complement()))
                .wrap(Wrap { trim: true })
                .scroll((scroll, 0));
            frame.render_widget(paragraph, split_top_line[1]);
        }
        // the board
        else {
            let y_factor = 2.1;
            let zoom = state.graphics_state.zoom_level.multiplier();
            let center_x = state.graphics_state.center_x;
            let center_y = state.graphics_state.center_y;

            let x_len = zoom * (f64::from(canvas_area.width) - 2.5);
            let y_len = zoom * y_factor * (f64::from(canvas_area.height) - 2.5);

            // use same center with the top level layout
            // if state.ui_state.top_level() {
            //     let alt_x_len = zoom * (f64::from(splitted_in_game[0].width) - 2.5);
            //     let diff = x_len - alt_x_len;
            //     center_x += diff;
            // }
            let x_bounds = [center_x - x_len, center_x + x_len];
            let y_bounds = [center_y - y_len, center_y + y_len];
            let canvas = Canvas::default()
                .block(Block::default().borders(Borders::ALL))
                .x_bounds(x_bounds)
                .y_bounds(y_bounds)
                .paint(|ctx| draw_board(ctx, state, x_bounds, y_bounds));
            frame.render_widget(canvas, canvas_area);
            output_bound = (x_bounds, y_bounds);
        }

        if state.ui_state.top_level() {
            // the actions
            if !matches!(state.ui_state, UIState::RulesSummary(_)) {
                let mut action_area = splitted_layout[1];
                let text = Text::from(
                    "[c]ontinue game  [↲]\n\
                    [n]ew game\n\
                    [r]ules summary  [h]\n\
                    \n\
                    [q]uit",
                );
                action_area.height = text.height() as u16 + 2;
                let paragraph = Paragraph::new(text)
                    .block(Block::default().title("Actions").borders(Borders::ALL));
                frame.render_widget(Clear, action_area);
                frame.render_widget(paragraph, action_area);
            }

            let (player_size, mut settings_size, mut help_size) = (5, 10, 16);
            let both_settings = menu_area.height >= 27 + help_size;
            let small_help = menu_area.height < player_size + settings_size + help_size;
            assert!(!(both_settings && small_help));
            if both_settings {
                settings_size = 22;
            }
            if small_help {
                (settings_size, help_size) = (8, 8);
            }
            let [player_area, settings_area, help_area] = *Layout::vertical([
                Constraint::Max(player_size),
                Constraint::Min(settings_size),
                Constraint::Min(help_size),
            ])
            .split(menu_area) else {
                unreachable!()
            };
            // the players
            {
                let text = setting_renderer
                    .render_player_settings(settings, state.menu_selection.player_index());
                let paragraph = Paragraph::new(text).block(
                    Block::default()
                        .title("Players")
                        .borders(Borders::BOTTOM.complement()),
                );
                frame.render_widget(paragraph, player_area);
            }

            // the settings
            {
                if both_settings {
                    let [general, graphic] =
                        *Layout::vertical([Constraint::Fill(1), Constraint::Fill(1)])
                            .split(settings_area)
                    else {
                        unreachable!()
                    };
                    let par =
                        setting_renderer.render_general_settings(settings, state.menu_selection);
                    frame.render_widget(par, general);
                    let par =
                        setting_renderer.render_graphic_settings(settings, state.menu_selection);
                    frame.render_widget(par, graphic);
                } else {
                    let par =
                        setting_renderer.render_current_settings(settings, state.menu_selection);
                    frame.render_widget(par, settings_area);
                }
            }

            // the top level help text
            {
                let text = if small_help {
                    MENU_HELP_SHORT
                } else {
                    MENU_HELP_LONG
                };
                let paragraph = Paragraph::new(text)
                    .block(Block::default().title("Help").borders(Borders::ALL))
                    .wrap(Wrap { trim: true });
                frame.render_widget(paragraph, help_area);
            }
        } else {
            let piece_height = 10;
            let double_piece_height = 20;
            let help_height = Text::raw(IN_GAME_HELP_LONG).height() as u16 + 2;
            let small_in_game_help = menu_area.height < double_piece_height + help_height;
            let constraints = if small_in_game_help {
                let small_height = Text::raw(IN_GAME_HELP_SHORT).height() as u16 + 2;
                [
                    Constraint::Min(piece_height - 3),
                    Constraint::Max(small_height),
                ]
            } else {
                [Constraint::Min(piece_height), Constraint::Max(help_height)]
            };
            let [piece_area, tooltip_area] = *Layout::vertical(constraints).split(menu_area) else {
                unreachable!()
            };

            let suggestion_height = 12;
            let render_both = state.ui_state == UIState::ShowAIMoves
                && piece_area.height >= suggestion_height + piece_height;
            let (piece_area, suggestion_area) = if render_both {
                let split =
                    Layout::vertical([Constraint::Fill(1), Constraint::Max(suggestion_height)])
                        .split(piece_area);
                (split[0], split[1])
            } else {
                (piece_area, piece_area)
            };
            // the AI suggested moves
            if state.ui_state == UIState::ShowAIMoves {
                let text;
                if let Some(ai_result) = state.ai_state.actual_result() {
                    text = ai_suggestions(ai_result, state.game_state);
                } else {
                    text = Text::raw("Waiting for AI calculation...");
                }
                let paragraph = Paragraph::new(text).block(
                    Block::default()
                        .title("Suggested Moves")
                        .borders(Borders::ALL),
                );
                frame.render_widget(paragraph, suggestion_area);
            }
            // the available pieces
            if render_both || state.ui_state != UIState::ShowAIMoves {
                let zoom = state.settings.piece_zoom_level.multiplier();
                let x_len = zoom * (f64::from(2 * piece_area.width) - 4.5);
                let y_len = zoom * 2.1 * (f64::from(2 * piece_area.height) - 4.48);
                let canvas = Canvas::default()
                    .block(
                        Block::default()
                            .title("Available Pieces")
                            .borders(Borders::ALL),
                    )
                    .x_bounds([0.0, x_len])
                    .y_bounds([-y_len, 0.0])
                    .paint(|ctx| draw_pieces(ctx, state, game_setup, x_len));
                frame.render_widget(canvas, piece_area);
            }

            // the in game help text
            {
                let text = if small_in_game_help {
                    IN_GAME_HELP_SHORT
                } else {
                    IN_GAME_HELP_LONG
                };
                let paragraph = Paragraph::new(text)
                    .block(Block::default().title("Help").borders(Borders::ALL));
                frame.render_widget(paragraph, tooltip_area);
            }

            // the message that might be shown at the top of the board
            {
                let message_hor = Layout::horizontal(vec![
                    Constraint::Fill(1),
                    Constraint::Max(42),
                    Constraint::Fill(1),
                ])
                .split(canvas_area);
                let message_vert = Layout::vertical(vec![
                    Constraint::Fill(1),
                    Constraint::Max(6),
                    Constraint::Fill(20),
                ])
                .split(message_hor[1]);
                let msg_area = message_vert[1];
                match state.ui_state {
                    UIState::ShowOptions(true, _) => {
                        frame.render_widget(Clear, msg_area);
                        frame.render_widget(skip_turn_message(state.game_state), msg_area);
                    }
                    UIState::GameFinished(result) => {
                        frame.render_widget(Clear, msg_area);
                        frame.render_widget(game_finished_message(settings, result), msg_area);
                    }
                    _ => (),
                }
            }
        }
    })?;
    Ok(output_bound)
}

pub fn translate_index(OpenIndex { x, y }: OpenIndex) -> (f64, f64) {
    let x = f64::from(i32::try_from(x).unwrap());
    let y = f64::from(i32::try_from(y).unwrap());
    (x * 21.0, y * 24.0 - x * 12.0)
}

pub fn color_palette(index: usize) -> Color {
    match index {
        0 => ColorScheme::RED,
        1 => ColorScheme::BLUE,
        2 => ColorScheme::GREEN,
        3 => ColorScheme::PURPLE,
        4 => ColorScheme::ORANGE,
        5 => ColorScheme::TURQUOISE,
        6 => ColorScheme::YELLOW_GREEN,
        7 => ColorScheme::PINK,
        _ => panic!("invalid color index"),
    }
}

fn skip_turn_message(game_state: &HiveGameState) -> Paragraph<'static> {
    let player_name = match game_state.player() {
        Player::White => "white",
        Player::Black => "black",
    };
    let msg = Text::from(vec![
        Line::raw(format!("The {} player has no available move", player_name))
            .alignment(Alignment::Center),
        Line::raw("and must therefore skip its turn.").alignment(Alignment::Center),
        Line::raw(""),
        Line::raw("continue with [1] or [↲]").alignment(Alignment::Center),
    ]);
    Paragraph::new(msg).block(Block::default().borders(Borders::ALL))
}

fn game_finished_message(settings: &Settings, result: HiveResult) -> Paragraph<'static> {
    let primary_color = settings.color_scheme.primary();
    let msg;
    if let Some(player) = result.to_player() {
        let player_name = match player {
            Player::White => "white",
            Player::Black => "black",
        };
        msg = Text::from(vec![
            Line::raw(""),
            Line::styled("Victory!", primary_color).alignment(Alignment::Center),
            Line::raw(""),
            Line::styled(format!("The {} player has won!", player_name), Color::White)
                .alignment(Alignment::Center),
        ]);
    } else {
        msg = Text::from(vec![
            Line::raw(""),
            Line::styled("Draw!", primary_color).alignment(Alignment::Center),
            Line::raw(""),
            Line::styled("None of the players could get on top!", Color::White)
                .alignment(Alignment::Center),
        ]);
    }
    Paragraph::new(msg).block(Block::default().borders(Borders::ALL).style(primary_color))
}

pub fn postprocess_ai_suggestions(ai_result: &mut AIResult, settings: &Settings) {
    const MOVES_CUTOFF: RatingType = 20;
    const MAX_PER_FIELD: usize = 2;
    const MAX_SHOWN: usize = 6;

    let ratings = &mut ai_result.all_ratings;
    let annotations = &mut ai_result.annotations;
    let mut seen_pieces = HashSet::new();
    let (best_rating, _, _) = ratings[0];
    ratings.retain(|(r, _, _)| best_rating - r <= MOVES_CUTOFF);

    let filter_moves = settings.filter_ai_suggestions == FilterAISuggestions::Yes;
    annotations.clear();
    let mut index = 0;
    while index < ratings.len() {
        let (_, path, ctx) = &ratings[index];
        let keep_entry;
        match ctx {
            HiveContext::TargetField(ctx) => {
                let &from = ctx.inner();
                let to = ctx[*path.last().unwrap()];
                let from_len = annotations.entry(from).or_default().len();
                let to_len = annotations.entry(to).or_default().len();
                keep_entry = !filter_moves || (from_len < MAX_PER_FIELD && to_len < MAX_PER_FIELD);
                if keep_entry {
                    // unwrap: the entries already exist due to the previous entry calls
                    annotations.get_mut(&from).unwrap().push((index, None));
                    annotations.get_mut(&to).unwrap().push((index, None));
                }
            }
            HiveContext::Piece(ctx) => {
                let &field = ctx.inner();
                let (piece_t, _) = ctx[*path.last().unwrap()];
                let entry = annotations.entry(field).or_default();
                keep_entry = !filter_moves
                    || (entry.len() < MAX_PER_FIELD && !seen_pieces.contains(&piece_t));
                if keep_entry {
                    entry.push((index, Some(piece_t)));
                    seen_pieces.insert(piece_t);
                }
            }
            HiveContext::SkipPlayer => return,
            HiveContext::BaseField(_) => unreachable!("this context should not be possible here"),
        };
        if keep_entry {
            index += 1;
        } else {
            ratings.remove(index);
        }
    }
    while ratings.len() > MAX_SHOWN {
        ratings.pop();
    }
}

pub fn ai_suggestions(ai_result: &AIResult, game_state: &HiveGameState) -> Text<'static> {
    let desired_width = 45;
    let mut text = Text::raw(format!("    {:<40}{}", "Move Description", "Rating"));
    for (i, (rating, indizes, ctx)) in ai_result.all_ratings.iter().enumerate() {
        let mut line = match ctx {
            HiveContext::TargetField(ctx) => {
                let &from = ctx.inner();
                let piece = game_state.board()[from].top().unwrap();
                let p_color = tui_graphics::piece_color(piece.p_type);
                let line = Line::from(vec![
                    Span::styled(format!("[{}] ", i + 1), color_palette(i)),
                    Span::raw("move "),
                    Span::styled(piece.p_type.name(), p_color),
                    Span::raw(" according to "),
                    Span::styled(format!("({})", i + 1), color_palette(i)),
                ]);
                line
            }
            HiveContext::Piece(ctx) => {
                let (p_type, _) = ctx[*indizes.last().unwrap()];
                let p_color = tui_graphics::piece_color(p_type);
                let line = Line::from(vec![
                    Span::styled(format!("[{}] ", i + 1), color_palette(i)),
                    Span::raw("place "),
                    Span::styled(p_type.name(), p_color),
                    Span::raw(" at field "),
                    Span::styled(format!("({}{})", p_type.letter(), i + 1), color_palette(i)),
                ]);
                line
            }
            _ => unreachable!("this context should not be possible here"),
        };
        let offset = desired_width - usize::min(line.width(), desired_width - 1);
        let placeholder = iter::repeat(" ").take(offset).collect::<String>();
        line.spans.extend([
            Span::raw(placeholder),
            Span::styled(format!("{:>3}", rating), color_palette(i)),
        ]);
        text.lines.push(line);
    }
    text.extend([
        Line::raw(""),
        Line::raw("press a number to apply the according move"),
        Line::raw("[Esc] or [h] to return"),
    ]);
    text
}

fn draw_level_of_board(
    ctx: &mut Context<'_>,
    state: AllState<'_>,
    anim_ctx: AnimationContext<'_>,
    board: &HiveBoard,
    level: usize,
) {
    assert!(level <= 2);
    let zoom = state.graphics_state.zoom_level.multiplier();
    let animation = state.animation_state.animation();
    let get_piece = |content: &HiveContent| {
        if level >= 2 {
            if content.len() > 2 {
                content.top().copied()
            } else {
                None
            }
        } else {
            content.get(level).copied()
        }
    };
    let scale = match level {
        0 => 1.0,
        1 => 0.7,
        _ => 0.5,
    };

    // draw interiors
    for field in board.iter_fields() {
        let (x_mid, y_mid) = translate_index(field.index());
        field
            .content_checked()
            .and_then(|content| get_piece(content))
            .inspect(|piece| {
                let is_white = piece.player == Player::White;
                if is_white || level > 0 {
                    let color = if is_white {
                        DARK_WHITE
                    } else {
                        Color::from_u32(0)
                    };
                    if level == 0 {
                        draw_interior(ctx, state.settings.white_tiles_style, x_mid, y_mid, color);
                    } else {
                        let y_offset = (1.0 - scale) * 10.0 + level as f64 * 0.5;
                        tui_graphics::draw_hex_interior(
                            ctx,
                            x_mid,
                            y_mid + y_offset,
                            color,
                            false,
                            scale,
                        );
                    }
                }
            });
    }
    animation.inspect(|a| a.draw(ctx, anim_ctx, Layer::Interiors(level)));
    ctx.layer();

    // draw pieces
    for field in board.iter_fields() {
        let (x_mid, mut y_mid) = translate_index(field.index());
        y_mid += level as f64 * 0.3;
        field
            .content_checked()
            .and_then(|content| get_piece(content).map(|p| (p, content.len())))
            .inspect(|&(piece, stack_size)| match level {
                0 => {
                    let y_mid = if stack_size == 1 {
                        y_mid
                    } else {
                        y_mid - tui_graphics::to_bottom_offset(piece.p_type)
                    };
                    tui_graphics::draw_piece(ctx, piece.p_type, x_mid, y_mid, zoom)
                }
                1 => tui_graphics::draw_small_piece(ctx, piece.p_type, x_mid, y_mid, zoom),
                _ => tui_graphics::draw_tiny_piece(ctx, piece.p_type, x_mid, y_mid, zoom),
            });
    }
    animation.inspect(|a| a.draw(ctx, anim_ctx, Layer::Pieces(level)));
    ctx.layer();
}

pub fn draw_board(
    ctx: &mut Context<'_>,
    state: AllState<'_>,
    x_bounds: [f64; 2],
    y_bounds: [f64; 2],
) {
    let zoom = state.graphics_state.zoom_level.multiplier();
    let animation = state.animation_state.animation();
    let anim_ctx = AnimationContext {
        graphics_state: &state.graphics_state,
        x_bounds,
        y_bounds,
    };
    let temporary_state = animation.and_then(|a| a.get_temporary_state(state.game_state));
    let board = temporary_state
        .as_ref()
        .map_or(state.game_state.board(), |s| s.board());

    // first round: draw borders
    for field in board.iter_fields() {
        let (x_mid, y_mid) = translate_index(field.index());
        if let Some(content) = field.content_checked() {
            if content.is_empty()
                && state.settings.borders_style == BordersStyle::Partial
                && board.size() > 1
            {
                // we don't want to draw "lonely" borders, thus we check where tiles are adjacent
                let mut to_draw = Vec::new();
                for direction in HexaDirection::enumerate_all() {
                    if let Some(next) = field.next(direction).and_then(|f| f.content_checked()) {
                        if !next.is_empty() {
                            to_draw.push(direction.prev_direction());
                            to_draw.push(direction);
                            to_draw.push(direction.next_direction());
                        }
                    }
                }
                to_draw.sort();
                to_draw.dedup();
                tui_graphics::draw_restricted_hex_border(ctx, x_mid, y_mid, &to_draw);
            } else if state.settings.borders_style != BordersStyle::None {
                tui_graphics::draw_hex_border(ctx, x_mid, y_mid);
            }
            // which sides should be drawn?
        }
    }
    animation.inspect(|a| a.draw(ctx, anim_ctx, Layer::Borders));
    ctx.layer();

    // draw the three levels (bottom, stacked, top)
    draw_level_of_board(ctx, state, anim_ctx, board, 0);
    draw_level_of_board(ctx, state, anim_ctx, board, 1);
    draw_level_of_board(ctx, state, anim_ctx, board, 2);

    // is a specific field selected?
    let primary_color = state.settings.color_scheme.primary();
    match state.ui_state {
        UIState::PositionSelected(index) | UIState::PieceSelected(index) => {
            let (x_mid, y_mid) = translate_index(index);
            let stack_size = board[index].len();
            let level = if stack_size <= 1 { 0 } else { stack_size - 1 };
            tui_graphics::draw_interior_hex_border_lvl(
                ctx,
                x_mid,
                y_mid,
                0.0,
                0.0,
                primary_color,
                level,
            );
        }
        UIState::GameFinished(result) => {
            if let Some((index, _)) = find_losing_queen(board, result) {
                let (x_mid, y_mid) = translate_index(index);
                tui_graphics::draw_interior_hex_border(ctx, x_mid, y_mid, 0.0, 0.0, primary_color);
            }
        }
        _ => (),
    }
    animation.inspect(|a| a.draw(ctx, anim_ctx, Layer::Selection));
    ctx.layer();

    animation.inspect(|a| a.draw(ctx, anim_ctx, Layer::Final));

    // print indizes
    if matches!(
        state.ui_state,
        UIState::ShowOptions(false, _) | UIState::PieceSelected(_)
    ) {
        let color = if matches!(state.ui_state, UIState::ShowOptions(false, _)) {
            state.settings.color_scheme.primary()
        } else {
            state.settings.color_scheme.secondary()
        };
        for (&board_index, &number) in state.board_annotations.iter() {
            let (x, y) = translate_index(board_index);
            if number >= 9 {
                ctx.print(
                    x - zoom * 4.0,
                    y - zoom * 2.0,
                    Line::styled(format!("[ {}]", number + 1), color),
                );
            } else {
                ctx.print(
                    x - zoom * 1.0,
                    y - zoom * 2.0,
                    Line::styled(format!("[{}]", number + 1), color),
                );
            }
        }
    } else if state.ui_state == UIState::ShowAIMoves {
        if let Some(ai_result) = state.ai_state.actual_result() {
            let annot_to_str = |(i, piece_t): (usize, Option<PieceType>)| {
                let str = if let Some(piece_t) = piece_t {
                    format!("({}{})", piece_t.letter(), i + 1)
                } else {
                    format!("({})", i + 1)
                };
                Line::styled(str, color_palette(i))
            };

            for (&field, annots) in ai_result.annotations.iter().filter(|(_, a)| !a.is_empty()) {
                let pair_up = annots.len() > 2;
                let rows = if pair_up {
                    (annots.len() + 1) / 2
                } else {
                    annots.len()
                };
                let y_diff_per_row = 4.26;
                let y_start_offset = 0.5 * y_diff_per_row * ((rows - 1) as f64) - 2.0;
                for row_index in 0..rows {
                    let y_offset = zoom * (y_start_offset - row_index as f64 * y_diff_per_row);
                    let (x, y) = translate_index(field);
                    if pair_up && 2 * row_index + 1 < annots.len() {
                        ctx.print(
                            x - zoom * 5.97,
                            y + y_offset,
                            annot_to_str(annots[2 * row_index]),
                        );
                        ctx.print(
                            x + zoom * 2.1,
                            y + y_offset,
                            annot_to_str(annots[2 * row_index + 1]),
                        );
                    } else {
                        let a = annots[if pair_up { 2 * row_index } else { row_index }];
                        let x_shift = if a.1.is_some() { 2.0 } else { 1.0 };
                        ctx.print(x - zoom * x_shift, y + y_offset, annot_to_str(a));
                    }
                }
            }
        }
    }
}

fn available_pieces_player(state: AllState<'_>) -> Player {
    if matches!(
        state.ui_state,
        UIState::PlaysAnimation(false) | UIState::ShowOptions(_, true)
    ) {
        state.game_state.player().switched()
    } else {
        state.game_state.player()
    }
}

fn row_column_index(state: AllState<'_>, stack_index: usize, xlen: f64) -> (usize, usize) {
    let zoom = state.settings.piece_zoom_level.multiplier();
    let stacks_per_row = f64::floor((xlen - 2.0 * 12.5) / (23.0 + 4.0 * zoom)) as usize + 1;
    (stack_index / stacks_per_row, stack_index % stacks_per_row)
}

fn draw_pieces(ctx: &mut Context<'_>, state: AllState<'_>, game_setup: &GameSetup, xlen: f64) {
    let zoom = state.settings.piece_zoom_level.multiplier();
    let player = available_pieces_player(state);
    let (pieces, _) = state.game_state.pieces_for_player(player);
    let interior_color = match player {
        Player::White => DARK_WHITE,
        Player::Black => Color::from_u32(0x00303030),
    };

    let pieces_with_counts = if let UIState::PositionSelected(_) = state.ui_state {
        let mut annots = state
            .piece_annotations
            .iter()
            .map(|(&p_type, &index)| (p_type, index))
            .collect::<Vec<_>>();
        annots.sort_by_key(|&(_, index)| index);
        annots
            .iter()
            .map(|(p_type, _)| (*p_type, pieces[p_type]))
            .collect::<Vec<_>>()
    } else {
        pieces
            .iter()
            .map(|(&p_type, &count)| (p_type, count))
            .collect::<Vec<_>>()
    };

    let max_depth = 3;
    let initial_pieces = game_setup.pieces_for_player(player);
    let max_initial_count = initial_pieces
        .iter()
        .map(|(_, &count)| count)
        .max()
        .unwrap();
    let y_offset_per_row = -58.0 + (u32::saturating_sub(4, max_initial_count) as f64) * 9.0;
    let get_coords = |i: usize, depth: u32| {
        let (row, col) = row_column_index(state, i, xlen);
        let x = 12.5 + (col as f64) * (23.0 + 4.0 * zoom);
        let y = -10_f64 - 8.0 * zoom - f64::from(u32::try_from(max_depth - depth).unwrap()) * 9.0;
        (x, y + (row as f64) * y_offset_per_row)
    };
    for depth in 0..=max_depth {
        // multiple iteration to draw "stacked" pieces
        let it = pieces_with_counts
            .iter()
            .copied()
            .filter(|&(_, count)| count > 0)
            .enumerate();

        // draw the interior
        for (i, (_, count)) in it.clone() {
            if count > max_depth - depth {
                let (x, y) = get_coords(i, depth);
                draw_interior(ctx, state.settings.white_tiles_style, x, y, interior_color);
            }
        }
        ctx.layer();
        // draw the pieces themselves
        let secondary_color = state.settings.color_scheme.secondary();
        for (i, (piece_t, count)) in it {
            if count > max_depth - depth {
                let (x, y) = get_coords(i, depth);
                tui_graphics::draw_piece(ctx, piece_t, x, y, zoom);

                if depth == max_depth {
                    let y_pos: f64 = (row_column_index(state, i, xlen).0 as f64)
                        * (y_offset_per_row - 2.5 * zoom)
                        - 2.0;
                    let intial_count = initial_pieces[&piece_t];
                    if let UIState::PositionSelected(_) = state.ui_state {
                        let number = state.piece_annotations[&piece_t] + 1;
                        ctx.print(
                            x - zoom * 3.5 - 2.5,
                            y_pos,
                            Line::styled(format!("[{number}]"), secondary_color),
                        );
                        ctx.print(
                            x + zoom * 1.0 + 1.2,
                            y_pos,
                            Line::raw(format!("{count}/{intial_count}")),
                        );
                    } else {
                        let content;
                        let x_shift;
                        if zoom < 1.5 {
                            x_shift = zoom * 4.0;
                            content = format!("{count} / {intial_count}")
                        } else {
                            x_shift = zoom + 1.0;
                            content = format!("{count}/{intial_count}")
                        };
                        ctx.print(x - x_shift, y_pos, Line::styled(content, Color::White));
                    }
                }
            }
        }
        ctx.layer();
    }
}

pub fn draw_interior(ctx: &mut Context<'_>, style: WhiteTilesStyle, x: f64, y: f64, color: Color) {
    match style {
        WhiteTilesStyle::Full => tui_graphics::draw_hex_interior(ctx, x, y, color, false, 1.0),
        WhiteTilesStyle::Border => {
            tui_graphics::draw_interior_hex_border(ctx, x, y, 1.5, 1.5, color)
        }
        WhiteTilesStyle::Hybrid => tui_graphics::draw_hex_interior(ctx, x, y, color, true, 1.0),
    }
}

pub fn find_losing_queen(board: &HiveBoard, result: HiveResult) -> Option<(OpenIndex, Player)> {
    result.to_player().and_then(|player| {
        for field in board.iter_fields() {
            let queen_pos = field.content().bottom().and_then(|&piece| {
                (piece.p_type == PieceType::Queen && piece.player != player)
                    .then_some(field.index())
            });
            if let Some(pos) = queen_pos {
                return Some((pos, player));
            }
        }
        None
    })
}
