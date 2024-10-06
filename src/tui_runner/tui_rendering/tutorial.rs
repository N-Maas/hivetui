use core::slice;
use std::mem;

use ratatui::{
    layout::Alignment,
    style::Stylize,
    text::{Line, Span, Text},
};

use crate::{pieces::PieceType, tui_graphics::piece_color, tui_runner::tui_settings::Settings};

const INTRODUCTION_BEFORE_QUEEN: &str = "\
    Hive is a chess-like game played with hexagonal pieces. \
    Unlike chess, there is no explicit board. Instead, the board \
    is implicitely represented by the placed pieces. At the start \
    of the game, the board is empty. As in chess, both players have \
    alternating turns. At your turn, you can either place a new piece \
    or move an already placed piece. \
    Victory is achieved by surrounding the enemy \
";
const INTRODUCTION_AFTER_QUEEN: &str = " with six pieces (can be friend or enemy).";

const PLACEMENT: &str = "\
    Any available piece can be placed at a position that is not yet \
    occupied. The position must be adjacent to one of your own pieces \
    and must not be adjacent to an enemy piece.\
";

const MOVEMENT_BEFORE_QUEEN: &str = "\
    Moving a piece is only possible if you have already placed your \
";

const MOVEMENT_AFTER_QUEEN: &str = ". \
    You can move pieces that have at least one valid destination according \
    to their specific movement rules. However, the movement must obey the \
    one hive rule: at any point in time, all pieces must be connected to \
    each other. A move that would split the hive (even only temporarily \
    during the movement) is thus forbidden. Also, sufficient space must \
    be available: you can not move to an adjacent position if both the \
    next left and the next right position are occupied, since the piece \
    does not \"fit\" through the bottleneck.\
";

const EXTENSIONS: &str = "\
    While the original game only contains the above pieces, some more were \
    added by extensions. From each piece in the following list, one is added \
    to the game setup by different extensions.
";

const QUEEN: [&str; 3] = [
    "The most important piece, since you loose the game if the ",
    " is surrounded. She must be placed within your first four turns. \
    Once placed, the ",
    " can move to any adjacent unoccupied position.",
];

const ANT: [&str; 2] = [
    "Flexible pieces that travel the edge of the hive. The ",
    " can move to any position that is reachable via an unoccupied path.",
];

const SPIDER: [&str; 4] = [
    "Similar to ",
    ", ",
    " crawl along the hive. However, the ",
    " is much more restricted: it must move exactly three steps.",
];

const GRASSHOPPER: [&str; 2] = [
    "Less flexible, but ignores obstacles. The ",
    " jumps over adjacent pieces (at least one!) in a straight line, \
    moving to the first unoccupied position in the line.",
];

const BEETLE: [&str; 5] = [
    "Slow but powerful. The ",
    " moves only one position at a time. However, it has a super-power: the ",
    " can crawl atop another piece and travel along the top of the hive. \
    The piece below the ",
    " is blocked and unable to move. Also, the position now counts as \
    belonging to the ",
    " player for the purpose of placing new pieces (i.e., you can place \
    a piece next to it even if the piece below is not yours).",
];

const LADYBUG: [&str; 3] = [
    "Powerful for short ranges. The ",
    " makes exactly three steps: First, it moves atop an adjacent piece. \
    Second, it moves one position on top of the hive. Third, it moves down to an adjacent \
    unoccupied position. Thus, the ",
    " is only temporarily atop the hive and always ends its move at an unoccupied position.",
];

const MOSQUITO: [&str; 6] = [
    "The ",
    " has no movement on its own, but instead imitates other pieces. It can use the \
    movement rules of any piece adjacent to its current position. However, if the ",
    " uses a ",
    " move to get atop the hive, it must move like a ",
    " until it crawls down. If its only neighbor is another ",
    ", it can not move at all.",
];

const REMARKS: &str = "\
    Note: This is a summary, not a comprehensive explanation of the \
    rules. Please refer to the official rules of Hive instead.\
";

const TUTORIAL_FIRST: &str = "\
    Hive is a chess-like game where both players place and move pieces that \
    correspond to insects, aiming to surround the enemy Queen.\
    This is a TUI (Terminal User Interface) implementation, which means that \
    the game is played exclusively with the keyboard, using keys to navigate \
    the menu and select moves during the game.\
";

const TUTORIAL_LAST: &str = "\
    Generally, something that is done by pressing key X is marked with [X]. \
    The help text in the bottom right provides additional explanations. Press \
    [j] to open a rule summary as well as this tutorial.
";

const TUTORIAL_MENU: &str = "\
    For general navigation, use enter [↲] to continue or confirm an action, \
    escape [Esc] to return to the previous menu and tab [⇆] to switch between \
    different settings. Specific actions such as starting a new game, saving or \
    leaving the game are performed by pressing the displayed key. To change a \
    setting, use the up/down arrow keys [↑↓] for navigation and the left/right \
    arrow keys [←→] to change the selected setting.
";

const TUTORIAL_IN_GAME: &str = "\
    An in-game move always consists of two steps. To move, a piece and the target \
    of the move need to be chosen. To place a new piece, a free position and the \
    type of piece to be placed need to be chosen. By default, the selection works \
    by just pressing the number diplayed at the target. Two-digit numbers must be \
    preceded with a space! Alternatively, the \"move selection\" setting can be \
    changed to entering the number and confirming with [↲]. Further controls, e.g. \
    to move the camera, are shown in the bottom right.
";

const TUTORIAL_TIPS: [&str; 6] = [
    "You can always undo and redo moves. Just press [u] if you accidentially entered the wrong move!",
    "If you are unsure which move makes sense, press [h] to show moves suggested by the AI",
    "For AI opponents, you can select a characteristic which influences the play style \
    of the AI (\"balanced\" tends to be the hardest)",
    "The current game state is automatically saved and will be restored at the start of the game",
    "Change the text size by changing the text size of your terminal (often [Ctrl +/-])",
    "Try playing around with the graphic settings!",
];

pub fn build_rules_summary(settings: &Settings) -> Text<'static> {
    // first a few helpers
    let insert = |start, val, end, color| {
        let span = Span::styled(val, color);
        Line::from(vec![Span::raw(start), span.bold(), Span::raw(end)])
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
    let mosquito = piece_color(PieceType::Mosquito);
    let beetle = piece_color(PieceType::Beetle);
    let lines = vec![
        Line::styled("Summary of Rules", primary)
            .bold()
            .alignment(Alignment::Center),
        Line::raw(""),
        insert(
            INTRODUCTION_BEFORE_QUEEN,
            String::from("Bee Queen"),
            INTRODUCTION_AFTER_QUEEN,
            piece_color(PieceType::Queen),
        ),
        Line::raw(""),
        Line::raw("Placing a Piece").bold(),
        Line::raw(PLACEMENT),
        Line::raw(""),
        Line::raw("Moving a Piece").bold(),
        insert(
            MOVEMENT_BEFORE_QUEEN,
            String::from("Queen"),
            MOVEMENT_AFTER_QUEEN,
            piece_color(PieceType::Queen),
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
        Line::raw("Extensions").bold(),
        Line::raw(EXTENSIONS),
        Line::raw(""),
        piece_text(LADYBUG.iter(), PieceType::Ladybug),
        Line::raw(""),
        combine(
            combine(
                insert("", String::from("Mosquito: "), MOSQUITO[0], mosquito),
                insert("", String::from("Mosquito"), MOSQUITO[1], mosquito),
            ),
            combine(
                combine(
                    insert("", String::from("Mosquito"), MOSQUITO[2], mosquito),
                    insert("", String::from("Beetle"), MOSQUITO[3], beetle),
                ),
                combine(
                    insert("", String::from("Beetle"), MOSQUITO[4], beetle),
                    insert("", String::from("Mosquito"), MOSQUITO[5], mosquito),
                ),
            ),
        ),
        Line::raw(""),
        Line::raw(""),
        Line::raw(REMARKS),
    ];
    Text::from(lines)
}

pub fn build_tutorial(lines: &mut Vec<Line<'static>>, textwidth: u16) {
    lines.push(
        Line::styled("Tutorial", piece_color(PieceType::Queen))
            .bold()
            .alignment(Alignment::Center),
    );
    lines.push(Line::raw(""));
    custom_linebreaks(lines, TUTORIAL_FIRST.into(), 0, true, textwidth);
    lines.push(Line::raw(""));
    custom_linebreaks(lines, TUTORIAL_LAST.into(), 0, true, textwidth);
    lines.push(Line::raw(""));
    lines.push(Line::raw("Menu Navigation").bold());
    custom_linebreaks(lines, TUTORIAL_MENU.into(), 0, true, textwidth);
    lines.push(Line::raw(""));
    lines.push(Line::raw("In Game").bold());
    custom_linebreaks(lines, TUTORIAL_IN_GAME.into(), 0, true, textwidth);
    lines.push(Line::raw(""));

    // the tips are tricky to render: we need manual line breaking
    lines.push(Line::raw("Additional Tips").bold());
    for tip in TUTORIAL_TIPS {
        custom_linebreaks(lines, format!("- {tip}"), 1, false, textwidth);
    }
}

fn custom_linebreaks(
    lines: &mut Vec<Line<'static>>,
    mut input: String,
    indentation: usize,
    trim: bool,
    textwidth: u16,
) {
    while let Some((split_idx, _)) = input
        .match_indices(' ')
        .filter(|&(i, _)| i <= textwidth as usize)
        .last()
        .filter(|_| input.len() > textwidth as usize)
    {
        if split_idx < 5 {
            break;
        }
        let mut remaining = input.split_off(split_idx);
        if trim {
            remaining = remaining.trim().to_string();
        } else {
            remaining = " ".repeat(indentation).to_string() + &remaining;
        }
        lines.push(Line::raw(mem::replace(&mut input, remaining)));
    }
    lines.push(Line::raw(input));
}
