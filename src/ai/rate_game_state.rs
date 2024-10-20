use std::cmp::Ordering;

use hivetuilib_ai::RatingType;
use hivetuilib_board::{
    hypothetical::Hypothetical, index_map::ArrayIndexMap, open_board::OpenIndex, prelude::*,
};

use crate::{
    ai::neighbors_in_a_row,
    pieces::{feasible_steps_plain, grasshopper_moves, Piece, PieceType, Player},
    state::{HiveBoard, HiveGameState, HiveResult},
};

use super::{blocks, distance, would_block};

#[derive(Debug, Clone)]
pub struct RatingWeights {
    pub own_queen: f64,
    pub enemy_queen: f64,
    pub own_movability: f64,
    pub enemy_movability: f64,
    pub own_pieces: f64,
    pub enemy_pieces: f64,
}

impl Default for RatingWeights {
    fn default() -> Self {
        Self {
            own_queen: 1.0,
            enemy_queen: 1.0,
            own_movability: 1.0,
            enemy_movability: 1.0,
            own_pieces: 1.0,
            enemy_pieces: 1.0,
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct Flags {
    queen_endangered: bool,
    queen_is_ant_reachable: bool,
    queen_has_beetle_on_top: bool,
    has_free_ant: bool,
    has_blocking_ant: bool,
}

#[derive(Debug, Clone)]
struct MetaData {
    queen_pos: [Option<OpenIndex>; 2],
    queen_neighbors: [u32; 2],
    flags: [Flags; 2],
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MovabilityType {
    Movable,
    AtQueen,
    Blocked(Player),
    Unmovable,
}

impl MetaData {
    fn q_neighbors(&self, player: Player) -> u32 {
        self.queen_neighbors[usize::from(player)]
    }

    fn q_pos(&self, player: Player) -> Option<OpenIndex> {
        self.queen_pos[usize::from(player)]
    }

    fn distance_to_queen(&self, player: Player, field: impl Into<OpenIndex>) -> u32 {
        self.q_pos(player)
            .map_or(10, |pos| distance(pos, field.into()))
    }

    fn adjacent_to_queen(&self, player: Player, field: impl Into<OpenIndex>) -> bool {
        self.distance_to_queen(player, field) == 1
    }

    fn flags(&self, player: Player) -> &Flags {
        &self.flags[usize::from(player)]
    }

    fn flags_mut(&mut self, player: Player) -> &mut Flags {
        &mut self.flags[usize::from(player)]
    }
}

fn calculate_metadata(data: &HiveGameState) -> MetaData {
    let mut meta_data = MetaData {
        queen_pos: [None; 2],
        queen_neighbors: [0; 2],
        flags: Default::default(),
    };

    // first pass: determine position of queen (and whether there is a beetle)
    for field in data.board().iter_fields() {
        if !field.is_empty() {
            let queen = field.content().bottom().unwrap();
            if queen.p_type == PieceType::Queen {
                meta_data.queen_pos[usize::from(queen.player)] = Some(field.index());
                let beetle = field.content().top().unwrap();
                if beetle.p_type == PieceType::Beetle && beetle.player != queen.player {
                    meta_data.flags_mut(queen.player).queen_has_beetle_on_top = true;
                }
            }
        }
    }

    // this needs to be a separate pass, because we need to know the position of the queen
    for field in data.board().iter_fields() {
        if !field.is_empty() {
            let ant = field.content().bottom().unwrap();
            if ant.p_type == PieceType::Ant
                && data.is_movable(field, false)
                && field.content().len() == 1
                && !meta_data.adjacent_to_queen(ant.player.switched(), field)
            {
                let blocks_enemy_piece = field.neighbors().any(|n| {
                    !n.is_empty()
                        && n.content().top().unwrap().player != ant.player
                        && !data.is_movable(n, false)
                        && blocks(field, n)
                });
                if blocks_enemy_piece {
                    meta_data.flags_mut(ant.player).has_blocking_ant = true;
                } else {
                    meta_data.flags_mut(ant.player).has_free_ant = true;
                }
            }
        }
    }

    for &player in [Player::White, Player::Black].iter() {
        if let Some(pos) = meta_data.q_pos(player) {
            let field = data.board().get_field_unchecked(pos);
            meta_data.queen_neighbors[usize::from(player)] =
                field.neighbors().filter(|f| !f.is_empty()).count() as u32;

            // to check whether a queen is ant-reachable, we check whether
            // there is a feasible path to the border
            let mut found_border = false;
            let mut search = field.search();
            while !found_border
                && search.extend(|f| {
                    if f.neighbor_count() < 6 {
                        found_border = true;
                    }
                    feasible_steps_plain(f).map(Field::index)
                })
            {}
            meta_data.flags_mut(player).queen_is_ant_reachable = found_border;
        }

        let enemy_ant_available = meta_data.flags(player.switched()).has_free_ant
            || (meta_data.flags(player.switched()).has_blocking_ant
                && meta_data.q_neighbors(player) >= 4);
        if enemy_ant_available && meta_data.flags(player).queen_is_ant_reachable {
            meta_data.flags_mut(player).queen_endangered = true;
        }
    }
    meta_data
}

fn rate_remaining_pieces(data: &HiveGameState, player: Player) -> RatingType {
    let ants = 10 * data.remaining_pieces(player, PieceType::Ant);
    let ladybugs = 7 * data.remaining_pieces(player, PieceType::Ladybug);
    let mosquitos = 7 * data.remaining_pieces(player, PieceType::Mosquito);
    let spiders = 5 * data.remaining_pieces(player, PieceType::Spider);
    let grasshoppers = 5 * data.remaining_pieces(player, PieceType::Grasshopper);
    // TODO: increase for beetle to 6 or 7?
    let beetles = 6 * data.remaining_pieces(player, PieceType::Beetle);
    (ants + ladybugs + mosquitos + spiders + grasshoppers + beetles) as RatingType
}

fn rate_piece_movability(
    data: &HiveGameState,
    meta: &mut MetaData,
) -> (RatingType, RatingType, RatingType, RatingType, u32, u32) {
    let mut rating = [0; 2];
    let mut beetle_bonus = [0; 2];
    let mut other_bonus = [0; 2];
    let mut pieces_reaching_queen = [0; 2];
    for field in data.board().iter_fields().filter(|f| !f.is_empty()) {
        let &piece = field.content().top().unwrap();
        if field.content().len() == 1 && data.is_movable(field, false) {
            let (mut val, bonus, mut o_bonus, could_reach_queen) =
                single_piece_rating(data, meta, piece, field, MovabilityType::Movable);
            let player = usize::from(piece.player);
            if is_only_half_movable(data, meta, piece, field) {
                val = val * 2 / 3 - 1;
                o_bonus = o_bonus * 2 / 3;
            }
            rating[player] += val;
            beetle_bonus[player] = RatingType::max(beetle_bonus[player], bonus);
            other_bonus[player] += o_bonus;
            pieces_reaching_queen[player] += if could_reach_queen { 1 } else { 0 };
        } else if field.content().len() == 1 && !data.is_movable(field, false) {
            // is this blocked by only one adjacent piece?
            let mut value =
                single_piece_rating(data, meta, piece, field, MovabilityType::Unmovable).0;
            for n in field.neighbors().filter(|f| !f.is_empty()) {
                if data.is_movable(n, false) && blocks(n, field) {
                    let movability = MovabilityType::Blocked(n.content().top().unwrap().player);
                    value = RatingType::max(
                        value,
                        single_piece_rating(data, meta, piece, field, movability).0,
                    );
                }
            }
            rating[usize::from(piece.player)] += value;
        } else if field.content().len() > 1 {
            assert!(data.is_movable(field, false));
            match field.content().pieces() {
                [inner @ .., next, beetle] => {
                    assert!(
                        beetle.p_type == PieceType::Beetle || beetle.p_type == PieceType::Mosquito
                    );
                    for &piece in inner {
                        let movability = MovabilityType::Unmovable;
                        rating[usize::from(piece.player)] +=
                            single_piece_rating(data, meta, piece, field, movability).0;
                    }
                    let mov_type = if blocks(field, field) {
                        MovabilityType::Blocked(beetle.player)
                    } else {
                        MovabilityType::Unmovable
                    };
                    rating[usize::from(next.player)] +=
                        single_piece_rating(data, meta, *next, field, mov_type).0;
                    // a on-top mosquito is just considered a beetle
                    let beetle = Piece {
                        p_type: PieceType::Beetle,
                        ..*beetle
                    };
                    let (val, b_bonus, o_bonus, _) =
                        single_piece_rating(data, meta, beetle, field, MovabilityType::Movable);
                    let b_player = usize::from(beetle.player);
                    rating[b_player] += val;
                    beetle_bonus[b_player] = RatingType::max(beetle_bonus[b_player], b_bonus);
                    other_bonus[b_player] += o_bonus;
                    pieces_reaching_queen[b_player] += 1;
                }
                _ => unreachable!(),
            }
        }
    }

    let p1 = usize::from(data.player());
    let p2 = usize::from(data.player().switched());
    (
        rating[p1],
        rating[p2],
        beetle_bonus[p1] + other_bonus[p1],
        beetle_bonus[p2] + other_bonus[p2],
        pieces_reaching_queen[p1],
        pieces_reaching_queen[p2],
    )
}

// Check whether moving this piece would block another own piece.
fn is_only_half_movable(
    data: &HiveGameState,
    meta: &MetaData,
    piece: Piece,
    field: Field<HiveBoard>,
) -> bool {
    assert!(data.is_movable(field, false));
    let at_queen = meta.adjacent_to_queen(piece.player.switched(), field);
    if !at_queen {
        for f in field.neighbors() {
            if !f.is_empty()
                && f.content().top().unwrap().player == piece.player
                && f.content().len() == 1
                && data.is_movable(f, false)
            {
                let mut hypothetical =
                    Hypothetical::with_index_map(field.board(), ArrayIndexMap::<_, _, 1>::new());
                hypothetical[field].pop();
                let h_f = hypothetical.get_field_unchecked(f.index());

                // Now we need to check whether the OHR holds when moving the adjacent piece.
                // This is not always correct (and doesn't need to be, for the AI)
                let neighbor_count = h_f.neighbors().filter(|f| !f.is_empty()).count();
                if !(neighbor_count <= 1
                    || (neighbor_count <= 4
                        // 4 or less neighbors and all neighbors are in a row => movable
                        && neighbors_in_a_row(h_f))
                    || h_f.content().len() > 1)
                {
                    return true;
                }
            }
        }
    }
    false
}

/// rating, beetle bonus, other bonus, whether queen might be reached
fn single_piece_rating(
    data: &HiveGameState,
    meta: &mut MetaData,
    piece: Piece,
    field: Field<HiveBoard>,
    mut movability: MovabilityType,
) -> (RatingType, RatingType, RatingType, bool) {
    let at_queen = meta.adjacent_to_queen(piece.player.switched(), field);
    if at_queen && (field.content().len() == 1 || movability != MovabilityType::Movable) {
        movability = if movability == MovabilityType::Movable {
            MovabilityType::AtQueen
        } else {
            MovabilityType::Unmovable
        };
    }

    let mut could_reach_queen = false;
    let mut beetle_bonus = 0;
    let mut other_bonus = 0;
    let base_rating = match piece.p_type {
        PieceType::Queen => 10,
        PieceType::Ant => match movability {
            MovabilityType::Movable => {
                if meta.flags(piece.player.switched()).queen_is_ant_reachable {
                    could_reach_queen = true;
                    23
                } else {
                    18
                }
            }
            MovabilityType::Blocked(_) => {
                if meta.flags(piece.player.switched()).queen_is_ant_reachable {
                    16
                } else {
                    12
                }
            }
            MovabilityType::AtQueen => 8,
            MovabilityType::Unmovable => 4,
        },
        PieceType::Spider | PieceType::Grasshopper => match movability {
            MovabilityType::Movable => {
                let moves = match piece.p_type {
                    PieceType::Spider => PieceType::Spider.get_moves(field),
                    PieceType::Grasshopper => grasshopper_moves(field).collect(),
                    _ => unreachable!(),
                };

                let mut reaches_queen = false;
                let mut can_block = false;
                for f in moves {
                    if meta.adjacent_to_queen(piece.player.switched(), f) {
                        reaches_queen = true;
                    } else if piece.player == data.player() {
                        for n in f.neighbors() {
                            if !n.is_empty()
                                && data.is_movable(n, false)
                                && n.content().top().unwrap().player != piece.player
                                && !meta.adjacent_to_queen(piece.player, field)
                                && would_block(f, n)
                            {
                                can_block = true;
                                break;
                            }
                        }
                    }
                }
                could_reach_queen = reaches_queen;
                meta.flags_mut(piece.player.switched()).queen_endangered |= reaches_queen;
                if reaches_queen && piece.player == data.player() {
                    // TODO: does it really make sense to check the current turn here?
                    // TODO: use other_bonus?
                    16
                } else if reaches_queen || (can_block && piece.player == data.player()) {
                    14
                } else if can_block {
                    11
                } else {
                    8
                }
            }
            MovabilityType::Blocked(_) => 10,
            MovabilityType::AtQueen => 5,
            MovabilityType::Unmovable => {
                // grasshoppers are more likely to escape
                if piece.p_type == PieceType::Grasshopper {
                    4
                } else {
                    3
                }
            }
        },
        PieceType::Beetle => match movability {
            MovabilityType::Movable => {
                could_reach_queen = true;
                let enemy = piece.player.switched();
                // offensive beetle
                // TODO: correctly determine queen endangerment?
                let dist = meta.distance_to_queen(enemy, field);
                match dist {
                    0 | 1 => {
                        assert!(field.content().len() > 1);
                        let queen_field =
                            data.board().get_field_unchecked(meta.q_pos(enemy).unwrap());
                        let num_placeable = queen_field
                            .neighbors()
                            .filter(|f| {
                                f.is_empty()
                                    && f.neighbors()
                                        .filter(|n| {
                                            n.content()
                                                .bottom()
                                                .map_or(false, |p| p.player == enemy)
                                        })
                                        .count()
                                        == 1
                            })
                            .count() as u32;
                        let num_placeable =
                            u32::min(num_placeable, data.total_num_pieces(piece.player)) as i32;
                        if dist == 0 {
                            beetle_bonus = 10 + 15 * num_placeable;
                        } else {
                            // TODO: bonus if blocking an enemy piece?
                            beetle_bonus = 10 + 5 * num_placeable;
                        }
                    }
                    2 => {
                        beetle_bonus = 4;
                    }
                    _ => (),
                };
                let mut move_rating = 8 + RatingType::max(0, 7 - dist as RatingType);
                if field.content().len() > 1 && dist > 0 {
                    // bonus: a beetle on top of the hive can not easily be blocked
                    move_rating += 2;
                }
                // defensive beetle
                if beetle_bonus <= 5 {
                    let mut best_blocking = field
                        .neighbors()
                        .filter_map(|f| {
                            if !f.is_empty() && data.is_movable(f, false) {
                                f.content().top()
                            } else {
                                None
                            }
                        })
                        .filter_map(|p| {
                            if p.player == enemy
                                && p.p_type == PieceType::Beetle
                                && meta.adjacent_to_queen(piece.player, field)
                            {
                                Some(19)
                            } else if p.player == enemy {
                                Some(15)
                            } else {
                                None
                            }
                        })
                        .max()
                        .unwrap_or(8);
                    // the beetle can't be used yet
                    if piece.player != data.player() {
                        best_blocking -= 5;
                    }
                    if best_blocking >= move_rating + beetle_bonus {
                        beetle_bonus = 0;
                        best_blocking
                    } else {
                        move_rating
                    }
                } else {
                    move_rating
                }
            }
            MovabilityType::Blocked(_) => 10,
            MovabilityType::AtQueen => 7,
            // beetles are somewhat likely to escape
            MovabilityType::Unmovable => 4,
        },
        PieceType::Ladybug => match movability {
            MovabilityType::Movable => {
                let mut reaches_queen = false;
                for f in PieceType::Ladybug.get_moves(field) {
                    if meta.adjacent_to_queen(piece.player.switched(), f) {
                        reaches_queen = true;
                    }
                }
                // TODO: should we check for blocking opportunities?
                could_reach_queen = reaches_queen;
                meta.flags_mut(piece.player.switched()).queen_endangered |= reaches_queen;

                let dist = meta.distance_to_queen(piece.player.switched(), field);
                if dist <= 2 && reaches_queen {
                    other_bonus = 8;
                } else if dist <= 2 {
                    other_bonus = 5;
                } else if dist == 3 && reaches_queen {
                    other_bonus = 7;
                } else if dist == 3 {
                    other_bonus = 4;
                } else if dist == 4 {
                    other_bonus = 3;
                }
                11
            }
            MovabilityType::Blocked(_) => {
                let dist = meta.distance_to_queen(piece.player.switched(), field);
                if dist <= 3 {
                    18 - 2 * dist as i32
                } else {
                    10
                }
            }
            MovabilityType::AtQueen => 8,
            MovabilityType::Unmovable => 5,
        },
        PieceType::Mosquito => match movability {
            MovabilityType::Movable => {
                use PieceType::*;

                let piece_set =
                    PieceType::get_mosquito_piece_set(field, false).moves_dominance_set();
                let mut best = (7, 0, 0);
                for p_type in [Ant, Ladybug, Beetle, Grasshopper, Spider] {
                    let old_val = best.0 + best.1 + best.2;
                    if piece_set.contains(p_type) {
                        let (mut rating, mut b_bonus, mut bonus, reaches_queen) =
                            single_piece_rating(
                                data,
                                meta,
                                Piece {
                                    player: piece.player,
                                    p_type,
                                },
                                field,
                                MovabilityType::Movable,
                            );
                        could_reach_queen |= reaches_queen;
                        // cases where the mosquito is probably worse than the original
                        if p_type == Ant {
                            rating -= 2;
                        } else if p_type == Ladybug && !reaches_queen {
                            bonus = 0;
                        } else if p_type == Beetle && field.content().len() <= 1 {
                            b_bonus = b_bonus.saturating_sub(2);
                        }
                        if rating + b_bonus + bonus > old_val {
                            best = (rating, b_bonus, bonus);
                        }
                    };
                    if old_val >= 15 {
                        // break early since it seems unlikely it gets better
                        break;
                    }
                }
                beetle_bonus = best.1;
                other_bonus = best.2;
                best.0
            }
            MovabilityType::Blocked(_) => 10,
            MovabilityType::AtQueen => 7,
            MovabilityType::Unmovable => 4,
        },
    };

    (
        match movability {
            MovabilityType::Blocked(blocking_player) => {
                if blocking_player == piece.player && blocking_player == data.player() {
                    // TODO: even worsen this in case of free ant?
                    base_rating * 3 / 4
                } else {
                    base_rating / 2
                }
            }
            _ => base_rating,
        },
        beetle_bonus,
        other_bonus,
        could_reach_queen,
    )
}

fn determine_less_endangered(data: &HiveGameState, meta: &MetaData) -> Option<Player> {
    let player = data.player();
    let mut my_neighbor_count = meta.q_neighbors(player);
    if meta.flags(player).queen_has_beetle_on_top {
        my_neighbor_count += 1;
    }
    let mut enemy_neighbor_count = meta.q_neighbors(player.switched());
    if meta.flags(player.switched()).queen_has_beetle_on_top {
        enemy_neighbor_count += 1;
    }

    match my_neighbor_count.cmp(&enemy_neighbor_count) {
        Ordering::Less => Some(player),
        Ordering::Equal => {
            if meta.flags(player.switched()).queen_endangered {
                Some(player)
            } else if meta.flags(player).queen_endangered {
                Some(player.switched())
            } else {
                None
            }
        }
        Ordering::Greater => Some(player.switched()),
    }
}

fn rate_queen_situation(
    data: &HiveGameState,
    meta: &MetaData,
    player: Player,
    enemy_beetle_bonus: RatingType,
    less_endangered: Option<Player>,
    enemies_reaching_queen: u32,
) -> RatingType {
    let mut val = 6;

    const QUEEN_VAL: [RatingType; 6] = [0, 0, 20, 45, 80, 125];
    let is_less_endangered = less_endangered == Some(player);
    let mut can_move = false;
    let mut num_neighbors = 0_u32;
    let mut num_friendly_movable = 0;
    if let Some(index) = meta.q_pos(player) {
        let pos = data.board().get_field_unchecked(index);
        can_move = pos.content().len() == 1 && data.is_movable(pos, false);
        for field in pos.neighbors() {
            if let Some(piece) = field.content().top() {
                num_neighbors += 1;
                if field.content().len() == 1 && piece.player == player {
                    if let PieceType::Grasshopper | PieceType::Ladybug = piece.p_type {
                        // bonus for defensive pieces
                        // TODO: mosquito, beetle, pillbug?
                        val += 2;
                    }
                    if data.is_movable(field, false) {
                        num_friendly_movable += 1;
                    }
                }
            }
        }
    }

    let enough = num_neighbors + enemies_reaching_queen >= 6;
    let exactly_enough = num_neighbors + enemies_reaching_queen == 6;
    val -= QUEEN_VAL[num_neighbors as usize];
    if val < 0 {
        if num_friendly_movable > 0 && player == data.player() {
            val += 18;
        } else if num_friendly_movable > 0 && player != data.player() {
            val += 12;
        }
        if num_friendly_movable > 1 {
            val += 6;
        }
    }
    val -= enemy_beetle_bonus;
    if meta.flags(player).queen_endangered {
        val -= 10;
    }
    if meta.flags(player).queen_is_ant_reachable
        && !enough
        && less_endangered != Some(player.switched())
    {
        val -= 4;
    } else if meta.flags(player).queen_is_ant_reachable {
        val -= 8;
    }
    if val >= 0 {
        return val;
    }

    // TODO: still unclear whether this is good
    if !enough && less_endangered != Some(player.switched()) {
        val * 3 / 5
    } else if can_move && (player == data.player() || !meta.flags(player).queen_endangered) {
        val * 2 / 3
    } else if (can_move || exactly_enough) && is_less_endangered {
        val * 3 / 4
    } else if is_less_endangered || !enough {
        val * 4 / 5
    } else if can_move {
        val * 5 / 6
    } else if exactly_enough {
        val * 9 / 10
    } else {
        val
    }
}

pub fn rate_game_state(data: &HiveGameState, weights: &RatingWeights, player: usize) -> RatingType {
    let player = Player::from(player);
    let enemy = player.switched();
    if let Some(result) = data.result() {
        return match (result, player) {
            (HiveResult::Draw, _) => 0,
            (HiveResult::WhiteWin, Player::White) => 1000,
            (HiveResult::BlackWin, Player::Black) => 1000,
            _ => -1000,
        };
    }

    assert_eq!(data.player(), player);
    let mut meta = calculate_metadata(data);

    let my_remaining = rate_remaining_pieces(data, player);
    let enemy_remaining = rate_remaining_pieces(data, enemy);
    let (
        my_movability,
        enemy_movability,
        my_beetle_bonus,
        enemy_beetle_bonus,
        mut my_reach_queen,
        mut enemy_reach_queen,
    ) = rate_piece_movability(data, &mut meta);
    my_reach_queen += data.total_num_pieces(player);
    enemy_reach_queen += data.total_num_pieces(enemy);

    let less_endangered = determine_less_endangered(data, &meta);
    let my_queen = rate_queen_situation(
        data,
        &meta,
        player,
        enemy_beetle_bonus,
        less_endangered,
        enemy_reach_queen,
    );
    let enemy_queen = rate_queen_situation(
        data,
        &meta,
        enemy,
        my_beetle_bonus,
        less_endangered,
        my_reach_queen,
    );
    let pieces =
        weights.own_pieces * my_remaining as f64 - weights.enemy_pieces * enemy_remaining as f64;
    let movability = weights.own_movability * my_movability as f64
        - weights.enemy_movability * enemy_movability as f64;
    let queen = weights.own_queen * my_queen as f64 - weights.enemy_queen * enemy_queen as f64;
    (pieces + movability + queen).round() as RatingType
}

pub fn print_and_compare_rating(data: &HiveGameState, expected: Option<[RatingType; 6]>) {
    let player = data.player();
    let enemy = player.switched();

    assert_eq!(data.player(), player);
    let mut meta = calculate_metadata(data);

    let my_remaining = rate_remaining_pieces(data, player);
    let enemy_remaining = rate_remaining_pieces(data, enemy);
    let (
        my_movability,
        enemy_movability,
        my_beetle_bonus,
        enemy_beetle_bonus,
        mut my_reach_queen,
        mut enemy_reach_queen,
    ) = rate_piece_movability(data, &mut meta);
    my_reach_queen += data.total_num_pieces(player);
    enemy_reach_queen += data.total_num_pieces(enemy);

    let less_endangered = determine_less_endangered(data, &meta);
    let my_queen = rate_queen_situation(
        data,
        &meta,
        player,
        enemy_beetle_bonus,
        less_endangered,
        enemy_reach_queen,
    );
    let enemy_queen = rate_queen_situation(
        data,
        &meta,
        enemy,
        my_beetle_bonus,
        less_endangered,
        my_reach_queen,
    );
    println!("       Current player -   Enemy player");
    println!("Rem.   {:<15}-{:>15}", my_remaining, enemy_remaining);
    println!("Mov.   {:<15}-{:>15}", my_movability, enemy_movability);
    println!("Queen  {:<15}-{:>15}", my_queen, enemy_queen);

    if let Some(expected) = expected {
        assert_eq!(my_remaining, expected[0]);
        assert_eq!(enemy_remaining, expected[1]);
        assert_eq!(my_movability, expected[2]);
        assert_eq!(enemy_movability, expected[3]);
        assert_eq!(my_queen, expected[4]);
        assert_eq!(enemy_queen, expected[5]);
    }
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;

    use hivetuilib_board::{
        open_board::OpenIndex, structures::directions::HexaDirection, BoardToMap,
    };

    use crate::{
        ai::rate_game_state::Flags,
        display::print_annotated_board,
        pieces::{PieceType, Player},
        state::HiveGameState,
    };

    use super::*;

    #[test]
    fn meta_data_test() {
        let mut pieces = BTreeMap::new();
        pieces.insert(PieceType::Queen, 1);
        pieces.insert(PieceType::Ant, 3);
        pieces.insert(PieceType::Spider, 1);
        pieces.insert(PieceType::Grasshopper, 1);
        pieces.insert(PieceType::Beetle, 1);

        let mut state = HiveGameState::new(pieces);
        let zero = OpenIndex::from((0, 0));
        let up = OpenIndex::from((0, 1));
        state.place_piece(PieceType::Queen, zero);
        state.place_piece(PieceType::Spider, up);
        state.place_piece(PieceType::Spider, zero + HexaDirection::UpRight);
        state.place_piece(PieceType::Queen, up + HexaDirection::UpLeft);
        state.place_piece(PieceType::Beetle, up + HexaDirection::Up);
        state.place_piece(PieceType::Grasshopper, zero + HexaDirection::UpLeft);
        state.place_piece(PieceType::Ant, zero + HexaDirection::DownLeft);
        state.place_piece(PieceType::Ant, zero + HexaDirection::Down);
        state.move_piece(up + HexaDirection::Up, up + HexaDirection::UpLeft, false);
        state.place_piece(
            PieceType::Ant,
            zero + HexaDirection::UpRight + HexaDirection::DownRight,
        );
        state.place_piece(
            PieceType::Grasshopper,
            zero + HexaDirection::DownRight + HexaDirection::DownRight,
        );
        state.place_piece(
            PieceType::Ant,
            zero + HexaDirection::DownRight + HexaDirection::DownRight + HexaDirection::Down,
        );

        print_annotated_board::<usize>(&state, &state.board().get_index_map(), false, None, None);

        let meta = calculate_metadata(&state);
        assert_eq!(
            &meta.queen_pos,
            &[Some(zero), Some(up + HexaDirection::UpLeft)]
        );
        assert_eq!(&meta.queen_neighbors, &[5, 2]);
        assert_eq!(
            meta.flags(Player::White),
            &Flags {
                queen_endangered: false,
                queen_is_ant_reachable: false,
                queen_has_beetle_on_top: false,
                has_free_ant: true,
                has_blocking_ant: false,
            }
        );
        assert_eq!(
            meta.flags(Player::Black),
            &Flags {
                queen_endangered: true,
                queen_is_ant_reachable: true,
                queen_has_beetle_on_top: true,
                has_free_ant: false,
                has_blocking_ant: true,
            }
        );
    }

    #[test]
    fn rating_test() {
        let mut pieces = BTreeMap::new();
        pieces.insert(PieceType::Queen, 1);
        pieces.insert(PieceType::Ant, 3);
        pieces.insert(PieceType::Spider, 1);
        pieces.insert(PieceType::Grasshopper, 1);

        let mut state = HiveGameState::new(pieces);
        let zero = OpenIndex::from((0, 0));
        let up = OpenIndex::from((0, 1));
        state.place_piece(PieceType::Grasshopper, zero);
        state.place_piece(PieceType::Grasshopper, up);
        state.place_piece(PieceType::Queen, zero + HexaDirection::Down);
        state.place_piece(PieceType::Queen, up + HexaDirection::Up);
        state.place_piece(PieceType::Ant, zero + HexaDirection::DownLeft);
        state.place_piece(PieceType::Ant, zero + HexaDirection::DownRight);
        state.place_piece(
            PieceType::Spider,
            zero + HexaDirection::DownRight + HexaDirection::UpRight,
        );
        state.place_piece(
            PieceType::Ant,
            up + HexaDirection::UpRight + HexaDirection::Up,
        );
        state.move_piece(
            zero + HexaDirection::DownLeft,
            up + HexaDirection::UpRight + HexaDirection::UpRight,
            false,
        );
        state.place_piece(PieceType::Spider, zero + HexaDirection::UpLeft);

        print_annotated_board::<usize>(&state, &state.board().get_index_map(), false, None, None);
        // note: white has not enough pieces for surrounding black
        print_and_compare_rating(&state, Some([20, 10, 45, 40, -20, -9]));
    }

    #[test]
    fn rating_test_complex() {
        let mut pieces = BTreeMap::new();
        pieces.insert(PieceType::Queen, 1);
        pieces.insert(PieceType::Ant, 2);
        pieces.insert(PieceType::Spider, 2);
        pieces.insert(PieceType::Grasshopper, 2);
        pieces.insert(PieceType::Beetle, 2);

        let mut state = HiveGameState::new(pieces);
        let zero = OpenIndex::from((0, 0));
        let up = OpenIndex::from((0, 1));
        state.place_piece(PieceType::Grasshopper, zero);
        state.place_piece(PieceType::Grasshopper, up);
        state.place_piece(PieceType::Queen, zero + HexaDirection::Down);
        state.place_piece(PieceType::Queen, up + HexaDirection::UpRight);
        state.place_piece(PieceType::Ant, zero + HexaDirection::DownLeft);
        state.place_piece(PieceType::Ant, up + HexaDirection::Up);
        state.place_piece(
            PieceType::Grasshopper,
            up + HexaDirection::UpRight + HexaDirection::UpRight,
        );
        state.place_piece(
            PieceType::Spider,
            up + HexaDirection::Up + HexaDirection::UpLeft,
        );
        state.move_piece(zero, zero, false);
        state.place_piece(
            PieceType::Ant,
            zero + HexaDirection::Down + HexaDirection::DownRight,
        );
        state.place_piece(PieceType::Beetle, zero + HexaDirection::DownRight);
        state.place_piece(
            PieceType::Beetle,
            zero + HexaDirection::Down + HexaDirection::DownRight + HexaDirection::DownRight,
        );
        state.place_piece(
            PieceType::Spider,
            up + HexaDirection::UpRight + HexaDirection::UpRight + HexaDirection::DownRight,
        );
        state.move_piece(
            zero + HexaDirection::Down + HexaDirection::DownRight + HexaDirection::DownRight,
            zero + HexaDirection::Down + HexaDirection::DownRight,
            false,
        );

        print_annotated_board::<usize>(&state, &state.board().get_index_map(), false, None, None);
        // note that white queen is movable, but the beetle is only half movable
        print_and_compare_rating(&state, Some([21, 16, 63, 50, -47, -44]));
    }
}
