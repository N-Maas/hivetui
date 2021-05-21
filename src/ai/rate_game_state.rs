use std::{cmp::Ordering, iter};

use tgp_ai::RatingType;
use tgp_board::{open_board::OpenIndex, prelude::*};

use crate::{
    pieces::{feasible_steps_plain, grasshopper_moves, Piece, PieceType, Player},
    state::{HiveBoard, HiveGameState, HiveResult},
};

use super::{blocks, distance, would_block};

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

// TODO: is queen ant-reachable?
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
                        iter::empty::<OpenIndex>().collect()
                    } else {
                        feasible_steps_plain(f).collect()
                    }
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
    let ant_rating = 10 * data.remaining_pieces(player, PieceType::Ant);
    let spider_rating = 5 * data.remaining_pieces(player, PieceType::Spider);
    let grasshopper_rating = 5 * data.remaining_pieces(player, PieceType::Grasshopper);
    let beetle_rating = 5 * data.remaining_pieces(player, PieceType::Beetle);
    (ant_rating + spider_rating + grasshopper_rating + beetle_rating) as RatingType
}

fn rate_piece_movability(data: &HiveGameState, meta: &mut MetaData) -> (RatingType, RatingType) {
    let mut rating = [0; 2];
    for field in data.board().iter_fields().filter(|f| !f.is_empty()) {
        let &piece = field.content().top().unwrap();
        if field.content().len() == 1 && data.is_movable(field, false) {
            rating[usize::from(piece.player)] +=
                single_piece_rating(data, meta, piece, field, MovabilityType::Movable);
        } else if field.content().len() == 1 && !data.is_movable(field, false) {
            // is this blocked by only one adjacent piece?
            let mut value =
                single_piece_rating(data, meta, piece, field, MovabilityType::Unmovable);
            for n in field.neighbors().filter(|f| !f.is_empty()) {
                if data.is_movable(n, false) && blocks(n, field) {
                    let movability = MovabilityType::Blocked(n.content().top().unwrap().player);
                    value = RatingType::max(
                        value,
                        single_piece_rating(data, meta, piece, field, movability),
                    );
                }
            }
            rating[usize::from(piece.player)] += value;
        } else if field.content().len() > 1 {
            assert!(data.is_movable(field, false));
            match field.content().pieces() {
                [inner @ .., next, beetle] => {
                    for &piece in inner {
                        let movability = MovabilityType::Unmovable;
                        rating[usize::from(piece.player)] +=
                            single_piece_rating(data, meta, piece, field, movability);
                    }
                    let mov_type = if blocks(field, field) {
                        MovabilityType::Blocked(beetle.player)
                    } else {
                        MovabilityType::Unmovable
                    };
                    rating[usize::from(next.player)] +=
                        single_piece_rating(data, meta, piece, field, mov_type);
                    rating[usize::from(beetle.player)] +=
                        single_piece_rating(data, meta, *beetle, field, MovabilityType::Movable);
                }
                _ => unreachable!(),
            }
        }
    }
    (
        rating[usize::from(data.player())],
        rating[usize::from(data.player().switched())],
    )
}

fn single_piece_rating(
    data: &HiveGameState,
    meta: &mut MetaData,
    piece: Piece,
    field: Field<HiveBoard>,
    movability: MovabilityType,
) -> RatingType {
    let base_rating = match piece.p_type {
        PieceType::Queen => 0,
        PieceType::Ant => match movability {
            MovabilityType::Movable | MovabilityType::Blocked(_) => {
                if meta.flags(piece.player.switched()).queen_is_ant_reachable {
                    24
                } else {
                    16
                }
            }
            MovabilityType::Unmovable => 5,
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
                    } else {
                        for n in f.neighbors() {
                            if !n.is_empty() && data.is_movable(n, false) && would_block(f, n) {
                                can_block = true;
                                break;
                            }
                        }
                    }
                }
                if reaches_queen {
                    meta.flags_mut(piece.player.switched()).queen_endangered = true;
                    18
                } else if can_block && piece.player == data.player() {
                    15
                } else {
                    10
                }
            }
            MovabilityType::Blocked(_) => 10,
            MovabilityType::Unmovable => 2,
        },
        PieceType::Beetle => match movability {
            // TODO!!! (sit on queen bonus e.g.)
            MovabilityType::Movable => 16,
            MovabilityType::Blocked(_) => 16,
            // a beetle near the queen is still a danger
            MovabilityType::Unmovable => 3,
        },
    };

    match movability {
        MovabilityType::Blocked(blocking_player) => {
            if blocking_player == piece.player && blocking_player == data.player() {
                base_rating * 3 / 4
            } else {
                base_rating / 2
            }
        }
        _ => base_rating,
    }
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
    is_less_endangered: bool,
) -> RatingType {
    const QUEEN_VAL: [RatingType; 6] = [0, 0, 25, 50, 80, 115];
    let mut can_move = false;
    let mut num_neighbors = 0;
    let mut num_friendly_movable = 0;
    if let Some(index) = meta.q_pos(player) {
        let pos = data.board().get_field_unchecked(index);
        can_move = pos.content().len() == 1 && data.is_movable(pos, false);
        for field in pos.neighbors() {
            if let Some(piece) = field.content().top() {
                num_neighbors += 1;
                if field.content().len() == 1 && piece.player == player {
                    num_friendly_movable += 1;
                }
            }
        }
    }

    let val = 15 * num_friendly_movable - QUEEN_VAL[num_neighbors];

    if can_move {
        (val * 3 / 5) + 5
    } else if is_less_endangered {
        val * 4 / 5
    } else {
        val
    }
}

pub fn rate_game_state(data: &HiveGameState, player: usize) -> RatingType {
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
    let (my_movability, enemy_movability) = rate_piece_movability(data, &mut meta);
    let less_endangered = determine_less_endangered(data, &meta);
    let my_queen = rate_queen_situation(data, &meta, player, less_endangered == Some(player));
    let enemy_queen = rate_queen_situation(data, &meta, enemy, less_endangered == Some(enemy));
    my_remaining - enemy_remaining + my_movability - enemy_movability + my_queen - enemy_queen
}

pub fn print_and_compare_rating(data: &HiveGameState, expected: Option<&[RatingType; 6]>) {
    let player = Player::from(data.player());
    let enemy = player.switched();

    assert_eq!(data.player(), player);
    let mut meta = calculate_metadata(data);

    let my_remaining = rate_remaining_pieces(data, player);
    let enemy_remaining = rate_remaining_pieces(data, enemy);
    let (my_movability, enemy_movability) = rate_piece_movability(data, &mut meta);
    let less_endangered = determine_less_endangered(data, &meta);
    let my_queen = rate_queen_situation(data, &meta, player, less_endangered == Some(player));
    let enemy_queen = rate_queen_situation(data, &meta, enemy, less_endangered == Some(enemy));
    println!("           Current player -   Enemy player");
    println!("Remaining  {:<15}-{:>15}", my_remaining, enemy_remaining);
    println!("Movability {:<15}-{:>15}", my_movability, enemy_movability);
    println!("Queen      {:<15}-{:>15}", my_queen, enemy_queen);

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

    use tgp_board::{open_board::OpenIndex, structures::directions::HexaDirection, BoardToMap};

    use crate::{ai::rate_game_state::Flags, display::print_annotated_board, pieces::{PieceType, Player}, state::HiveGameState};

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

        print_annotated_board::<usize>(&state, &state.board().get_index_map(), false);

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
}
