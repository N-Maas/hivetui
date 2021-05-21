use std::iter;

use tgp_ai::RatingType;
use tgp_board::{open_board::OpenIndex, prelude::*};

use crate::{
    pieces::{feasible_steps_plain, PieceType, Player},
    state::{HiveGameState, HiveResult},
};

use super::{blocks, distance};

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
        (val * 4 / 5) + 5
    } else {
        val
    }
}

pub fn rate_game_state(data: &HiveGameState, player: usize) -> RatingType {
    let player = Player::from(player);
    if let Some(result) = data.result() {
        return match (result, player) {
            (HiveResult::Draw, _) => 0,
            (HiveResult::WhiteWin, Player::White) => 1000,
            (HiveResult::BlackWin, Player::Black) => 1000,
            _ => -1000,
        };
    }

    assert_eq!(data.player(), player);
    let meta = calculate_metadata(data);

    let my_remaining = rate_remaining_pieces(data, player);
    let enemy_remaining = rate_remaining_pieces(data, player.switched());
    // TODO
    let my_queen = rate_queen_situation(data, &meta, player, false);
    let enemy_queen = rate_queen_situation(data, &meta, player.switched(), false);
    my_remaining - enemy_remaining + my_queen - enemy_queen
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;

    use tgp_board::{open_board::OpenIndex, structures::directions::HexaDirection, BoardToMap};

    use crate::{
        ai::rate_game_state::Flags,
        display::print_annotated_board,
        pieces::{PieceType, Player},
        state::HiveGameState,
    };

    use super::calculate_metadata;

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
