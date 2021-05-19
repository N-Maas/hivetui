use tgp_ai::RatingType;
use tgp_board::{open_board::OpenIndex, prelude::*};

use crate::{
    pieces::{PieceType, Player},
    state::{HiveGameState, HiveResult},
};

use super::distance;

#[derive(Debug, Clone)]
struct MetaData {
    queen_pos: [Option<OpenIndex>; 2],
    queen_endangered: [bool; 2],
}

impl MetaData {
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
}

// TODO: is queen ant-reachable?
fn calculate_metadata(data: &HiveGameState) -> MetaData {
    let mut meta_data = MetaData {
        queen_pos: [None; 2],
        queen_endangered: [false; 2],
    };

    for field in data.board().iter_fields() {
        if !field.is_empty() {
            let queen = field.content().bottom().unwrap();
            if queen.p_type == PieceType::Queen {
                meta_data.queen_pos[usize::from(queen.player)] = Some(field.index());
            }
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
        can_move = pos.content().len() == 1 && data.can_move(pos, false);
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
