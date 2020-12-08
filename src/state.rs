use std::collections::BTreeMap;

use tgp::board::{
    directions::{DirectionEnumerable, HexaDirection},
    hypothetical::Hypothetical,
    open_board::OpenBoard,
    open_board::OpenIndex,
    search::Searchable,
    structures::OffsetStructure,
    Board, Field,
};

use crate::pieces::{Piece, PieceType, Player};

pub type HiveBoard = OpenBoard<Vec<Piece>, OffsetStructure<OpenIndex, HexaDirection>>;

pub struct HiveGameState {
    current_player: Player,
    white_pieces: BTreeMap<PieceType, u32>,
    black_pieces: BTreeMap<PieceType, u32>,
    board: HiveBoard,
}

impl HiveGameState {
    pub fn pieces(&self) -> &BTreeMap<PieceType, u32> {
        match self.current_player {
            Player::White => &self.white_pieces,
            Player::Black => &self.black_pieces,
        }
    }

    pub fn pieces_mut(&mut self) -> &mut BTreeMap<PieceType, u32> {
        match self.current_player {
            Player::White => &mut self.white_pieces,
            Player::Black => &mut self.black_pieces,
        }
    }

    pub fn place_piece(&mut self, p_type: PieceType, target: OpenIndex) {
        let pieces = self.pieces_mut();
        assert!(
            pieces[&p_type] > 0 && self.board.get_field(target).map_or(false, |f| f.is_empty())
        );
    }

    fn add_new_neighbors(&mut self, target: OpenIndex) {
        for d in HexaDirection::enumerate_all() {
            let field = self
                .board
                .get_field(target)
                .expect(&format!("Invalid index: {:?}", target));
            if !field.has_next(d) {
                self.board.insert(target + d, Vec::new());
            }
        }
    }

    fn remove_old_neighbors(&mut self, target: OpenIndex) {
        let field = self.board.get_field_unchecked(target);
        let indizes = field.neighbors().map(|f| f.index()).collect::<Vec<_>>();
        for i in indizes {
            let adjacent_to_occupied_field = self
                .board
                .get_field(i)
                // unwrap: safe because only neighbors of field are contained
                .unwrap()
                .neighbors()
                .any(|f| !f.is_empty());
            if !adjacent_to_occupied_field {
                self.board.delete(i);
            }
        }
    }
}

fn move_violates_ohr(field: Field<HiveBoard>) -> bool {
    assert!(!field.is_empty());
    // scan surrounding fields whether it might be possible that the OHR (one hive rule) might be violated
    // additionally, collect some non-empty neighbors of potentially different components
    let mut prev_empty = false;
    let neighbor_cycle = field.neighbors().chain(field.neighbors().next());
    let mut occupied_fields = Vec::new();
    for f in neighbor_cycle {
        if prev_empty && !f.is_empty() {
            occupied_fields.push(f);
        }
        prev_empty = f.is_empty();
    }
    if occupied_fields.len() < 2 {
        return false;
    }

    // determine the connectivity component of one of the occupied field - it must contain all pieces
    // unwrap: correct due to previous length check
    let mut hypothetical = Hypothetical::from_field(field);
    hypothetical.clear_field(field);
    let mut component = occupied_fields.first().unwrap().search();
    component.grow_repeated(|f| !f.is_empty());
    occupied_fields.into_iter().any(|f| !component.contains(f))
}
