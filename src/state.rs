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
    pieces_on_board: u8,
}

impl HiveGameState {
    pub fn new(pieces: BTreeMap<PieceType, u32>) -> Self {
        let mut board = HiveBoard::new(OffsetStructure::new());
        board.extend_and_insert(OpenIndex::from((0, 0)), Vec::new());
        Self {
            current_player: Player::White,
            white_pieces: pieces.clone(),
            black_pieces: pieces,
            board,
            pieces_on_board: 0,
        }
    }

    pub fn pieces(&self) -> &BTreeMap<PieceType, u32> {
        match self.current_player {
            Player::White => &self.white_pieces,
            Player::Black => &self.black_pieces,
        }
    }

    pub fn get_all_movables(&self) -> impl Iterator<Item = Field<HiveBoard>> {
        let moves_enabled = self.pieces()[&PieceType::Queen] == 0;
        self.board.iter_fields().filter(move |f| {
            let valid_field = f
                .content()
                .last()
                .map_or(false, |piece| piece.player == self.current_player);
            moves_enabled && valid_field && can_move(*f)
        })
    }

    pub fn get_all_placement_targets(&self) -> impl Iterator<Item = Field<HiveBoard>> {
        let init_pos = OpenIndex::from((0, 0)) + HexaDirection::Up;
        self.board.iter_fields().filter(move |f| {
            if self.pieces_on_board == 1 {
                f.index() == init_pos
            } else {
                f.is_empty()
                    && !f.neighbors().any(|n| {
                        n.content()
                            .last()
                            .map_or(false, |piece| piece.player != self.current_player)
                    })
            }
        })
    }

    pub fn get_possible_moves<'a>(
        &self,
        field: Field<'a, HiveBoard>,
    ) -> impl Iterator<Item = Field<'a, HiveBoard>> {
        assert!(!field.is_empty() && can_move(field));
        // unwrap: correct because of assertion
        let Piece { p_type, .. } = field.content().last().unwrap();
        p_type.get_moves(field).into_iter()
    }

    pub fn get_available_pieces(&self) -> impl Iterator<Item = PieceType> + '_ {
        self.pieces()
            .iter()
            .filter(|&(_, count)| *count > 0)
            .map(|(p, _)| *p)
    }

    pub fn place_piece(&mut self, p_type: PieceType, target: OpenIndex) {
        assert!(
            self.pieces()[&p_type] > 0
                && self.board.get_field(target).map_or(false, |f| f.is_empty())
        );
        let pieces = self.pieces_mut();
        pieces.entry(p_type).and_modify(|count| *count -= 1);
        self.board[target].push(Piece {
            player: self.current_player,
            p_type,
        });
        self.add_new_neighbors(target);
        self.pieces_on_board += 1;
        self.current_player.switch();
    }

    pub fn move_piece(&mut self, from: OpenIndex, to: OpenIndex) {
        assert!(self.board.get_field(from).map_or(false, |f| !f.is_empty()));
        let piece = self.board[from]
            .pop()
            .expect(&format!("Piece was not present at: {:?}", from));
        self.board[to].push(piece);
        self.remove_old_neighbors(from);
        self.add_new_neighbors(to);
        self.current_player.switch();
    }

    fn pieces_mut(&mut self) -> &mut BTreeMap<PieceType, u32> {
        match self.current_player {
            Player::White => &mut self.white_pieces,
            Player::Black => &mut self.black_pieces,
        }
    }

    fn add_new_neighbors(&mut self, target: OpenIndex) {
        for d in HexaDirection::enumerate_all() {
            let field = self
                .board
                .get_field(target)
                .expect(&format!("Invalid index: {:?}", target));
            if !field.has_next(d) {
                self.board.extend_and_insert(target + d, Vec::new());
            }
        }
    }

    fn remove_old_neighbors(&mut self, target: OpenIndex) {
        let field = self.board.get_field_unchecked(target);
        let indizes = field
            .neighbors()
            .filter(|f| f.is_empty())
            .map(|f| f.index())
            .collect::<Vec<_>>();
        for i in indizes {
            let adjacent_to_occupied_field = self
                .board
                .get_field(i)
                // unwrap: safe because only neighbors of field are contained
                .unwrap()
                .neighbors()
                .any(|f| !f.is_empty());
            if !adjacent_to_occupied_field {
                assert!(self.board.delete(i));
            }
        }
    }
}

fn can_move(field: Field<HiveBoard>) -> bool {
    assert!(!field.is_empty());
    let Piece { p_type, .. } = field.content().last().unwrap();
    p_type.is_movable(field) && !move_violates_ohr(field)
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
