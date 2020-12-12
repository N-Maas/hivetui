use std::collections::BTreeMap;

use tgp::board::{
    directions::{DirectionEnumerable, HexaDirection},
    hypothetical::Hypothetical,
    open_board::{OpenBoard, OpenIndex},
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
    white_pieces_on_board: u8,
    black_pieces_on_board: u8,
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
            white_pieces_on_board: 0,
            black_pieces_on_board: 0,
        }
    }

    pub fn board(&self) -> &HiveBoard {
        &self.board
    }

    pub fn pieces(&self) -> (&BTreeMap<PieceType, u32>, u8) {
        match self.current_player {
            Player::White => (&self.white_pieces, self.white_pieces_on_board),
            Player::Black => (&self.black_pieces, self.black_pieces_on_board),
        }
    }

    pub fn get_all_movables(&self) -> impl Iterator<Item = Field<HiveBoard>> {
        let moves_enabled = self.pieces().0[&PieceType::Queen] == 0;
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
            if self.white_pieces_on_board + self.black_pieces_on_board == 1 {
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

    pub fn get_available_pieces(&self) -> Vec<PieceType> {
        let (pieces, num) = self.pieces();
        if pieces[&PieceType::Queen] > 0 && num == 3 {
            return vec![PieceType::Queen];
        }
        pieces
            .iter()
            .filter(|&(_, count)| *count > 0)
            .map(|(p, _)| *p)
            .collect()
    }

    pub fn place_piece(&mut self, p_type: PieceType, target: OpenIndex) -> Option<&'static str> {
        assert!(
            self.pieces().0[&p_type] > 0
                && self.board.get_field(target).map_or(false, |f| f.is_empty())
        );
        let pieces = self.pieces_mut();
        pieces.entry(p_type).and_modify(|count| *count -= 1);
        self.board[target].push(Piece {
            player: self.current_player,
            p_type,
        });
        self.add_new_neighbors(target);
        match self.current_player {
            Player::White => self.white_pieces_on_board += 1,
            Player::Black => self.black_pieces_on_board += 1,
        }
        self.current_player.switch();
        self.test_game_finished(target)
    }

    pub fn move_piece(&mut self, from: OpenIndex, to: OpenIndex) -> Option<&'static str> {
        assert!(self.board.get_field(from).map_or(false, |f| !f.is_empty()));
        let piece = self.board[from]
            .pop()
            .expect(&format!("Piece was not present at: {:?}", from));
        self.board[to].push(piece);
        self.remove_old_neighbors(from);
        self.add_new_neighbors(to);
        self.current_player.switch();
        self.test_game_finished(to)
    }

    pub fn test_game_finished(&self, target: OpenIndex) -> Option<&'static str> {
        let field = self.board.get_field_unchecked(target);
        let white_win = self.is_adjacent_to_surrounded_queeen(field, Player::Black);
        let black_win = self.is_adjacent_to_surrounded_queeen(field, Player::White);
        match (white_win, black_win) {
            (true, true) => Some("The game ended with a draw!"),
            (true, false) => Some("The white player has won!"),
            (false, true) => Some("The black player has won!"),
            _ => None,
        }
    }

    fn is_adjacent_to_surrounded_queeen(&self, field: Field<HiveBoard>, player: Player) -> bool {
        let queen = field.neighbors().find(|f| {
            f.content().first()
                == Some(&Piece {
                    player,
                    p_type: PieceType::Queen,
                })
        });
        if let Some(f) = queen {
            !f.neighbors().any(|n| n.is_empty())
        } else {
            false
        }
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
            occupied_fields.push(f.index());
        }
        prev_empty = f.is_empty();
    }
    if occupied_fields.len() < 2 {
        return false;
    }

    // determine the connectivity component of one of the occupied field - it must contain all pieces
    // unwrap: correct due to previous length check
    let mut hypothetical = Hypothetical::from_field(field);
    let mut modified_content = field.content().clone();
    modified_content.pop();
    hypothetical.replace(field.index(), modified_content);
    let mut component = hypothetical
        .get_field_unchecked(*occupied_fields.first().unwrap())
        .search();
    component.grow_repeated(|f| !f.is_empty());
    occupied_fields.into_iter().any(|f| !component.contains(f))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn first_moves_test() {
        let mut pieces = BTreeMap::new();
        pieces.insert(PieceType::Queen, 1);
        pieces.insert(PieceType::Beetle, 1);
        let mut state = HiveGameState::new(pieces);
        assert_eq!(
            state.get_all_placement_targets().next().unwrap().index(),
            OpenIndex::from((0, 0))
        );
        state.place_piece(PieceType::Queen, OpenIndex::from((0, 0)));
        assert_eq!(state.board.size(), 7);
        assert_eq!(
            state.get_all_placement_targets().next().unwrap().index(),
            OpenIndex::from((0, 1))
        );
        state.place_piece(PieceType::Queen, OpenIndex::from((0, 1)));
        assert_eq!(state.board.size(), 10);
        assert_eq!(
            state.get_all_movables().next().unwrap().index(),
            OpenIndex::from((0, 0))
        );
        let moves = state
            .get_possible_moves(state.board.get_field_unchecked(OpenIndex::from((0, 0))))
            .map(|f| f.index())
            .collect::<Vec<_>>();
        assert_eq!(moves.len(), 2);
        assert!(moves.contains(&(OpenIndex::from((0, 0)) + HexaDirection::UpRight)));
        assert!(moves.contains(&(OpenIndex::from((0, 0)) + HexaDirection::UpRight)));
        state.move_piece(
            OpenIndex::from((0, 0)),
            OpenIndex::from((0, 0)) + HexaDirection::UpRight,
        );
        assert_eq!(state.board.size(), 10);
        state.place_piece(PieceType::Beetle, OpenIndex::from((0, 2)));
        assert!(move_violates_ohr(
            state.board.get_field_unchecked(OpenIndex::from((0, 1)))
        ));
    }
}
