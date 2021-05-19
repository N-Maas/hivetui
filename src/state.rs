use std::{
    collections::BTreeMap,
    fmt::{self, Display},
};

use tgp::{
    mapped_decision::MappedDecision, plain_decision::PlainDecision, vec_context::VecContext,
    Decision, GameData, Outcome, RevEffect,
};
use tgp_board::{
    hypothetical::Hypothetical,
    index_map::ArrayIndexMap,
    open_board::{OpenBoard, OpenIndex},
    prelude::*,
    structures::{
        directions::{DirectionEnumerable, HexaDirection},
        OffsetStructure,
    },
};

use crate::pieces::{Piece, PieceType, Player};

#[derive(Debug, Default, Clone)]
pub struct HiveContent {
    pub is_movable: bool,
    pieces: Vec<Piece>,
}

impl HiveContent {
    pub fn new() -> Self {
        Self {
            pieces: Vec::new(),
            is_movable: false,
        }
    }

    pub fn len(&self) -> usize {
        self.pieces.len()
    }

    pub fn is_empty(&self) -> bool {
        self.pieces.is_empty()
    }

    pub fn top(&self) -> Option<&Piece> {
        self.pieces.last()
    }

    pub fn bottom(&self) -> Option<&Piece> {
        self.pieces.first()
    }

    pub fn push(&mut self, piece: Piece) {
        self.pieces.push(piece);
    }

    pub fn pop(&mut self) -> Piece {
        self.pieces.pop().expect("Piece was not present!")
    }

    pub fn pieces(&self) -> &[Piece] {
        &self.pieces
    }
}

impl Emptyable for HiveContent {
    fn call_field_is_empty(&self) -> bool {
        self.pieces.is_empty()
    }
}

pub type HiveBoard = OpenBoard<HiveContent, OffsetStructure<OpenIndex, HexaDirection>>;

#[derive(Debug, Clone, PartialEq)]
pub enum HiveContext {
    BaseField(VecContext<OpenIndex>),
    TargetField(VecContext<OpenIndex, OpenIndex>),
    Piece(VecContext<(PieceType, u32), OpenIndex>),
    SkipPlayer,
}

impl From<VecContext<OpenIndex>> for HiveContext {
    fn from(vc: VecContext<OpenIndex>) -> Self {
        Self::BaseField(vc)
    }
}

impl From<VecContext<OpenIndex, OpenIndex>> for HiveContext {
    fn from(vc: VecContext<OpenIndex, OpenIndex>) -> Self {
        Self::TargetField(vc)
    }
}

impl From<VecContext<(PieceType, u32), OpenIndex>> for HiveContext {
    fn from(vc: VecContext<(PieceType, u32), OpenIndex>) -> Self {
        Self::Piece(vc)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HiveResult {
    Draw,
    WhiteWin,
    BlackWin,
}

impl Display for HiveResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Draw => write!(f, "The game ended with a draw!"),
            Self::WhiteWin => write!(f, "The white player has won!"),
            Self::BlackWin => write!(f, "The black player has won!"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct HiveGameState {
    current_player: Player,
    white_pieces: BTreeMap<PieceType, u32>,
    black_pieces: BTreeMap<PieceType, u32>,
    board: HiveBoard,
    white_pieces_on_board: u8,
    black_pieces_on_board: u8,
    result: Option<HiveResult>,
}

impl HiveGameState {
    pub fn new(pieces: BTreeMap<PieceType, u32>) -> Self {
        let mut board = HiveBoard::new(OffsetStructure::new());
        board.extend_and_insert(OpenIndex::from((0, 0)), HiveContent::new());
        Self {
            current_player: Player::White,
            white_pieces: pieces.clone(),
            black_pieces: pieces,
            board,
            white_pieces_on_board: 0,
            black_pieces_on_board: 0,
            result: None,
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

    pub fn remaining_pieces(&self, player: Player, p_type: PieceType) -> u32 {
        match player {
            Player::White => *self.white_pieces.get(&p_type).unwrap_or(&0),
            Player::Black => *self.black_pieces.get(&p_type).unwrap_or(&0),
        }
    }

    pub fn result(&self) -> Option<HiveResult> {
        self.result
    }

    pub fn player(&self) -> Player {
        self.current_player
    }

    pub fn player_usize(&self) -> usize {
        match self.current_player {
            Player::White => 0,
            Player::Black => 1,
        }
    }

    fn get_all_movables(&self) -> impl Iterator<Item = Field<HiveBoard>> {
        let moves_enabled = self.pieces().0[&PieceType::Queen] == 0;
        self.board.iter_fields().filter(move |f| {
            let valid_field = f
                .content()
                .top()
                .map_or(false, |piece| piece.player == self.current_player);
            moves_enabled && valid_field && self.is_movable(*f, true)
        })
    }

    fn get_all_placement_targets(&self) -> impl Iterator<Item = Field<HiveBoard>> {
        let init_pos = OpenIndex::from((0, 0)) + HexaDirection::Up;
        self.board.iter_fields().filter(move |f| {
            if self.white_pieces_on_board + self.black_pieces_on_board == 1 {
                f.index() == init_pos
            } else {
                f.is_empty()
                    && !f.neighbors().any(|n| {
                        n.content()
                            .top()
                            .map_or(false, |piece| piece.player != self.current_player)
                    })
            }
        })
    }

    pub fn place_piece(&mut self, p_type: PieceType, target: OpenIndex) {
        debug_assert!(
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
        self.result = self.test_game_finished(target);

        if self.black_pieces_on_board == 0 {
            // edge case for first piece
            self.board[target].is_movable = true;
        } else {
            let field = self.board.get_field(target).unwrap();
            self.board[target].is_movable = p_type.is_movable(field);
            let needs_update = self.check_movable_to(target);
            if needs_update {
                self.check_movability_for_all();
            }
        }
    }

    pub fn remove_piece(&mut self, target: OpenIndex) {
        debug_assert!(self
            .board
            .get_field(target)
            .map_or(false, |f| !f.is_empty()));
        self.current_player.switch();
        let piece = self.board[target].pop();
        let pieces = self.pieces_mut();
        pieces.entry(piece.p_type).and_modify(|count| *count += 1);
        self.remove_old_neighbors(target);
        match self.current_player {
            Player::White => self.white_pieces_on_board -= 1,
            Player::Black => self.black_pieces_on_board -= 1,
        }

        let needs_update = self.check_movable_from(target);
        if needs_update {
            self.check_movability_for_all();
        }
    }

    pub fn move_piece(&mut self, from: OpenIndex, to: OpenIndex, test_for_finished: bool) {
        assert!(self.board.get_field(from).map_or(false, |f| !f.is_empty()));
        let piece = self.board[from].pop();
        self.board[to].push(piece);
        self.board[to].is_movable = true;
        self.remove_old_neighbors(from);
        self.add_new_neighbors(to);
        self.current_player.switch();
        if test_for_finished {
            self.result = self.test_game_finished(to);
        }

        let needs_update = self.check_movable_from(from) || self.check_movable_to(to);
        if needs_update {
            self.check_movability_for_all();
        }
    }

    fn check_movable_from(&mut self, from: OpenIndex) -> bool {
        let content = &mut self.board[from];
        if content.is_empty() {
            content.is_movable = false;
        } else {
            self.board[from].is_movable =
                self.check_piece_is_movable(self.board.get_field(from).unwrap());
        }

        let field = self.board.get_field(from).unwrap();
        let neighbors = field
            .neighbors_by_direction()
            .filter(|(_, f)| !f.is_empty())
            .map(|(d, f)| (d, f.index()))
            .collect::<Vec<_>>();
        let in_a_row = neighbors.iter().all(|(d, _)| {
            !field.next(d.next_direction()).unwrap().is_empty()
                || !field.next(d.prev_direction()).unwrap().is_empty()
        });

        if in_a_row {
            for (_, i) in neighbors {
                let field = self.board.get_field(i).unwrap();
                self.board[i].is_movable = self.check_piece_is_movable(field);
            }
            false
        } else {
            true
        }
    }

    /// does not test the moved/placed piece itself
    fn check_movable_to(&mut self, to: OpenIndex) -> bool {
        if self.white_pieces_on_board + self.black_pieces_on_board <= 2 {
            return false;
        }

        let field = self.board.get_field(to).unwrap();
        let neighbors = field
            .neighbors_by_direction()
            .filter(|(_, f)| !f.is_empty())
            .map(|(d, f)| (d, f.index()))
            .collect::<Vec<_>>();
        let in_a_row = neighbors.iter().all(|(d, _)| {
            !field.next(d.next_direction()).unwrap().is_empty()
                || !field.next(d.prev_direction()).unwrap().is_empty()
        });

        if in_a_row {
            for &(_, i) in neighbors.iter() {
                let field = self.board.get_field(i).unwrap();
                if neighbors.len() <= 2 && self.board[i].is_movable {
                    let p_type = self.board[i].top().unwrap().p_type;
                    self.board[i].is_movable = p_type.is_movable(field);
                } else {
                    self.board[i].is_movable = self.check_piece_is_movable(field);
                }
            }
            false
        } else {
            true
        }
    }

    fn check_movability_for_all(&mut self) {
        for index in self.board.all_indices() {
            let field = self.board.get_field(index).unwrap();
            if !field.is_empty() {
                self.board[index].is_movable = self.check_piece_is_movable(field);
            }
        }
    }

    fn test_game_finished(&self, target: OpenIndex) -> Option<HiveResult> {
        let field = self.board.get_field_unchecked(target);
        let white_win = self.is_adjacent_to_surrounded_queeen(field, Player::Black);
        let black_win = self.is_adjacent_to_surrounded_queeen(field, Player::White);
        match (white_win, black_win) {
            (true, true) => Some(HiveResult::Draw),
            (true, false) => Some(HiveResult::WhiteWin),
            (false, true) => Some(HiveResult::BlackWin),
            _ => None,
        }
    }

    fn is_adjacent_to_surrounded_queeen(&self, field: Field<HiveBoard>, player: Player) -> bool {
        let queen = field.neighbors().find(|f| {
            f.content().bottom()
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

    pub fn is_movable(&self, field: Field<HiveBoard>, check_player: bool) -> bool {
        assert!(!field.is_empty());
        field.content().is_movable
            && (!check_player || field.content().top().unwrap().player == self.current_player)
    }

    fn check_piece_is_movable(&self, field: Field<HiveBoard>) -> bool {
        assert!(!field.is_empty());
        let Piece { p_type, player } = field.content().top().unwrap();
        p_type.is_movable(field) && !move_violates_ohr(field)
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
                .unwrap_or_else(|| panic!("Invalid index: {:?}", target));
            if !field.has_next(d) {
                self.board.extend_and_insert(target + d, HiveContent::new());
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

    fn create_placement_decision(&self, index: OpenIndex) -> Box<dyn tgp::Decision<Self>> {
        assert!(self.board.get_field_unchecked(index).is_empty());
        let mut dec = MappedDecision::with_inner(self.player_usize(), index);

        let (pieces, num) = self.pieces();
        if pieces[&PieceType::Queen] > 0 && num == 3 {
            dec.add_option((PieceType::Queen, 1));
        } else {
            for (&p_type, &count) in pieces {
                if count > 0 {
                    dec.add_option((p_type, count));
                }
            }
        }
        dec.spawn_by_rev_effect(|&index, &(p_type, _)| {
            (
                move |game_state: &mut HiveGameState| {
                    game_state.place_piece(p_type, index);
                    None
                },
                move |game_state: &mut HiveGameState| {
                    game_state.remove_piece(index);
                },
            )
        })
    }

    fn create_movement_decision(&self, index: OpenIndex) -> Box<dyn tgp::Decision<Self>> {
        let field = self.board.get_field_unchecked(index);
        assert!(self.is_movable(field, true));
        // unwrap: correct because of assertion
        let Piece { p_type, .. } = field.content().top().unwrap();
        let mut dec = MappedDecision::with_inner(self.player_usize(), index);

        for field in p_type.get_moves(field).into_iter() {
            dec.add_option(field.index());
        }
        dec.spawn_by_rev_effect(|&from, &to| {
            (
                move |game_state: &mut HiveGameState| {
                    game_state.move_piece(from, to, true);
                    None
                },
                move |game_state: &mut HiveGameState| {
                    game_state.move_piece(to, from, false);
                },
            )
        })
    }
}

impl GameData for HiveGameState {
    type Context = HiveContext;
    type EffectType = dyn RevEffect<Self>;

    fn next_decision(&self) -> Option<Box<dyn Decision<Self>>> {
        if self.result.is_some() {
            return None;
        }

        let mut base_dec = MappedDecision::new(self.player_usize());
        for field in self.get_all_placement_targets() {
            base_dec.add_option(field.index());
        }
        for field in self.get_all_movables() {
            base_dec.add_option(field.index());
        }
        if base_dec.is_empty() {
            let mut dec = PlainDecision::with_context(self.player_usize(), HiveContext::SkipPlayer);
            dec.add_rev_effect(
                |game_state: &mut HiveGameState| {
                    game_state.current_player.switch();
                    None
                },
                |game_state| game_state.current_player.switch(),
            );
            Some(Box::new(dec))
        } else {
            let result = base_dec.spawn_by_outcome(
                |game_state: &HiveGameState, _: &(), &index: &OpenIndex| {
                    let field = game_state.board.get_field_unchecked(index);
                    let child_dec = if field.is_empty() {
                        game_state.create_placement_decision(index)
                    } else {
                        game_state.create_movement_decision(index)
                    };
                    Outcome::FollowUp(child_dec)
                },
            );
            Some(result)
        }
    }
}

fn move_violates_ohr(field: Field<HiveBoard>) -> bool {
    assert!(!field.is_empty());
    if field.content().len() > 1 {
        return false;
    }

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
    let mut hypothetical =
        Hypothetical::with_index_map(field.board(), ArrayIndexMap::<_, _, 1>::new());
    hypothetical[field].pop();
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
        let moves = state.create_movement_decision(OpenIndex::from((0, 0)));
        assert_eq!(moves.option_count(), 2);
        match moves.context(&state) {
            HiveContext::TargetField(context) => {
                assert!(context.contains(&(OpenIndex::from((0, 0)) + HexaDirection::UpRight)));
                assert!(context.contains(&(OpenIndex::from((0, 0)) + HexaDirection::UpLeft)));
            }
            _ => {
                assert!(false)
            }
        }
        state.move_piece(
            OpenIndex::from((0, 0)),
            OpenIndex::from((0, 0)) + HexaDirection::UpRight,
            true,
        );
        assert_eq!(state.board.size(), 10);
        state.place_piece(PieceType::Beetle, OpenIndex::from((0, 2)));
        assert!(move_violates_ohr(
            state.board.get_field_unchecked(OpenIndex::from((0, 1)))
        ));
    }

    #[test]
    fn ant_spider_self_test() {
        let mut pieces = BTreeMap::new();
        pieces.insert(PieceType::Queen, 1);
        pieces.insert(PieceType::Ant, 1);
        pieces.insert(PieceType::Spider, 1);
        let mut state = HiveGameState::new(pieces);
        state.place_piece(PieceType::Queen, OpenIndex::from((0, 0)));
        state.place_piece(PieceType::Queen, OpenIndex::from((0, 1)));
        state.place_piece(PieceType::Ant, OpenIndex::from((0, -1)));
        state.current_player = Player::White;

        let moves = state.create_movement_decision(OpenIndex::from((0, -1)));
        assert_eq!(moves.option_count(), 7);
        match moves.context(&state) {
            HiveContext::TargetField(context) => {
                assert!(!context.contains(&(OpenIndex::from((0, -1)))));
            }
            _ => {
                assert!(false)
            }
        }

        state.remove_piece(OpenIndex::from((0, -1)));
        state.current_player = Player::White;
        state.place_piece(PieceType::Spider, OpenIndex::from((0, -1)));
        state.current_player = Player::White;
        let moves = state.create_movement_decision(OpenIndex::from((0, -1)));
        assert_eq!(moves.option_count(), 2);
        match moves.context(&state) {
            HiveContext::TargetField(context) => {
                assert!(!context.contains(&(OpenIndex::from((0, -1)))));
            }
            _ => {
                assert!(false)
            }
        }
    }

    // tests that a spider doesn't stand in the way of itself
    #[test]
    fn spider_hole_test() {
        let mut pieces = BTreeMap::new();
        pieces.insert(PieceType::Queen, 1);
        pieces.insert(PieceType::Spider, 5);

        let mut state = HiveGameState::new(pieces);
        let zero = OpenIndex::from((0, 0));
        let right = zero + HexaDirection::UpRight;
        let left = zero + HexaDirection::UpLeft;
        // place spider and queens
        state.place_piece(PieceType::Spider, zero);
        state.place_piece(PieceType::Queen, zero + HexaDirection::DownLeft);
        state.place_piece(PieceType::Queen, zero + HexaDirection::DownRight);
        // construct hole around spider
        state.place_piece(PieceType::Spider, right + HexaDirection::DownRight);
        state.place_piece(PieceType::Spider, right + HexaDirection::UpRight);
        state.place_piece(PieceType::Spider, right + HexaDirection::Up);
        state.place_piece(PieceType::Spider, left + HexaDirection::DownLeft);
        state.place_piece(PieceType::Spider, left + HexaDirection::UpLeft);
        state.place_piece(PieceType::Spider, left + HexaDirection::Up);
        state.place_piece(
            PieceType::Spider,
            zero + HexaDirection::Up + HexaDirection::Up,
        );

        state.current_player = Player::White;
        let moves = state.create_movement_decision(zero);
        assert_eq!(moves.option_count(), 2);
        match moves.context(&state) {
            HiveContext::TargetField(context) => {
                assert!(context.contains(&left));
                assert!(context.contains(&right));
            }
            _ => {
                assert!(false)
            }
        }
    }

    #[test]
    fn beetle_up_test() {
        let mut pieces = BTreeMap::new();
        pieces.insert(PieceType::Queen, 1);
        pieces.insert(PieceType::Beetle, 3);

        let mut state = HiveGameState::new(pieces);
        let zero = OpenIndex::from((0, 0));
        let up = zero + HexaDirection::Up;
        state.place_piece(PieceType::Queen, zero);
        state.place_piece(PieceType::Queen, up);
        state.place_piece(PieceType::Beetle, zero + HexaDirection::Down);
        state.place_piece(PieceType::Beetle, up + HexaDirection::Up);

        let moves = state.create_movement_decision(zero + HexaDirection::Down);
        assert_eq!(moves.option_count(), 3);
        match moves.context(&state) {
            HiveContext::TargetField(context) => {
                assert!(context.contains(&zero));
                assert!(context.contains(&(zero + HexaDirection::DownLeft)));
                assert!(context.contains(&(zero + HexaDirection::DownRight)));
            }
            _ => {
                assert!(false)
            }
        }
        state.move_piece(zero + HexaDirection::Down, zero, true);
        state.move_piece(up + HexaDirection::Up, up, true);
        state.place_piece(PieceType::Beetle, zero + HexaDirection::DownRight);

        state.current_player = Player::White;
        let moves = state.create_movement_decision(zero);
        assert_eq!(moves.option_count(), 6);
        match moves.context(&state) {
            HiveContext::TargetField(context) => {
                for f in state.board.get_field_unchecked(zero).neighbors() {
                    assert!(context.contains(&f.index()));
                }
            }
            _ => {
                assert!(false)
            }
        }

        // move the beetles to block one
        state.move_piece(zero, zero + HexaDirection::UpRight, true);
        state.move_piece(up, up + HexaDirection::UpRight, true);
        state.move_piece(
            zero + HexaDirection::UpRight,
            zero + HexaDirection::UpRight + HexaDirection::UpRight,
            true,
        );
        state.current_player = Player::White;
        state.place_piece(
            PieceType::Beetle,
            zero + HexaDirection::DownRight + HexaDirection::UpRight,
        );

        let moves = state.create_movement_decision(up + HexaDirection::UpRight);
        assert_eq!(moves.option_count(), 4);
        match moves.context(&state) {
            HiveContext::TargetField(context) => {
                // the beetle can not move into the hole
                assert!(!context.contains(&(zero + HexaDirection::UpRight)));
            }
            _ => {
                assert!(false)
            }
        }
    }
}
