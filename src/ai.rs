use tgp_ai::{
    rater::{DecisionType, Rater},
    RateAndMap, RatingType,
};
use tgp_board::{
    hypothetical::Hypothetical,
    index_map::ArrayIndexMap,
    open_board::OpenIndex,
    prelude::*,
    structures::directions::{DirectionEnumerable, DirectionOffset, HexaDirection},
};

use crate::{
    pieces::{Piece, PieceType, Player},
    state::{HiveBoard, HiveContext, HiveGameState},
};

#[derive(Debug, Clone, Copy)]
enum MetaInterest {
    Uninteresting,
    Blocks(OpenIndex),
    WouldBlock(OpenIndex),
    AdjacentToQueen(OpenIndex, Player),
}

impl Default for MetaInterest {
    fn default() -> Self {
        MetaInterest::Uninteresting
    }
}

#[derive(Debug, Default, Clone, Copy)]
struct FieldMeta {
    pub can_move: bool,
    pub interest: MetaInterest,
}

impl FieldMeta {
    fn upgrade(&mut self, new: MetaInterest) {
        match (self.interest, new) {
            (MetaInterest::Uninteresting, _) => self.interest = new,
            (MetaInterest::Blocks(_), MetaInterest::AdjacentToQueen(_, _)) => self.interest = new,
            (MetaInterest::WouldBlock(_), MetaInterest::AdjacentToQueen(_, _)) => {
                self.interest = new
            }
            _ => {}
        };
    }
}

fn dummy_piece() -> Piece {
    Piece {
        player: Player::White,
        p_type: PieceType::Spider,
    }
}

fn would_block(target: Field<HiveBoard>, blocked: Field<HiveBoard>) -> bool {
    debug_assert!(target.is_empty());
    debug_assert!(!blocked.is_empty());
    let mut hypothetical =
        Hypothetical::with_index_map(target.board(), ArrayIndexMap::<_, _, 1>::new());
    hypothetical[target].push(dummy_piece());
    let h_field = hypothetical.get_field_unchecked(blocked.index());
    if !h_field.content().last().unwrap().p_type.is_movable(h_field) {
        true
    } else {
        let b_to_t = HexaDirection::from_offset(target.index() - blocked.index()).unwrap();
        // Alternatively, the OHR might not hold
        // This is not always correct (and doesn't need to be, for the AI)
        blocked.next(b_to_t.next_direction()).unwrap().is_empty()
            && blocked.next(b_to_t.prev_direction()).unwrap().is_empty()
    }
}

fn blocks(target: Field<HiveBoard>, blocked: Field<HiveBoard>) -> bool {
    debug_assert!(!target.is_empty());
    debug_assert!(!blocked.is_empty());
    let mut hypothetical =
        Hypothetical::with_index_map(target.board(), ArrayIndexMap::<_, _, 1>::new());
    hypothetical[target].pop();
    let h_field = hypothetical.get_field_unchecked(blocked.index());
    if h_field.content().last().unwrap().p_type.is_movable(h_field) {
        // Now we need to verify the OHR holds when moving the blocked piece.
        // This is not always correct (and doesn't need to be, for the AI)
        let neighbor_count = h_field.neighbors().filter(|f| !f.is_empty()).count();
        neighbor_count <= 1
            || (neighbor_count <= 4
                // 4 or less neighbors and all neighbors are in a row => movable
                && h_field.neighbors_by_direction().all(|(d, f)| {
                    f.is_empty()
                        || !h_field.next(d.next_direction()).unwrap().is_empty()
                        || !h_field.next(d.prev_direction()).unwrap().is_empty()
                }))
    } else {
        false
    }
}

pub struct HiveAI {
    // TODO: weights etc.
}

impl RateAndMap<HiveGameState> for HiveAI {
    fn apply_type_mapping(&self, context: &HiveContext) -> DecisionType {
        match context {
            HiveContext::BaseField(_) => DecisionType::HigherLevel,
            HiveContext::TargetField(_) => DecisionType::BottomLevel,
            HiveContext::Piece(_) => DecisionType::BottomLevel,
            HiveContext::SkipPlayer => DecisionType::BottomLevel,
        }
    }

    fn rate_moves(
        &self,
        rater: &mut Rater<HiveGameState>,
        data: &HiveGameState,
        old_context: &[(HiveContext, usize)],
        player: usize,
    ) {
        assert!(data.player_usize() == player);
        let board = data.board();
        let mut meta_info = board.get_index_map();

        // initialize, movability
        for field in board.iter_fields() {
            let mut meta = FieldMeta::default();
            if !field.is_empty() {
                meta.can_move = data.can_move(field, false);
            }
            meta_info.insert(field.index(), meta);
        }

        let mut queen_endangered = false;

        // points of interest
        for field in board.iter_fields() {
            if !field.is_empty() {
                let queen = field.content().last().unwrap();
                if queen.p_type == PieceType::Queen {
                    for n in field.neighbors() {
                        let n_meta = meta_info.get_mut(n.index()).unwrap();
                        n_meta.upgrade(MetaInterest::AdjacentToQueen(field.index(), queen.player));

                        let player = data.player();
                        if queen.player == player
                            && n.content().last().map_or(false, |p| p.player != player)
                        {
                            queen_endangered = true;
                        }
                    }
                } else if meta_info.get(field.index()).unwrap().can_move {
                    for n in field.neighbors() {
                        if n.is_empty() {
                            if would_block(n, field) {
                                let n_meta = meta_info.get_mut(n.index()).unwrap();
                                n_meta.upgrade(MetaInterest::WouldBlock(field.index()));
                            }
                        } else {
                            let n_is_blocked = !meta_info.get(n.index()).unwrap().can_move;
                            if n_is_blocked && blocks(field, n) {
                                let meta = meta_info.get_mut(field.index()).unwrap();
                                meta.upgrade(MetaInterest::Blocks(n.index()));
                            }
                        }
                    }
                }
            }
        }
        debug_assert!((|| {
            for field in board.iter_fields() {
                let meta = meta_info.get(field.index()).unwrap();
                match meta.interest {
                    MetaInterest::Blocks(_) => assert!(!field.is_empty()),
                    MetaInterest::WouldBlock(_) => assert!(field.is_empty()),
                    _ => {}
                }
            }
            true
        })());
    }

    fn rate_game_state(
        &self,
        data: &HiveGameState,
        old_context: &[(HiveContext, usize)],
        player: usize,
    ) -> RatingType {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;

    use tgp_board::{open_board::OpenIndex, structures::directions::HexaDirection, Board};

    use crate::{
        ai::{blocks, would_block},
        pieces::PieceType,
        state::HiveGameState,
    };

    #[test]
    fn blocking_test() {
        let mut pieces = BTreeMap::new();
        pieces.insert(PieceType::Queen, 1);
        pieces.insert(PieceType::Ant, 2);
        let mut state = HiveGameState::new(pieces);
        let zero = OpenIndex::from((0, 0));
        let up = OpenIndex::from((0, 1));
        state.place_piece(PieceType::Queen, zero);
        state.place_piece(PieceType::Queen, up);

        assert!(would_block(
            state
                .board()
                .get_field_unchecked(zero + HexaDirection::Down),
            state.board().get_field_unchecked(zero)
        ));
        assert!(would_block(
            state
                .board()
                .get_field_unchecked(zero + HexaDirection::DownLeft),
            state.board().get_field_unchecked(zero)
        ));
        assert!(!would_block(
            state
                .board()
                .get_field_unchecked(zero + HexaDirection::UpLeft),
            state.board().get_field_unchecked(zero)
        ));

        state.place_piece(PieceType::Ant, zero + HexaDirection::UpRight);
        assert!(state.can_move(state.board().get_field_unchecked(zero), false));
        assert!(!would_block(
            state
                .board()
                .get_field_unchecked(zero + HexaDirection::DownRight),
            state.board().get_field_unchecked(zero)
        ));
        state.place_piece(PieceType::Ant, zero + HexaDirection::DownRight);
        assert!(blocks(
            state
                .board()
                .get_field_unchecked(zero + HexaDirection::DownRight),
            state.board().get_field_unchecked(zero)
        ));
        assert!(state.can_move(state.board().get_field_unchecked(zero), false));
        assert!(!would_block(
            state
                .board()
                .get_field_unchecked(zero + HexaDirection::Down),
            state.board().get_field_unchecked(zero)
        ));
        state.place_piece(PieceType::Ant, zero + HexaDirection::Down);
        assert!(state.can_move(state.board().get_field_unchecked(zero), false));
        assert!(would_block(
            state
                .board()
                .get_field_unchecked(zero + HexaDirection::DownLeft),
            state.board().get_field_unchecked(zero)
        ));
        state.place_piece(PieceType::Ant, zero + HexaDirection::DownLeft);
        assert!(blocks(
            state
                .board()
                .get_field_unchecked(zero + HexaDirection::DownLeft),
            state.board().get_field_unchecked(zero)
        ));
        state.remove_piece(zero + HexaDirection::UpRight);
        assert!(!blocks(
            state
                .board()
                .get_field_unchecked(zero + HexaDirection::DownLeft),
            state.board().get_field_unchecked(zero)
        ));
    }
}
