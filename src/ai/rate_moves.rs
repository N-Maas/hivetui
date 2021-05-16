use std::collections::{hash_map::Entry, HashMap};

use tgp_ai::{rater::Rater, RatingType};
use tgp_board::{
    hypothetical::Hypothetical,
    index_map::{ArrayIndexMap, HashIndexMap},
    open_board::OpenIndex,
    prelude::*,
    structures::directions::{DirectionEnumerable, DirectionOffset, HexaDirection},
};

use crate::{
    pieces::{grashopper_moves, Piece, PieceType, Player},
    state::{HiveBoard, HiveContext, HiveGameState},
};

#[derive(Debug, Clone, Copy)]
enum MetaInterest {
    Uninteresting,
    Blocks(OpenIndex, Player),
    AdjacentToQueen(OpenIndex, Player),
}

impl MetaInterest {
    fn target_index(&self) -> OpenIndex {
        match *self {
            MetaInterest::Uninteresting => unreachable!(),
            MetaInterest::Blocks(i, _) => i,
            MetaInterest::AdjacentToQueen(i, _) => i,
        }
    }
}

impl Default for MetaInterest {
    fn default() -> Self {
        MetaInterest::Uninteresting
    }
}

#[derive(Debug, Clone, Copy)]
enum PositionType {
    NeutralOrBad,
    Blocking,
    AtQueen,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Equivalency {
    AntToNeutral(OpenIndex),
    AntToBlocking(OpenIndex, OpenIndex),
    AntToQueen(OpenIndex),
    PlaceAnt,
    PlaceQueen,
    // TODO: Beetle?
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
            (MetaInterest::Blocks(_, _), MetaInterest::AdjacentToQueen(_, _)) => {
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
        // Alternatively, the OHR might not hold
        // This is not always correct (and doesn't need to be, for the AI)
        no_common_neighbor(target, blocked.index())
    }
}

fn no_common_neighbor(a: Field<HiveBoard>, b: OpenIndex) -> bool {
    let a_to_b = HexaDirection::from_offset(b - a.index()).unwrap();
    a.next(a_to_b.next_direction()).unwrap().is_empty()
        && a.next(a_to_b.prev_direction()).unwrap().is_empty()
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

fn interest_to_type(
    meta: &HashIndexMap<OpenIndex, FieldMeta>,
    player: Player,
    interest: MetaInterest,
) -> (PositionType, RatingType) {
    let mut modifier = 0;
    (
        interest_to_type_with_mod(meta, player, interest, 0, 4, &mut modifier),
        modifier,
    )
}

fn interest_to_type_with_mod(
    meta: &HashIndexMap<OpenIndex, FieldMeta>,
    player: Player,
    interest: MetaInterest,
    bad_modifier: RatingType,
    queen_unblock_mod: RatingType,
    mod_out: &mut RatingType,
) -> PositionType {
    match interest {
        MetaInterest::Uninteresting => PositionType::NeutralOrBad,
        MetaInterest::Blocks(target, p) => {
            if p == player {
                *mod_out += bad_modifier;
                PositionType::NeutralOrBad
            } else {
                let target_meta = meta.get(target).unwrap().interest;
                if let MetaInterest::AdjacentToQueen(_, p) = target_meta {
                    // blocking a piece already adjacent to our queen is not really worthwile
                    if p == player {
                        dbg!("AdjacentToQueen doesn't block.");
                        *mod_out += queen_unblock_mod;
                        return PositionType::NeutralOrBad;
                    }
                }
                PositionType::Blocking
            }
        }
        MetaInterest::AdjacentToQueen(_, p) => {
            if p == player {
                *mod_out += bad_modifier;
                PositionType::NeutralOrBad
            } else {
                PositionType::AtQueen
            }
        }
    }
}

fn rate_usual_move(
    meta: &HashIndexMap<OpenIndex, FieldMeta>,
    piece: Piece,
    from: MetaInterest,
    to: MetaInterest,
    defensive: bool,
    modifier: RatingType,
) -> RatingType {
    let mut total_modifier = modifier;
    let from_type = interest_to_type_with_mod(meta, piece.player, from, 2, 0, &mut total_modifier);
    let to_type = interest_to_type_with_mod(meta, piece.player, to, -5, 4, &mut total_modifier);

    let rating = match (from_type, to_type) {
        (PositionType::NeutralOrBad, PositionType::NeutralOrBad) => 0,
        (PositionType::NeutralOrBad, PositionType::Blocking) => 12,
        (PositionType::NeutralOrBad, PositionType::AtQueen) => {
            if defensive {
                10
            } else {
                15
            }
        }
        (PositionType::Blocking, PositionType::NeutralOrBad) => -6,
        (PositionType::Blocking, PositionType::Blocking) => {
            if piece.p_type == PieceType::Ant {
                2
            } else {
                6
            }
        }
        (PositionType::Blocking, PositionType::AtQueen) => {
            if piece.p_type == PieceType::Ant {
                6
            } else {
                10
            }
        }
        (PositionType::AtQueen, PositionType::NeutralOrBad) => -6,
        (PositionType::AtQueen, PositionType::Blocking) => {
            if piece.p_type == PieceType::Ant {
                2
            } else {
                6
            }
        }
        (PositionType::AtQueen, PositionType::AtQueen) => 1,
    };
    rating + total_modifier
}

pub fn rate_moves(
    rater: &mut Rater,
    curr_context: &[HiveContext],
    data: &HiveGameState,
    old_context: &[(HiveContext, usize)],
    player: usize,
) {
    assert!(data.player_usize() == player);
    // Special case: As first piece, we always use the spider.
    if data.pieces().1 == 0 {
        println!("Start.");
        assert_eq!(rater.num_decisions(), 1);
        match &curr_context[0] {
            HiveContext::Piece(p_types) => {
                for (i, (t, _)) in p_types.iter().enumerate() {
                    match t {
                        PieceType::Spider => rater.rate(0, i, 100),
                        _ => rater.rate(0, i, 0),
                    }
                }
            }
            _ => unreachable!(),
        }
        return;
    }

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
    let mut queen_should_move = false;
    let mut defensive = false;
    let mut queen_neighbors = [0; 2];

    // points of interest
    for field in board.iter_fields() {
        if !field.is_empty() {
            let piece = field.content().last().unwrap();
            if piece.p_type == PieceType::Queen {
                let mut num_neighbors = 0;
                for n in field.neighbors() {
                    let n_meta = meta_info.get_mut(n.index()).unwrap();
                    n_meta.upgrade(MetaInterest::AdjacentToQueen(field.index(), piece.player));
                    if !n.is_empty() {
                        num_neighbors += 1;
                    }

                    let player = data.player();
                    if piece.player == player
                        && n.content().last().map_or(false, |p| p.player != player)
                    {
                        queen_endangered = true;
                    } else if piece.player == player
                        && meta_info.get_mut(field.index()).unwrap().can_move
                    {
                        queen_should_move = true;
                    }
                }
                queen_neighbors[usize::from(piece.player)] = num_neighbors;
            } else if meta_info.get(field.index()).unwrap().can_move {
                for n in field.neighbors() {
                    if n.is_empty() {
                        if would_block(n, field) {
                            let n_meta = meta_info.get_mut(n.index()).unwrap();
                            n_meta.upgrade(MetaInterest::Blocks(field.index(), piece.player));
                        }
                    } else {
                        let n_is_blocked = !meta_info.get(n.index()).unwrap().can_move;
                        if n_is_blocked && blocks(field, n) {
                            let meta = meta_info.get_mut(field.index()).unwrap();
                            meta.upgrade(MetaInterest::Blocks(
                                n.index(),
                                n.content().last().unwrap().player,
                            ));
                        }
                    }
                }
            }
        }
    }

    let player = data.player();
    if queen_neighbors[usize::from(player)] > queen_neighbors[usize::from(player.switched())] {
        defensive = true;
    }

    // movable queen special case: search for spider or grasshopper (or beetle?) that might endanger queen
    if queen_should_move {
        queen_should_move = false;
        'outer: for field in board.iter_fields() {
            if let Some(piece) = field.content().last() {
                if piece.player != data.player() {
                    let moves = match piece.p_type {
                        // TODO: add beetle?
                        PieceType::Spider => PieceType::Spider.get_moves(field),
                        PieceType::Grasshopper => grashopper_moves(field).collect(),
                        _ => Vec::new(),
                    };
                    for f in moves {
                        if let MetaInterest::AdjacentToQueen(_, player) =
                            meta_info.get(f.index()).unwrap().interest
                        {
                            if player == data.player() {
                                queen_should_move = true;
                                break 'outer;
                            }
                        }
                    }
                }
            }
        }
    }
    if old_context.is_empty() {
        dbg!(queen_endangered);
        dbg!(queen_should_move);
        dbg!(defensive);
        dbg!(queen_neighbors);
    }

    let mut eq_map = HashMap::<Equivalency, (usize, usize)>::new();

    // calculate the ratings
    for (i, c) in curr_context.iter().enumerate() {
        match c {
            HiveContext::SkipPlayer => rater.rate(0, 0, 0),
            HiveContext::BaseField(_) => unreachable!(),
            HiveContext::TargetField(context) => {
                let from = board.get_field_unchecked(*context.inner());
                let f_interest = meta_info.get(from.index()).unwrap().interest;
                let &piece = from.content().last().unwrap();
                debug_assert!(piece.player == data.player());
                for (j, target) in context.iter().enumerate() {
                    let to = board.get_field_unchecked(*target);
                    let t_interest = meta_info.get(to.index()).unwrap().interest;
                    debug_assert!(to.is_empty());

                    if piece.p_type == PieceType::Queen {
                        if queen_endangered {
                            rater.rate(i, j, 15);
                        } else if queen_should_move {
                            rater.rate(i, j, 10);
                        } else {
                            rater.rate(i, j, 0);
                        }
                    } else if piece.p_type == PieceType::Ant {
                        let mut is_better = false;
                        let (equivalency, m) =
                            interest_to_type(&meta_info, data.player(), t_interest);
                        let equivalency = match equivalency {
                            PositionType::NeutralOrBad => {
                                if m > 0 {
                                    is_better = true;
                                }
                                Equivalency::AntToNeutral(from.index())
                            }
                            PositionType::Blocking => {
                                debug_assert!(m == 0);
                                is_better = no_common_neighbor(to, t_interest.target_index());
                                Equivalency::AntToBlocking(from.index(), t_interest.target_index())
                            }
                            PositionType::AtQueen => {
                                debug_assert!(m == 0);
                                is_better = no_common_neighbor(to, t_interest.target_index());
                                Equivalency::AntToQueen(from.index())
                            }
                        };
                        let rating = rate_usual_move(
                            &meta_info, piece, f_interest, t_interest, defensive, m,
                        );
                        match eq_map.entry(equivalency) {
                            Entry::Occupied(mut entry) => {
                                if is_better {
                                    let (old_i, old_j) = entry.insert((i, j));
                                    rater.set_equivalent_as_representative(
                                        i, j, old_i, old_j, rating,
                                    );
                                } else {
                                    let &(old_i, old_j) = entry.get();
                                    rater.set_equivalent_to(i, j, old_i, old_j);
                                }
                            }
                            Entry::Vacant(mut entry) => {
                                entry.insert((i, j));
                                rater.rate(i, j, rating);
                            }
                        }
                    } else {
                        // TODO: Beetle special case (and equivalency)
                        let rating = rate_usual_move(
                            &meta_info, piece, f_interest, t_interest, defensive, 0,
                        );
                        rater.rate(i, j, rating);
                    }
                }
            }
            HiveContext::Piece(context) => {}
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;

    use tgp_board::{open_board::OpenIndex, structures::directions::HexaDirection, Board};

    use crate::{
        ai::rate_moves::{blocks, would_block},
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
