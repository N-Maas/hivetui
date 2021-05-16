use std::collections::{hash_map::Entry, HashMap};

use tgp::vec_context::VecContext;
use tgp_ai::{rater::Rater, RatingType};
use tgp_board::{
    hypothetical::Hypothetical,
    index_map::{ArrayIndexMap, HashIndexMap},
    open_board::OpenIndex,
    prelude::*,
    structures::directions::{DirectionEnumerable, DirectionOffset, HexaDirection},
};

use crate::{
    pieces::{grashopper_moves, spider_moves, Piece, PieceType, Player},
    state::{HiveBoard, HiveContext, HiveGameState},
};

// ------ metadata types -----
#[derive(Debug, Clone)]
struct MetaData {
    map: HashIndexMap<OpenIndex, FieldMeta>,
    queen_endangered: bool,
    queen_should_move: bool,
    defensive: bool,
    want_to_block: bool,
    queen_neighbors: [u32; 2],
}

impl MetaData {
    fn can_move(&self, field: impl Into<OpenIndex>) -> bool {
        self.map.get(field.into()).unwrap().can_move
    }

    fn interest(&self, field: impl Into<OpenIndex>) -> MetaInterest {
        self.map.get(field.into()).unwrap().interest
    }

    fn get_mut(&mut self, field: impl Into<OpenIndex>) -> &mut FieldMeta {
        self.map.get_mut(field.into()).unwrap()
    }

    fn is_blocking(&self, field: impl Into<OpenIndex>, player: Player) -> bool {
        match self.interest(field) {
            MetaInterest::Blocks(_, p) => p == player,
            _ => false,
        }
    }

    fn is_adj_to_queen(&self, field: impl Into<OpenIndex>, player: Player) -> bool {
        match self.interest(field) {
            MetaInterest::AdjacentToQueen(_, p) => p == player,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
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

// ------ types used by the rating -----
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PositionType {
    NeutralOrBad,
    Blocking,
    AtQueen,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Equivalency {
    AntToNeutral(OpenIndex),
    AntToBlocking(OpenIndex, OpenIndex),
    AntBlockingLow(OpenIndex),
    AntToQueen(OpenIndex),
    PlaceAnt,
    PlaceQueen,
    // TODO: Beetle?
}

// ------ utility functions -----
fn dummy_piece() -> Piece {
    Piece {
        player: Player::White,
        p_type: PieceType::Spider,
    }
}

fn would_block(target: Field<HiveBoard>, blocked: Field<HiveBoard>) -> bool {
    debug_assert!(target.is_empty());
    debug_assert!(!blocked.is_empty());
    if blocked.content().len() > 1 {
        return false;
    }

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

// ----- calculate metadata -----
fn calculate_metadata(data: &HiveGameState) -> MetaData {
    let board = data.board();
    let mut meta_data = MetaData {
        map: board.get_index_map(),
        queen_endangered: false,
        queen_should_move: false,
        defensive: false,
        want_to_block: false,
        queen_neighbors: [0; 2],
    };
    let mut free_enemy_ant = false;

    // initialize, movability
    for field in board.iter_fields() {
        let mut meta = FieldMeta::default();
        if !field.is_empty() {
            meta.can_move = data.can_move(field, false);
        }
        meta_data.map.insert(field.index(), meta);
    }

    // points of interest
    for field in board.iter_fields() {
        if !field.is_empty() {
            let queen = field.content().first().unwrap();
            if queen.p_type == PieceType::Queen {
                let mut num_neighbors = 0;
                for n in field.neighbors() {
                    let n_meta = meta_data.get_mut(n);
                    n_meta.upgrade(MetaInterest::AdjacentToQueen(field.index(), queen.player));
                    if !n.is_empty() {
                        num_neighbors += 1;
                    }

                    let player = data.player();
                    if queen.player == player
                        && n.content().last().map_or(false, |p| p.player != player)
                    {
                        meta_data.queen_endangered = true;
                    }
                }
                meta_data.queen_neighbors[usize::from(queen.player)] = num_neighbors;
            } else if meta_data.can_move(field) {
                for n in field.neighbors() {
                    if n.is_empty() {
                        if would_block(n, field) {
                            let n_meta = meta_data.get_mut(n);
                            n_meta.upgrade(MetaInterest::Blocks(field.index(), queen.player));
                        }
                    } else if !meta_data.can_move(n) && blocks(field, n) {
                        let meta = meta_data.get_mut(field);
                        meta.upgrade(MetaInterest::Blocks(
                            n.index(),
                            n.content().last().unwrap().player,
                        ));
                    }
                }
            }
        }
    }

    let player = data.player();
    if meta_data.queen_neighbors[usize::from(player)]
        > meta_data.queen_neighbors[usize::from(player.switched())]
    {
        meta_data.defensive = true;
    }

    // movable queen special case: search for spider or grasshopper (or beetle?) that might endanger queen
    'outer: for field in board.iter_fields() {
        if let Some(piece) = field.content().last() {
            if piece.player != data.player() && meta_data.can_move(field) {
                let moves = match piece.p_type {
                    // TODO: add beetle?
                    PieceType::Spider => PieceType::Spider.get_moves(field),
                    PieceType::Grasshopper => grashopper_moves(field).collect(),
                    _ => Vec::new(),
                };
                for f in moves {
                    if meta_data.is_adj_to_queen(f, data.player()) {
                        meta_data.queen_should_move = true;
                        break 'outer;
                    }
                }

                // test whether this is a free ant that the enemy has available to attack the queen
                // (it is free unless it is too occupied blocking another piece)
                if piece.p_type == PieceType::Ant
                    && !(meta_data.is_adj_to_queen(field, data.player())
                        || (meta_data.is_blocking(field, data.player())
                            && meta_data.queen_neighbors[usize::from(data.player())] <= 3))
                {
                    free_enemy_ant = true;
                }
            }
        }
    }

    let equal_neighbors = meta_data.queen_neighbors[0] == meta_data.queen_neighbors[1];
    if meta_data.defensive || (equal_neighbors && (meta_data.queen_should_move || free_enemy_ant)) {
        meta_data.want_to_block = true;
    }
    meta_data
}

// ----- functions for rating -----
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

fn set_eq(
    i: usize,
    j: usize,
    rater: &mut Rater,
    eq_map: &mut HashMap<Equivalency, (usize, usize)>,
    equivalency: Equivalency,
    rating: RatingType,
    is_better: bool,
) {
    // test whether an equivalent move exists already
    match eq_map.entry(equivalency) {
        Entry::Occupied(mut entry) => {
            if is_better {
                let (old_i, old_j) = entry.insert((i, j));
                rater.set_equivalent_as_representative(i, j, old_i, old_j, rating);
            } else {
                let &(old_i, old_j) = entry.get();
                rater.set_equivalent_to(i, j, old_i, old_j);
            }
        }
        Entry::Vacant(entry) => {
            entry.insert((i, j));
            rater.rate(i, j, rating);
        }
    }
}

fn handle_move_ratings(
    data: &HiveGameState,
    meta_data: &MetaData,
    context: &VecContext<OpenIndex, OpenIndex>,
    eq_map: &mut HashMap<Equivalency, (usize, usize)>,
    rater: &mut Rater,
    i: usize,
) {
    let from = data.board().get_field_unchecked(*context.inner());
    let f_interest = meta_data.interest(from);
    let &piece = from.content().last().unwrap();
    debug_assert!(piece.player == data.player());
    for (j, target) in context.iter().enumerate() {
        let to = data.board().get_field_unchecked(*target);
        let t_interest = meta_data.interest(to);

        if piece.p_type == PieceType::Queen {
            // moving the queen only makes sense when it is endangered
            if meta_data.queen_endangered
                && meta_data.queen_neighbors[usize::from(data.player())] > 1
            {
                rater.rate(i, j, 15);
            } else if meta_data.queen_should_move {
                rater.rate(i, j, 10);
            } else if meta_data.queen_neighbors[usize::from(data.player().switched())] == 5 {
                rater.rate(i, j, 6);
            } else {
                rater.rate(i, j, 0);
            }
        } else if piece.p_type == PieceType::Ant {
            // to avoid combinatorial explosion, it is really important to use equivalency classes for ants
            let mut is_better = false;
            let (equivalency, m) = interest_to_type(&meta_data.map, data.player(), t_interest);
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
                    if interest_to_type(&meta_data.map, data.player(), f_interest).0
                        == PositionType::NeutralOrBad
                    {
                        Equivalency::AntToBlocking(from.index(), t_interest.target_index())
                    } else {
                        Equivalency::AntBlockingLow(t_interest.target_index())
                    }
                }
                PositionType::AtQueen => {
                    debug_assert!(m == 0);
                    is_better = no_common_neighbor(to, t_interest.target_index());
                    Equivalency::AntToQueen(from.index())
                }
            };
            let rating = rate_usual_move(&meta_data, piece, f_interest, t_interest, 0);
            set_eq(i, j, rater, eq_map, equivalency, rating, is_better);
        } else {
            // TODO: Beetle special case (and equivalency)
            let rating = rate_usual_move(&meta_data, piece, f_interest, t_interest, 0);
            rater.rate(i, j, rating);
        }
    }
}

fn rate_usual_move(
    meta: &MetaData,
    piece: Piece,
    from: MetaInterest,
    to: MetaInterest,
    modifier: RatingType,
) -> RatingType {
    let mut total_modifier = modifier;
    let from_type =
        interest_to_type_with_mod(&meta.map, piece.player, from, 2, 0, &mut total_modifier);
    let to_type =
        interest_to_type_with_mod(&meta.map, piece.player, to, -5, 4, &mut total_modifier);

    let rating = match (from_type, to_type) {
        (PositionType::NeutralOrBad, PositionType::NeutralOrBad) => 0,
        (PositionType::NeutralOrBad, PositionType::Blocking) => 12,
        (PositionType::NeutralOrBad, PositionType::AtQueen) => {
            if meta.defensive {
                10
            } else {
                15
            }
        }
        (PositionType::Blocking, PositionType::NeutralOrBad) => -6,
        (PositionType::Blocking, PositionType::Blocking) => {
            if meta.want_to_block {
                8
            } else if piece.p_type == PieceType::Ant {
                4
            } else {
                6
            }
        }
        (PositionType::Blocking, PositionType::AtQueen) => {
            if meta.defensive {
                3
            } else if piece.p_type == PieceType::Ant {
                1 + 2 * meta.queen_neighbors[usize::from(piece.player.switched())] as i32
            } else {
                10
            }
        }
        (PositionType::AtQueen, PositionType::NeutralOrBad) => -6,
        (PositionType::AtQueen, PositionType::Blocking) => {
            if meta.want_to_block {
                9
            } else if piece.p_type == PieceType::Ant {
                2
            } else {
                4
            }
        }
        (PositionType::AtQueen, PositionType::AtQueen) => 1,
    };
    rating + total_modifier
}

fn handle_placement_ratings(
    data: &HiveGameState,
    meta_data: &MetaData,
    context: &VecContext<(PieceType, u32), OpenIndex>,
    eq_map: &mut HashMap<Equivalency, (usize, usize)>,
    rater: &mut Rater,
    i: usize,
) {
    let player = data.player();
    let target = data.board().get_field_unchecked(*context.inner());
    for (j, (piece_t, _)) in context.iter().enumerate() {
        if meta_data.is_adj_to_queen(target, player.switched()) {
            // placing a piece directly at the enemy queen is quite perfect..
            rater.rate(i, j, 18);
        } else {
            // .. otherwise, a few case distinctions are necessary
            let meta = meta_data.interest(target);
            assert!(interest_to_type(&meta_data.map, player, meta).0 == PositionType::NeutralOrBad);
            match piece_t {
                PieceType::Queen => {
                    let is_better = meta == MetaInterest::Uninteresting;
                    set_eq(i, j, rater, eq_map, Equivalency::PlaceQueen, 11, is_better);
                }
                PieceType::Ant => {
                    let is_better = meta == MetaInterest::Uninteresting;
                    set_eq(i, j, rater, eq_map, Equivalency::PlaceAnt, 11, is_better);
                }
                PieceType::Spider | PieceType::Grasshopper => {
                    // for spiders and grasshoppers, it highly depends on whether they can reach something useful
                    let reachable_fields: Vec<_> = if *piece_t == PieceType::Spider {
                        let tree = spider_moves(target);
                        tree.iter_paths().map(|p| p.endpoint()).collect()
                    } else {
                        grashopper_moves(target).collect()
                    };
                    let rating = reachable_fields
                        .into_iter()
                        .map(|f| {
                            let interest = meta_data.interest(f);
                            match interest_to_type(&meta_data.map, player, interest).0 {
                                PositionType::NeutralOrBad => 1,
                                PositionType::Blocking => 5,
                                PositionType::AtQueen => match interest {
                                    MetaInterest::AdjacentToQueen(queen, _) => {
                                        if meta_data.can_move(queen) {
                                            // the queen can just move away
                                            3
                                        } else if meta_data.defensive {
                                            5
                                        } else {
                                            8
                                        }
                                    }
                                    _ => unreachable!(),
                                },
                            }
                        })
                        .max()
                        .unwrap();
                    rater.rate(i, j, rating);
                }
                PieceType::Beetle => {
                    // TODO
                    rater.rate(i, j, 5);
                }
            }
        }
    }
}

/// Calculates a rating for all moves. Before that, some metadata for the field is calculated.
pub fn rate_moves(
    rater: &mut Rater,
    curr_context: &[HiveContext],
    data: &HiveGameState,
    // TODO
    _old_context: &[(HiveContext, usize)],
) {
    // Special case: We always use the spider as first piece.
    if data.pieces().1 == 0 {
        assert_eq!(rater.num_decisions(), 1);
        match &curr_context[0] {
            HiveContext::Piece(p_types) => {
                for (i, &(t, _)) in p_types.iter().enumerate() {
                    if t == PieceType::Spider {
                        rater.rate(0, i, 100);
                    } else {
                        rater.rate(0, i, 0);
                    }
                }
            }
            _ => unreachable!(),
        }
        return;
    }

    // calculate the metadata
    let meta_data = calculate_metadata(data);
    // if old_context.is_empty() {
    //     dbg!(&meta_data);
    // }

    // calculate the ratings
    // TODO: improve placement eq-classes (better and worse blocking..)? possibly extend to spider/grasshopper
    let mut eq_map = HashMap::<Equivalency, (usize, usize)>::new();
    for (i, c) in curr_context.iter().enumerate() {
        match c {
            HiveContext::BaseField(_) => unreachable!(),
            HiveContext::SkipPlayer => rater.rate(0, 0, 0),
            HiveContext::TargetField(context) => {
                handle_move_ratings(data, &meta_data, context, &mut eq_map, rater, i);
            }
            HiveContext::Piece(context) => {
                handle_placement_ratings(data, &meta_data, context, &mut eq_map, rater, i);
            }
        }
    }
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;

    use tgp_board::{
        open_board::OpenIndex, structures::directions::HexaDirection, Board, BoardToMap,
    };

    use crate::{
        ai::{
            rate_moves::{blocks, would_block, MetaInterest},
            HiveAI,
        },
        display::{print_annotated_board, print_move_ratings},
        pieces::{PieceType, Player},
        state::HiveGameState,
    };

    use super::calculate_metadata;

    #[test]
    fn meta_data_test_base() {
        let mut pieces = BTreeMap::new();
        pieces.insert(PieceType::Queen, 1);
        pieces.insert(PieceType::Ant, 2);
        let mut state = HiveGameState::new(pieces);
        let zero = OpenIndex::from((0, 0));
        let up = OpenIndex::from((0, 1));
        state.place_piece(PieceType::Ant, zero);
        state.place_piece(PieceType::Ant, up);
        state.place_piece(PieceType::Queen, zero + HexaDirection::Down);
        state.place_piece(PieceType::Queen, up + HexaDirection::Up);
        state.place_piece(PieceType::Ant, zero + HexaDirection::DownLeft);
        state.place_piece(
            PieceType::Ant,
            zero + HexaDirection::Down + HexaDirection::DownRight,
        );

        let meta_data = calculate_metadata(&state);
        assert!(meta_data.queen_endangered);
        assert!(!meta_data.queen_should_move);
        assert!(meta_data.defensive);
        assert_eq!(meta_data.queen_neighbors, [3, 1]);
        for f in state.board().iter_fields() {
            let movable = [
                up + HexaDirection::Up,
                zero + HexaDirection::DownLeft,
                zero + HexaDirection::Down + HexaDirection::DownRight,
            ]
            .contains(&f.index());
            assert_eq!(meta_data.can_move(f), movable);
        }
        for f in state
            .board()
            .get_field_unchecked(zero + HexaDirection::Down)
            .neighbors()
        {
            assert!(meta_data.is_adj_to_queen(f, Player::White));
        }
        for f in state
            .board()
            .get_field_unchecked(up + HexaDirection::Up)
            .neighbors()
        {
            assert!(meta_data.is_adj_to_queen(f, Player::Black));
        }
        assert_eq!(
            meta_data.interest(zero + HexaDirection::UpLeft),
            MetaInterest::Uninteresting
        );
        assert_eq!(
            meta_data.interest(zero + HexaDirection::UpRight),
            MetaInterest::Uninteresting
        );
        assert_eq!(
            meta_data.interest(zero + HexaDirection::Down),
            MetaInterest::Uninteresting
        );
        assert_eq!(
            meta_data.interest(zero + HexaDirection::DownLeft + HexaDirection::UpLeft),
            MetaInterest::Blocks(zero + HexaDirection::DownLeft, Player::White)
        );
        assert_eq!(
            meta_data.interest(zero + HexaDirection::DownRight + HexaDirection::DownRight),
            MetaInterest::Blocks(
                zero + HexaDirection::Down + HexaDirection::DownRight,
                Player::Black
            )
        );
    }

    #[test]
    fn meta_data_test_queen() {
        let mut pieces = BTreeMap::new();
        pieces.insert(PieceType::Queen, 1);
        pieces.insert(PieceType::Ant, 2);
        pieces.insert(PieceType::Spider, 1);
        pieces.insert(PieceType::Grasshopper, 1);
        let mut state = HiveGameState::new(pieces);
        let zero = OpenIndex::from((0, 0));
        let up = OpenIndex::from((0, 1));
        state.place_piece(PieceType::Ant, zero);
        state.place_piece(PieceType::Queen, up);
        state.place_piece(PieceType::Queen, zero + HexaDirection::Down);
        state.place_piece(PieceType::Spider, up + HexaDirection::UpRight);

        let meta_data = calculate_metadata(&state);
        assert!(!meta_data.queen_endangered);
        assert!(meta_data.queen_should_move);
        assert!(!meta_data.defensive);
        assert!(!meta_data.want_to_block);
        assert_eq!(meta_data.queen_neighbors, [1, 2]);
        for f in state.board().iter_fields() {
            let movable =
                [up + HexaDirection::UpRight, zero + HexaDirection::Down].contains(&f.index());
            assert_eq!(meta_data.can_move(f), movable);
        }

        state.place_piece(PieceType::Grasshopper, zero + HexaDirection::DownRight);
        let meta_data = calculate_metadata(&state);
        assert!(meta_data.queen_endangered);
        assert!(!meta_data.defensive);
        assert!(meta_data.want_to_block);
        assert_eq!(meta_data.queen_neighbors, [2, 2]);
        for f in state.board().iter_fields() {
            let movable = [
                up + HexaDirection::UpRight,
                zero + HexaDirection::Down,
                zero + HexaDirection::DownRight,
            ]
            .contains(&f.index());
            assert_eq!(meta_data.can_move(f), movable);
        }

        state.place_piece(PieceType::Ant, up + HexaDirection::Up);
        let meta_data = calculate_metadata(&state);
        assert!(!meta_data.queen_endangered);
        assert!(!meta_data.queen_should_move);
        assert!(!meta_data.defensive);
        assert!(!meta_data.want_to_block);
        assert_eq!(meta_data.queen_neighbors, [2, 3]);
        assert_eq!(
            meta_data.interest(up + HexaDirection::Up + HexaDirection::UpRight),
            MetaInterest::Uninteresting
        );

        state.remove_piece(up + HexaDirection::Up);
        state.place_piece(PieceType::Grasshopper, up + HexaDirection::Up);
        let meta_data = calculate_metadata(&state);
        assert!(!meta_data.queen_endangered);
        assert!(meta_data.queen_should_move);
        assert!(!meta_data.defensive);
        assert!(!meta_data.want_to_block);
        assert_eq!(meta_data.queen_neighbors, [2, 3]);
        assert_eq!(
            meta_data.interest(up + HexaDirection::Up + HexaDirection::UpRight),
            MetaInterest::Uninteresting
        );
    }

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
        state.place_piece(PieceType::Ant, zero);
        state.place_piece(PieceType::Ant, up);
        state.place_piece(PieceType::Queen, zero + HexaDirection::Down);
        state.place_piece(PieceType::Queen, up + HexaDirection::Up);
        state.place_piece(PieceType::Ant, zero + HexaDirection::DownLeft);
        state.place_piece(PieceType::Ant, zero + HexaDirection::DownRight);
        state.place_piece(
            PieceType::Spider,
            zero + HexaDirection::DownRight + HexaDirection::DownRight,
        );
        state.place_piece(PieceType::Ant, up + HexaDirection::UpRight);

        print_annotated_board::<usize>(&state, &state.board().get_index_map(), false);
        let rating = print_move_ratings(&state, &HiveAI {});
        // Move  <Q> from (0 , -1) to (1 , -1) =>   15
        // Move  <Q> from (0 , -1) to (-1, -2) =>   15
        // Move  <A> from (-1, -1) to (2 , 2 ) =>   14
        // Move  <A> from (-1, -1) to (0 , 3 ) =>   12
        // Move  <S> from (2 , 0 ) to (2 , 2 ) =>   12
        // Place <A>  at  (-2, -2)             =>   11
        // Place <G>  at  (0 , -2)             =>    3
        // Move  <A> from (-1, -1) to (-1, 0 ) =>    2
        // Place <G>  at  (-2, -2)             =>    1
        assert_eq!(
            rating
                .into_iter()
                .map(|(r, _, _)| r)
                .take(9)
                .collect::<Vec<_>>(),
            vec![15, 15, 14, 12, 12, 11, 3, 2, 1]
        );
    }
}
