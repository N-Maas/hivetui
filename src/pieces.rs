use std::fmt::{self, Display};

use tgp_board::{
    hypothetical::Hypothetical,
    index_map::ArrayIndexMap,
    open_board::OpenIndex,
    prelude::*,
    search::{SearchMode, SearchingSet, SearchingTree},
    structures::{
        directions::{DirectionEnumerable, HexaDirection},
        DirectionStructure, NeighborhoodStructure,
    },
};

use crate::state::{HiveBoard, HiveContent};

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum Player {
    White,
    Black,
}

impl Display for Player {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Player::White => write!(f, "White"),
            Player::Black => write!(f, "Black"),
        }
    }
}

impl From<Player> for usize {
    fn from(p: Player) -> Self {
        match p {
            Player::White => 0,
            Player::Black => 1,
        }
    }
}

impl From<usize> for Player {
    fn from(val: usize) -> Self {
        match val {
            0 => Player::White,
            1 => Player::Black,
            _ => panic!("Invalid player"),
        }
    }
}

impl Player {
    pub fn switch(&mut self) {
        use Player::*;
        *self = match self {
            White => Black,
            Black => White,
        };
    }

    pub fn switched(&self) -> Self {
        use Player::*;
        match self {
            White => Black,
            Black => White,
        }
    }
}

/// used to represent sets of adjacent pieces for the mosquito
#[derive(Debug, Clone, Copy)]
pub struct PieceSet {
    flags: u32,
}

impl PieceSet {
    pub fn new() -> Self {
        Self { flags: 0 }
    }

    pub fn insert(&mut self, p_type: PieceType) {
        self.flags |= Self::piece_to_flag(p_type);
    }

    pub fn contains(&self, p_type: PieceType) -> bool {
        (self.flags & Self::piece_to_flag(p_type)) != 0
    }

    #[must_use]
    pub fn moves_dominance_set(mut self) -> Self {
        use PieceType::*;

        // remove mosquito
        self.flags &= !Self::piece_to_flag(Mosquito);
        self.apply_dominance(Beetle, Queen);
        self.apply_dominance(Ant, Spider);
        self.apply_dominance(Ant, Queen);
        self
    }

    #[must_use]
    pub fn movable_dominance_set(mut self) -> Self {
        use PieceType::*;

        // remove mosquito
        self.flags &= !Self::piece_to_flag(Mosquito);
        self.apply_dominance(Beetle, Queen);
        self.apply_dominance(Beetle, Ant);
        self.apply_dominance(Beetle, Spider);
        self.apply_dominance(Beetle, Ladybug);
        self.apply_dominance(Grasshopper, Queen);
        self.apply_dominance(Grasshopper, Ant);
        self.apply_dominance(Grasshopper, Spider);
        self.apply_dominance(Grasshopper, Ladybug);
        self.apply_dominance(Ladybug, Spider);
        self.apply_dominance(Ladybug, Ant);
        self.apply_dominance(Ladybug, Queen);
        self.apply_dominance(Ant, Spider);
        self.apply_dominance(Ant, Queen);
        self.apply_dominance(Queen, Spider);
        self
    }

    pub fn iter(&self) -> impl Iterator<Item = PieceType> {
        use PieceType::*;

        let flags = self.flags;
        let all = [Queen, Ant, Spider, Grasshopper, Beetle, Ladybug, Mosquito];
        all.into_iter()
            .filter(move |&p_type| (flags & Self::piece_to_flag(p_type)) != 0)
    }

    fn apply_dominance(&mut self, dom: PieceType, sub: PieceType) {
        // this bitflag based implementation allows branchless calculation
        // of the dominating piece set
        // (which is probably a pointless micro-optimization, but fun)
        debug_assert!(dom != sub);
        let dom_index = Self::piece_to_offset(dom);
        let sub_index = Self::piece_to_offset(sub);
        let flag = self.flags & Self::piece_to_flag(dom);
        if dom_index > sub_index {
            self.flags &= !(flag >> (dom_index - sub_index));
        } else {
            self.flags &= !(flag << (sub_index - dom_index));
        }
    }

    fn piece_to_offset(p_type: PieceType) -> u32 {
        match p_type {
            PieceType::Queen => 0,
            PieceType::Ant => 1,
            PieceType::Spider => 2,
            PieceType::Grasshopper => 3,
            PieceType::Beetle => 4,
            PieceType::Ladybug => 5,
            PieceType::Mosquito => 6,
        }
    }

    fn piece_to_flag(p_type: PieceType) -> u32 {
        1 << Self::piece_to_offset(p_type)
    }
}

impl<P> FromIterator<P> for PieceSet
where
    P: Into<PieceType>,
{
    fn from_iter<T: IntoIterator<Item = P>>(iter: T) -> Self {
        let mut result = Self::new();
        for p_type in iter.into_iter() {
            result.insert(p_type.into());
        }
        result
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord, Hash)]
pub enum PieceType {
    Queen,
    Ant,
    Spider,
    Grasshopper,
    Beetle,
    Ladybug,
    Mosquito,
}

// note that both get_moves and is_movable do not consider the OHR yet
impl PieceType {
    pub fn name(&self) -> &'static str {
        match self {
            PieceType::Queen => "Queen",
            PieceType::Ant => "Ant",
            PieceType::Spider => "Spider",
            PieceType::Grasshopper => "Grasshopper",
            PieceType::Beetle => "Beetle",
            PieceType::Ladybug => "Ladybug",
            PieceType::Mosquito => "Mosquito",
        }
    }

    pub fn letter(&self) -> &'static str {
        &self.name()[0..1]
    }

    pub fn from_letter(val: &str) -> Option<Self> {
        val.chars()
            .next()
            .and_then(|c| match c {
                'Q' => Some(PieceType::Queen),
                'A' => Some(PieceType::Ant),
                'S' => Some(PieceType::Spider),
                'G' => Some(PieceType::Grasshopper),
                'B' => Some(PieceType::Beetle),
                'L' => Some(PieceType::Ladybug),
                'M' => Some(PieceType::Mosquito),
                _ => None,
            })
            .filter(|_| val.len() == 1)
    }

    pub fn get_mosquito_piece_set<B>(field: Field<'_, B>, top_piece_removed: bool) -> PieceSet
    where
        B: Board<Index = OpenIndex, Content = HiveContent>,
        B::Structure: NeighborhoodStructure<B>,
    {
        assert!(
            top_piece_removed
                || field
                    .content()
                    .top()
                    .map_or(false, |p| p.p_type == PieceType::Mosquito)
        );
        if field.content().len() > 1 || (top_piece_removed && !field.content().is_empty()) {
            let mut set = PieceSet::new();
            set.insert(PieceType::Beetle);
            set
        } else {
            field
                .neighbors()
                .filter_map(|f| f.content().top().map(|p| p.p_type))
                .collect()
        }
    }

    fn feasible_steps_flat<B>(field: Field<B>) -> impl Iterator<Item = Field<B>>
    where
        B: Board<Index = OpenIndex, Content = HiveContent>,
        B::Structure: DirectionStructure<B, Direction = HexaDirection>,
    {
        Self::feasible_steps_impl(field, Some(0), false, true, true)
    }

    fn feasible_steps_any<B>(field: Field<B>) -> impl Iterator<Item = Field<B>>
    where
        B: Board<Index = OpenIndex, Content = HiveContent>,
        B::Structure: DirectionStructure<B, Direction = HexaDirection>,
    {
        Self::feasible_steps_impl(field, None, true, true, true)
    }

    fn feasible_steps_impl<B>(
        field: Field<B>,
        height: Option<usize>,
        may_go_up: bool,
        may_go_flat: bool,
        may_go_down: bool,
    ) -> impl Iterator<Item = Field<B>>
    where
        B: Board<Index = OpenIndex, Content = HiveContent>,
        B::Structure: DirectionStructure<B, Direction = HexaDirection>,
    {
        let height = height.unwrap_or_else(|| field.content().len());
        field
            .neighbors_by_direction()
            .filter(move |(d, f)| {
                let target_height = f.content().len();
                if target_height > height && !may_go_up {
                    return false;
                } else if target_height == height && !may_go_flat {
                    return false;
                } else if target_height < height && !may_go_down {
                    return false;
                }
                let max_height = usize::max(height, target_height);
                let left_is_plain = field
                    .next(d.prev_direction())
                    .map_or(true, |f| f.content().len() <= max_height);
                let right_is_plain = field
                    .next(d.next_direction())
                    .map_or(true, |f| f.content().len() <= max_height);
                if max_height == 0 {
                    // we either move along the _border_ of the hive..
                    left_is_plain != right_is_plain
                } else {
                    // ..or on top of the hive
                    left_is_plain || right_is_plain
                }
            })
            .map(|(_, f)| f)
    }

    fn get_moves_impl<'a, B>(&self, field: Field<'a, B>) -> SearchingSet<'a, B::Map, B>
    where
        B: Board<Index = OpenIndex, Content = HiveContent> + BoardToMap<()>,
        B::Structure: DirectionStructure<B, Direction = HexaDirection> + NeighborhoodStructure<B>,
    {
        let mut search = field.search();
        match self {
            PieceType::Queen => {
                search.replace(Self::feasible_steps_flat);
            }
            PieceType::Ant => {
                search.extend_repeated(Self::feasible_steps_flat);
            }
            PieceType::Spider => {
                let mut tree = field.search_tree();
                for _ in 0..3 {
                    tree.extend(Self::feasible_steps_flat, SearchMode::NoCycles);
                }
                return tree.into_endpoint_set();
            }
            PieceType::Grasshopper => {
                return grasshopper_moves(field)
                    .collect::<Option<_>>()
                    .expect("Grasshopper has no adjacent piece.");
            }
            PieceType::Beetle => {
                search.replace(Self::feasible_steps_any);
            }
            PieceType::Ladybug => {
                // upwards move
                search.replace(|f| Self::feasible_steps_impl(f, Some(0), true, false, false));
                // move on top of hive
                search.replace(|f| Self::feasible_steps_any(f).filter(|f| !f.content().is_empty()));
                // downwards move
                search.replace(|f| {
                    Self::feasible_steps_impl(f, None, false, false, true)
                        .filter(|f| f.content().is_empty())
                });
            }
            PieceType::Mosquito => {
                let piece_set = Self::get_mosquito_piece_set(field, true);
                for p_type in piece_set.moves_dominance_set().iter() {
                    let p_moves = p_type.get_moves_impl(field);
                    for field in p_moves.into_iter() {
                        search.insert(field);
                    }
                }
            }
        }
        search
    }

    // TODO: don't return a vec?!
    pub fn get_moves<'a>(&self, field: Field<'a, HiveBoard>) -> Vec<Field<'a, HiveBoard>> {
        assert!(!field.is_empty());
        let mut hypothetical =
            Hypothetical::with_index_map(field.board(), ArrayIndexMap::<_, _, 1>::new());
        hypothetical[field].pop();
        let new_field = hypothetical.get_field_unchecked(field.index());
        let search = self.get_moves_impl(new_field);
        search
            .into_iter()
            .map(|f| f.original_field(field.board()))
            .filter(|&f| f != field)
            .collect()
    }

    pub fn is_movable(&self, field: Field<HiveBoard>) -> bool {
        assert!(!field.is_empty());
        match self {
            PieceType::Queen | PieceType::Ant => feasible_steps_plain(field).count() > 0,
            PieceType::Grasshopper | PieceType::Beetle => true,
            PieceType::Spider | PieceType::Ladybug => !self.get_moves(field).is_empty(),
            PieceType::Mosquito => {
                let piece_set = Self::get_mosquito_piece_set(field, false);
                for p_type in piece_set.movable_dominance_set().iter() {
                    if p_type.is_movable(field) {
                        return true;
                    }
                }
                false
            }
        }
    }

    pub fn is_movable_generic<B>(&self, field: Field<B>) -> bool
    where
        B: Board<Index = OpenIndex, Content = HiveContent> + BoardToMap<()>,
        B::Structure: DirectionStructure<B, Direction = HexaDirection> + NeighborhoodStructure<B>,
    {
        assert!(!field.is_empty());
        match self {
            PieceType::Queen | PieceType::Ant | PieceType::Spider => {
                feasible_steps_plain(field).count() > 0
            }
            PieceType::Grasshopper | PieceType::Beetle | PieceType::Ladybug => true,
            PieceType::Mosquito => {
                let piece_set = Self::get_mosquito_piece_set(field, false);
                for p_type in piece_set.movable_dominance_set().iter() {
                    if p_type.is_movable_generic(field) {
                        return true;
                    }
                }
                false
            }
        }
    }
}

pub fn feasible_steps_plain<B: Board<Index = OpenIndex, Content = HiveContent>>(
    field: Field<B>,
) -> impl Iterator<Item = Field<B>>
where
    B::Structure: DirectionStructure<B, Direction = HexaDirection>,
{
    field
        .neighbors_by_direction()
        .filter(move |(d, n)| {
            n.is_empty() && {
                let left_is_plain = field
                    .next(d.prev_direction())
                    .map_or(true, |f| f.is_empty());
                let right_is_plain = field
                    .next(d.next_direction())
                    .map_or(true, |f| f.is_empty());
                // move along the border of the hive
                left_is_plain != right_is_plain
            }
        })
        .map(|(_, f)| f)
}

/// Attention: the spider piece must be removed from the board beforehand!
pub fn spider_moves<B>(field: Field<B>) -> SearchingTree<'_, B::Map, B>
where
    B: Board<Index = OpenIndex, Content = HiveContent> + BoardToMap<()>,
    B::Structure: DirectionStructure<B, Direction = HexaDirection>,
{
    let mut tree = field.search_tree();
    for _ in 0..3 {
        tree.extend(PieceType::feasible_steps_flat, SearchMode::NoCycles);
    }
    tree
}

pub fn grasshopper_moves<B>(field: Field<B>) -> impl Iterator<Item = Field<B>>
where
    B: Board<Content = HiveContent> + BoardToMap<()>,
    B::Structure: DirectionStructure<B, Direction = HexaDirection>,
{
    field
        .neighbors_by_direction()
        .filter(|(_, f)| !f.is_empty())
        .map(|(d, f)| {
            f.iter_line(d)
                .find(|target_field| target_field.is_empty())
                .expect("Found no empty field for Grasshopper movement.")
        })
}

impl Display for PieceType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.letter())
    }
}

// impl FromStr for PieceType {
//     type Err = ();

//     fn from_str(s: &str) -> Result<Self, ()> {
//         match s {
//             _ if s.starts_with('Q') => Ok(PieceType::Queen),
//             _ if s.starts_with('A') => Ok(PieceType::Ant),
//             _ if s.starts_with('S') => Ok(PieceType::Spider),
//             _ if s.starts_with('G') => Ok(PieceType::Grasshopper),
//             _ if s.starts_with('B') => Ok(PieceType::Beetle),
//             _ => Err(()),
//         }
//     }
// }

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub struct Piece {
    pub player: Player,
    pub p_type: PieceType,
}

#[cfg(test)]
mod test {
    use super::{PieceSet, PieceType};

    #[test]
    fn piece_set() {
        let mut set = PieceSet::new();
        assert!(!set.contains(PieceType::Ant));
        set.insert(PieceType::Ant);
        assert!(set.contains(PieceType::Ant));
        set.insert(PieceType::Ladybug);
        set.insert(PieceType::Ant);
        assert_eq!(
            set.iter().collect::<Vec<_>>(),
            vec![PieceType::Ant, PieceType::Ladybug]
        );
        set.insert(PieceType::Spider);
        set.insert(PieceType::Queen);
        let dom_moves = set.moves_dominance_set();
        assert_eq!(
            dom_moves.iter().collect::<Vec<_>>(),
            vec![PieceType::Ant, PieceType::Ladybug]
        );
        let mut set = PieceSet::new();
        set.insert(PieceType::Queen);
        let dom_moves = set.moves_dominance_set();
        assert_eq!(dom_moves.iter().collect::<Vec<_>>(), vec![PieceType::Queen]);
    }
}
