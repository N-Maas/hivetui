use std::fmt::{self, Display};

use tgp_board::{
    hypothetical::Hypothetical,
    index_map::ArrayIndexMap,
    open_board::OpenIndex,
    prelude::*,
    search::{SearchMode, SearchingTree},
    structures::{
        directions::{DirectionEnumerable, HexaDirection},
        DirectionStructure,
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord, Hash)]
pub enum PieceType {
    Queen,
    Ant,
    Spider,
    Grasshopper,
    Beetle,
    Ladybug,
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
        }
    }

    pub fn letter(&self) -> &'static str {
        &self.name()[0..1]
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

    // TODO: don't return a vec?!
    pub fn get_moves<'a>(&self, field: Field<'a, HiveBoard>) -> Vec<Field<'a, HiveBoard>> {
        assert!(!field.is_empty());
        let mut hypothetical =
            Hypothetical::with_index_map(field.board(), ArrayIndexMap::<_, _, 1>::new());
        hypothetical[field].pop();
        let new_field = hypothetical.get_field_unchecked(field.index());
        let mut search = new_field.search();
        match self {
            PieceType::Queen => {
                search.replace(Self::feasible_steps_flat);
            }
            PieceType::Ant => {
                search.extend_repeated(Self::feasible_steps_flat);
            }
            PieceType::Spider => {
                let mut tree = new_field.search_tree();
                for _ in 0..3 {
                    tree.extend(Self::feasible_steps_flat, SearchMode::NoCycles);
                }
                search = tree.into_endpoint_set();
            }
            PieceType::Grasshopper => {
                search = grasshopper_moves(new_field)
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
        }
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
        }
    }

    pub fn is_movable_generic<B: Board<Index = OpenIndex, Content = HiveContent>>(
        &self,
        field: Field<B>,
    ) -> bool
    where
        B::Structure: DirectionStructure<B, Direction = HexaDirection>,
        B: BoardToMap<()>,
    {
        assert!(!field.is_empty());
        match self {
            PieceType::Queen | PieceType::Ant | PieceType::Spider => {
                feasible_steps_plain(field).count() > 0
            }
            PieceType::Grasshopper | PieceType::Beetle | PieceType::Ladybug => true,
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
        let string = match self {
            PieceType::Queen => "Q",
            PieceType::Ant => "A",
            PieceType::Spider => "S",
            PieceType::Grasshopper => "G",
            PieceType::Beetle => "B",
            PieceType::Ladybug => "L",
        };
        write!(f, "{}", string)
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
