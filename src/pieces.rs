use std::fmt::{self, Display};

use tgp_board::{
    hypothetical::Hypothetical,
    index_map::ArrayIndexMap,
    open_board::OpenIndex,
    prelude::*,
    search::FieldSearchResult,
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum PieceType {
    Queen,
    Ant,
    Spider,
    Grasshopper,
    Beetle,
}

// note that both get_moves and is_movable do not consider the OHR yet
impl PieceType {
    fn feasible_steps<B>(field: Field<B>) -> FieldSearchResult<OpenIndex>
    where
        B: Board<Index = OpenIndex, Content = HiveContent>,
        B::Structure: DirectionStructure<B, Direction = HexaDirection>,
    {
        field
            .neighbors_by_direction()
            .filter(move |(d, f)| {
                let height = field.content().len();
                let plain = f.content().len() <= height;
                let left_is_plain = field
                    .next(d.prev_direction())
                    .map_or(true, |f| f.content().len() <= height);
                let right_is_plain = field
                    .next(d.next_direction())
                    .map_or(true, |f| f.content().len() <= height);
                if height == 0 {
                    // we either move along the _border_ of the hive..
                    (left_is_plain != right_is_plain) && plain
                } else {
                    // ..or on top of the hive
                    (left_is_plain || right_is_plain) && plain
                }
            })
            .map(|(_, f)| f)
            .collect()
    }

    pub fn get_moves<'a>(&self, field: Field<'a, HiveBoard>) -> Vec<Field<'a, HiveBoard>> {
        assert!(!field.is_empty());
        let mut hypothetical =
            Hypothetical::with_index_map(field.board(), ArrayIndexMap::<_, _, 1>::new());
        hypothetical[field].pop();
        let new_field = hypothetical.get_field_unchecked(field.index());
        let mut search = new_field.search();
        match self {
            PieceType::Queen => {
                search.replace(Self::feasible_steps);
            }
            PieceType::Ant => {
                search.extend_repeated(Self::feasible_steps);
            }
            PieceType::Spider => {
                let mut tree = new_field.search_tree();
                for _ in 0..3 {
                    tree.extend(Self::feasible_steps, SearchMode::NoCycles);
                }
                search = tree.into_endpoint_set();
            }
            PieceType::Grasshopper => {
                search = grasshopper_moves(new_field)
                    .collect::<Option<_>>()
                    .expect("Grasshopper has no adjacent piece.");
            }
            PieceType::Beetle => {
                // plain or downwards move
                search.replace(Self::feasible_steps);
                // upwards move
                for f in new_field
                    .neighbors()
                    .filter(|f| f.content().len() > new_field.content().len())
                {
                    search.insert(f);
                }
            }
        }
        search
            .into_iter()
            .map(|f| f.original_field(field.board()))
            .filter(|&f| f != field)
            .collect()
    }

    pub fn is_movable<B: Board<Index = OpenIndex, Content = HiveContent>>(
        &self,
        field: Field<B>,
    ) -> bool
    where
        B::Structure: DirectionStructure<B, Direction = HexaDirection>,
        B: BoardToMap<()>,
    {
        assert!(!field.is_empty());
        match self {
            // TODO: not completetly correct for spider
            PieceType::Queen | PieceType::Ant => feasible_steps_plain(field).count() > 0,
            PieceType::Grasshopper | PieceType::Beetle => true,
            PieceType::Spider => {
                let mut search = field.search();
                search.extend(|f| feasible_steps_plain(f).collect());
                search.extend(|f| feasible_steps_plain(f).collect());
                search.size() > 2
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
        tree.extend(PieceType::feasible_steps, SearchMode::NoCycles);
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
