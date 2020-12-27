use std::str::FromStr;

use tgp::board::{
    directions::{DirectionEnumerable, HexaDirection},
    hypothetical::Hypothetical,
    open_board::OpenIndex,
    search::FieldSearchResult,
    search_tree::SearchMode,
    Board, DirectionStructure, Field,
};

use crate::state::HiveBoard;

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum Player {
    White,
    Black,
}

impl Player {
    pub fn switch(&mut self) {
        use Player::*;

        *self = match self {
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
        B: Board<Index = OpenIndex, Content = Vec<Piece>>,
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
        let mut hypothetical = Hypothetical::from_field(field);
        hypothetical[field].pop();
        let new_field = hypothetical.get_field_unchecked(field.index());
        let mut search = new_field.search();
        match self {
            PieceType::Queen => {
                search.replace(|f| Self::feasible_steps(f));
            }
            PieceType::Ant => {
                search.replace(|f| Self::feasible_steps(f));
                search.extend_repeated(|f| Self::feasible_steps(f));
            }
            PieceType::Spider => {
                let mut tree = new_field.search_tree();
                for _ in 0..3 {
                    tree.extend(|f| Self::feasible_steps(f), SearchMode::NoCycles);
                }
                search = tree.into_endpoint_set();
            }
            PieceType::Grasshopper => {
                search = new_field
                    .neighbors_by_direction()
                    .filter(|(_, f)| !f.is_empty())
                    .map(|(d, f)| {
                        f.iter_line(d)
                            .find(|target_field| target_field.is_empty())
                            .expect("Found no empty field for Grasshopper movement.")
                    })
                    .collect::<Option<_>>()
                    .expect("Grasshopper has no adjacent piece.");
            }
            PieceType::Beetle => {
                // plain or downwards move
                search.replace(|f| Self::feasible_steps(f));
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
            .collect()
    }

    pub fn is_movable(&self, field: Field<HiveBoard>) -> bool {
        assert!(!field.is_empty());
        match self {
            PieceType::Queen | PieceType::Ant | PieceType::Spider => {
                let mut hypothetical = Hypothetical::from_field(field);
                hypothetical[field].pop();
                Self::feasible_steps(hypothetical.get_field_unchecked(field.index()))
                    .into_iter()
                    .count()
                    > 0
            }
            PieceType::Grasshopper | PieceType::Beetle => true,
        }
    }
}

impl ToString for PieceType {
    fn to_string(&self) -> String {
        match self {
            PieceType::Queen => "Q".to_string(),
            PieceType::Ant => "A".to_string(),
            PieceType::Spider => "S".to_string(),
            PieceType::Grasshopper => "G".to_string(),
            PieceType::Beetle => "B".to_string(),
        }
    }
}

impl FromStr for PieceType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        match s {
            _ if s.starts_with("Q") => Ok(PieceType::Queen),
            _ if s.starts_with("A") => Ok(PieceType::Ant),
            _ if s.starts_with("S") => Ok(PieceType::Spider),
            _ if s.starts_with("G") => Ok(PieceType::Grasshopper),
            _ if s.starts_with("B") => Ok(PieceType::Beetle),
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub struct Piece {
    pub player: Player,
    pub p_type: PieceType,
}
