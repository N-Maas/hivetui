use std::str::FromStr;

use tgp::board::{
    directions::{DirectionEnumerable, HexaDirection},
    hypothetical::Hypothetical,
    open_board::OpenIndex,
    search::{FieldSearchResult, Searchable},
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
                let down = f.content().len() < height;
                let left_is_plain = field
                    .next(d.prev_direction())
                    .map_or(true, |f| f.content().len() == height);
                let right_is_plain = field
                    .next(d.next_direction())
                    .map_or(true, |f| f.content().len() == height);
                // we move along the _border_ of the hive
                down || (left_is_plain != right_is_plain && f.is_empty())
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
                for _ in 0..3 {
                    search.replace(|f| Self::feasible_steps(f));
                }
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
                search.replace(|f| Self::feasible_steps(f));
                // TODO: edge case: beetle needs to check freedom of movement also on top of the hive
                for f in new_field.neighbors().filter(|f| !f.is_empty()) {
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
                Self::feasible_steps(field).into_iter().count() > 0
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
