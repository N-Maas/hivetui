use tgp::board::{
    directions::DirectionEnumerable,
    open_board::OpenIndex,
    search::{FieldSearchResult, Searchable},
    Field,
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
    fn feasible_steps(field: Field<HiveBoard>) -> FieldSearchResult<OpenIndex> {
        field
            .neighbors_by_direction()
            .filter(move |(d, f)| {
                let left_is_free = field
                    .next(d.prev_direction())
                    .map_or(true, |f| f.is_empty());
                let right_is_free = field
                    .next(d.next_direction())
                    .map_or(true, |f| f.is_empty());
                // we move along the _border_ of the hive
                (left_is_free != right_is_free) && f.is_empty()
            })
            .map(|(_, f)| f)
            .collect()
    }

    pub fn get_moves<'a>(
        &self,
        field: Field<'a, HiveBoard>,
    ) -> impl Iterator<Item = Field<'a, HiveBoard>> {
        assert!(!field.is_empty());
        let mut search = field.search();
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
                search = field
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
                for f in field.neighbors().filter(|f| !f.is_empty()) {
                    search.insert(f);
                }
            }
        }
        search.into_iter()
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub struct Piece {
    pub player: Player,
    pub p_type: PieceType,
}
