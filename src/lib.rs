pub mod error;
pub mod operator;
pub mod parser;
pub mod query;
mod scanner;
mod token;
pub mod value;

pub use crate::error::FltrError;
use crate::value::Value;
use core::fmt::Display;

pub type Result<T> = core::result::Result<T, FltrError>;

pub trait Filterable: PartialEq<Value> + PartialOrd<Value> + Display {}

impl<V: PartialEq<Value> + PartialOrd<Value> + Display> Filterable for V {}

pub trait PathResolver {
    fn path_to_index(path: &str) -> Option<usize>;
    fn value(&self, idx: usize) -> &dyn Filterable;
}

impl<F: Filterable> PathResolver for F {
    fn path_to_index(_path: &str) -> Option<usize> {
        Some(0)
    }

    fn value(&self, _idx: usize) -> &dyn Filterable {
        self
    }
}

pub type Predicate<PR> = Box<dyn Fn(&PR) -> bool>;

pub fn fltrs<PR: PathResolver + 'static>(query: &str) -> Result<Predicate<PR>> {
    let exp = crate::parser::parse(query)?;
    let ops = crate::operator::Operators::<PR>::default();
    crate::query::query(exp, &ops)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn iter_i32_list_fltrs() -> Result<()> {
        assert_eq!(
            5,
            [3, 2, 1, 4, 5, 7, 5, 4, 3]
                .into_iter()
                .filter(fltrs("> 1 and  < 5")?)
                .count()
        );

        Ok(())
    }

    struct Point {
        x: i8,
        y: i16,
    }

    impl Point {
        fn new(x: i8, y: i16) -> Self {
            Self { x, y }
        }
    }

    impl PathResolver for Point {
        fn path_to_index(path: &str) -> Option<usize> {
            match path {
                "x" => Some(0),
                "y" => Some(1),
                _ => None,
            }
        }

        fn value(&self, idx: usize) -> &dyn Filterable {
            match idx {
                0 => &self.x,
                _ => &self.y,
            }
        }
    }

    #[test]
    fn iter_point_fltrs() -> Result<()> {
        let l: Vec<Point> = [Point::new(2, 4), Point::new(3, 5)]
            .into_iter()
            .filter(fltrs("x > 1 and  y < 5")?)
            .collect();
        assert_eq!(1, l.len());

        Ok(())
    }
}
