pub mod error;
pub mod operator;
pub mod parser;
pub mod query;
mod scanner;
mod token;
pub mod value;

use core::fmt::Display;

pub use crate::error::FltrError;
use crate::value::Value;

/// This is the default Result for the Filter trait definition.
/// The return value can be an `Ok(T)` or an error `Err(FltrError)`.
pub type Result<T> = core::result::Result<T, FltrError>;

pub type Predicate<PR> = Box<dyn Fn(&PR) -> bool>;

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

pub fn fltrs<PR: PathResolver + 'static>(query: &str) -> Predicate<PR> {
    let exp = crate::parser::parse(query).unwrap();
    let ops = crate::operator::Operators::<PR>::default();
    let q = crate::query::query(exp, &ops).unwrap();
    q.predicate
}

#[cfg(test)]
mod test {
    use super::*;

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
    fn iter_point_fltrs() {
        let l = [Point::new(2, 4), Point::new(3, 5)];
        // let l: Vec<&Point> = l.iter().filter(|p| p.x == 5).collect();
        let l: Vec<Point> = l.into_iter().filter(fltrs("x > 1 and  y < 5")).collect();
        assert_eq!(l.len(), 1);
    }

    #[test]
    fn iter_i32_list_fltrs() {
        let l: Vec<i32> = [3, 2, 1, 4, 5, 7, 5, 4, 3]
            .into_iter()
            .filter(fltrs("> 1 and  < 5"))
            .collect();
        assert_eq!(l.len(), 5)
    }
}
