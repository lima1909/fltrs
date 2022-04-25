//! # Fltrs
//!
//! Fltrs want to support creating easy and fast filters for iterable things (like Vec, Array, Map, Set, ...) in rust.
//! A filter is created based on an input string (query).
//! This has particular advantages if the filter is created at runtime, i.e. in a GUI or command line tool (CLI).
//!
//! ### Example:
//! ```
//! use fltrs::query;
//!
//! assert_eq!(
//!     5,
//!     [3, 2, 1, 4, 5, 7, 5, 4, 3]
//!         .into_iter()
//!         .filter(query("> 1 and < 5").unwrap())
//!         .count()
//! );
//! ```
//!
//! Fltrs supported queries on structs too. This is possible, if the struct implement the trait: [`PathResolver`].
//!
//! ### Example:
//! ```
//! use fltrs::{PathResolver, Filterable, query};
//!
//! struct Point {
//!     name: &'static str,
//!     x:    i32,
//!     y:    i32,
//! }
//!
//! impl PathResolver for Point {
//!     fn path_to_index(path: &str) -> Option<usize> {
//!         match path {
//!             "name"  => Some(0),
//!             "x"     => Some(1),
//!             "y"     => Some(2),
//!             _ => None,
//!         }
//!     }
//!
//!     fn value(&self, idx: usize) -> &dyn Filterable {
//!         match idx {
//!             0 => &self.name,
//!             1 => &self.x,
//!             _ => &self.y,
//!         }
//!     }
//! }
//!
//!
//! assert_eq!(
//!     1,
//!     [
//!       Point { name: "Point_1_3", x: 1, y: 3},
//!       Point { name: "Point_2_3", x: 2, y: 3},
//!       Point { name: "Point_2_6", x: 2, y: 6},
//!     ]
//!         .into_iter()
//!         .filter(query(r#"name starts_with "Point" and x > 1 and y < 5"#).unwrap())
//!         .count()
//! );
//! ```
//!
//!
pub mod error;
pub mod operator;
mod parser;
mod query;
mod scanner;
mod token;
pub mod value;

pub use crate::error::FltrError;
use crate::value::Value;
use core::fmt::Display;

/// The default [`core::result::Result`] with the error: [`FltrError`].
pub type Result<T> = core::result::Result<T, FltrError>;

/// Filterable means, the given value can be compared to [`Value`] and implement the trait [`core::fmt::Display`].
pub trait Filterable: PartialEq<Value> + PartialOrd<Value> + Display {}

impl<V: PartialEq<Value> + PartialOrd<Value> + Display> Filterable for V {}

/// PathResolver is a possibility to get the value from a field of an given struct.
///
/// ### Example:
/// ```
/// use fltrs::{PathResolver, Filterable};
///
/// struct Point {
///     name: &'static str,
///     x:    i32,
///     y:    i32,
/// }
///
/// impl PathResolver for Point {
///     fn path_to_index(path: &str) -> Option<usize> {
///         match path {
///             "name"  => Some(0),
///             "x"     => Some(1),
///             "y"     => Some(2),
///             _ => None,
///         }
///     }
///
///     fn value(&self, idx: usize) -> &dyn Filterable {
///         match idx {
///             0 => &self.name,
///             1 => &self.x,
///             _ => &self.y,
///         }
///     }
/// }

pub trait PathResolver {
    /// Is the mapping from a path (struct field name) to an index (that is used by the value-function).
    /// If the path is not a valid, than is the return value: `None`.
    fn path_to_index(path: &str) -> Option<usize>;
    /// The value of the struct field with the given index.
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

/// A Predicate is an boxed [`core::ops::Fn`].
pub type Predicate<PR> = Box<dyn Fn(&PR) -> bool>;

/// The `query` function create a [`Predicate`] respectively [`core::ops::Fn`] with which you can
/// execute a filter on a given slice.
///
/// # Example
/// ```
/// use fltrs::query;
///
/// assert_eq!(
///     ["Inge", "Paul", "Peter", "Ina"],
///     ["Inge", "Paul", "Peter", "Jasmin", "Ina", "Mario"]
///                 .into_iter()
///                 .filter(query(r#"starts_with "I" or starts_with "P""#).unwrap())
///                 .collect::<Vec<&str>>()
///                 .as_slice(),
/// );
/// ```
pub fn query<PR: PathResolver + 'static>(query: &str) -> Result<Predicate<PR>> {
    let exp = crate::parser::parse(query)?;
    let ops = crate::operator::Operators::<PR>::default();
    crate::query::query(exp, &ops)
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
    fn iter_point_fltrs() -> Result<()> {
        let l: Vec<Point> = [Point::new(2, 4), Point::new(3, 5)]
            .into_iter()
            .filter(query("x > 1 and  y < 5")?)
            .collect();
        assert_eq!(1, l.len());

        Ok(())
    }
}
