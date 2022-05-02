//! # Fltrs
//!
//! Fltrs want to support creating easy, fast and expandable filters for iterable things (like Vec, Array, Map, Set, ...) in rust.
//! A filter is created based on an input string (query).
//! This has particular advantages if the filter is created at runtime, i.e. in a GUI or command line tool (CLI).
//!
//! ### Examples:
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
//!         .filter(query(r#"name starts_with 'P' and x > 1 and y < 5"#).unwrap())
//!         .count()
//! );
//! ```
//!
//! ### Extensions:
//!
//! It is possible, to expand the filter/query to your own needs:
//! - create your own [`mod@crate::operator`]
//! - create a converter for the filter [`Value`] (e.g.: conversion of units)
//!
//! You can find examples on the [`Query`] builder page.
//!

pub mod error;
pub mod operator;
mod parser;
mod query;
mod scanner;
mod token;
pub mod value;

pub use crate::error::FltrError;
use crate::operator::{OperatorFn, Operators};
use crate::parser::{parse, AsValueFn, Parser};
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
/// ### Example
/// ```
/// use fltrs::query;
///
/// assert_eq!(
///     ["Inge", "Paul", "Peter", "Ina"],
///     ["Inge", "Paul", "Peter", "Jasmin", "Ina", "Mario"]
///                 .into_iter()
///                 .filter(query(r#"starts_with "In" or starts_with 'P'"#).unwrap())
///                 .collect::<Vec<&str>>()
///                 .as_slice(),
/// );
/// ```
pub fn query<PR: PathResolver + 'static>(query: &str) -> Result<Predicate<PR>> {
    crate::query::query(parse(query)?, &Operators::<PR>::default())
}

/// The Query is an builder to configure the [`query()`]. It is possible, to extend the Operators in the modul: [`mod@crate::operator`].
///
/// ### Example
///
/// Create your own operator:
///
/// ```
/// use fltrs::{value::Value, PathResolver, Predicate, Query, Result, query};
///
/// fn upper_eq<PR: PathResolver>(idx: usize, v: Value) -> Result<Predicate<PR>> {
///     Ok(Box::new(
///         move |pr| {
///           if let Value::Text(t) = &v {
///               return pr.value(idx).to_string().to_uppercase().eq(&t.to_uppercase());
///           }
///           false
///         }
///      ))
/// }
///
/// let query = Query::build()
///              .operators(&[("uppereq", upper_eq)])
///              .query(r#" uppereq "ab" "#)
///              .unwrap();
///
/// let result: Vec<&str> = ["yz", "aB", "Ab", "xY"].into_iter().filter(query).collect();
///
/// assert_eq!(vec!["aB", "Ab"], result);
///
/// ```
///
/// Create your own `as [convert function]` (for example: conversion of units):
///
/// ```
/// use fltrs::{value::Value, PathResolver, Predicate, Query, Result, query};
///
/// let query = Query::build()
///              .as_value_fn(&[("kbyte", |v| {
///                     if let Value::Int(x) = v {
///                         return Value::Int(x * 1024);
///                     }
///                     v
///                   }
///                 )])
///              .query(r#" > 1 as kbyte and < 6 as kbyte "#)
///              .unwrap();
///
/// // list of bytes
/// let result: Vec<i32> = [100, 1025, 7000, 4001].into_iter().filter(query).collect();
///
/// assert_eq!(vec![1025, 4001], result);
///
/// ```

pub struct Query<PR> {
    ops: Operators<PR>,
    as_value_fn: Vec<(&'static str, AsValueFn)>,
}

impl<PR: PathResolver + 'static> Query<PR> {
    pub fn build() -> Self {
        Self {
            ops: Operators::default(),
            as_value_fn: vec![],
        }
    }

    pub fn operators(mut self, ops: &[(&'static str, OperatorFn<PR>)]) -> Self {
        self.ops.op.extend_from_slice(ops);
        self
    }

    pub fn as_value_fn(mut self, fns: &[(&'static str, AsValueFn)]) -> Self {
        self.as_value_fn.extend_from_slice(fns);
        self
    }

    pub fn query(&self, query: &str) -> Result<Predicate<PR>> {
        let mut p = Parser::new(query);
        p.ops = self.ops.get_ops_names();
        p.as_value_fns = self.as_value_fn.clone();
        crate::query::query(p.parse()?, &self.ops)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(PartialEq, Debug)]
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

    #[cfg(feature = "regex")]
    #[test]
    fn iter_regex() -> Result<()> {
        assert_eq!(
            2,
            [1, 22, 333]
                .into_iter()
                .filter(query(r#"regex "[0-9]{2}""#)?)
                .count()
        );

        Ok(())
    }

    #[cfg(feature = "regex")]
    #[test]
    fn iter_point_regex() -> Result<()> {
        assert_eq!(
            1,
            [Point::new(22, 4), Point::new(3, 5)]
                .into_iter()
                .filter(query(r#"x regex "[0-9]{2}""#)?)
                .count()
        );

        Ok(())
    }

    #[test]
    fn iter_point_fltrs() -> Result<()> {
        assert_eq!(
            1,
            [Point::new(2, 4), Point::new(3, 5)]
                .into_iter()
                .filter(query("x > 1 and  y < 5")?)
                .count()
        );

        Ok(())
    }

    #[test]
    fn iter_point_one_of() -> Result<()> {
        assert_eq!(
            2,
            [Point::new(2, 4), Point::new(3, 5), Point::new(4, 6)]
                .into_iter()
                .filter(query("x one_of [1, 2, 7, 4]")?)
                .count()
        );

        Ok(())
    }
}
