//! # Fltrs
//!
//! *Easy to define filters for querying lists.* `Fltrs` has **no** dependencies!
//!
//! ## Overview
//!
//! Fltrs want to support creating easy, fast and expandable filters for iterable things (like Vec, Array, Map, Set, ...) in rust.
//! A filter is created based on an input string (query).
//! This has particular advantages if the filter is created at runtime, i.e. in a GUI or command line tool (CLI).
//!
//! ## Extensions:
//!
//! It is possible, to expand the filter/query to your own needs:
//! - create your own [`mod@crate::operator`]
//! - create a converter for the filter [`Value`] (e.g.: conversion of units).
//!
//! You can find examples on the [`Query`] builder page.
//!
//! ## Examples:
//!
//! ```
//! use fltrs::query;
//!
//! let result: Vec<_> = [3, 2, 1, 4, 5, 7, 5, 4, 3]
//!         .into_iter()
//!         .filter(query("> 1 and < 5").unwrap())
//!         .collect();
//!
//! assert_eq!(vec![3, 2, 4, 4, 3], result);
//! ```
//!
//! ```
//! use fltrs::query;
//!
//! let result: Vec<_> = ["Inge", "Petra", "Paul", "Egon", "Peter"]
//!         .into_iter()
//!         .filter(query("contains 'e'").unwrap())
//!         .collect();
//!
//! assert_eq!(vec!["Inge", "Petra", "Peter"], result);
//!
//! // or case insensitive (`contains` with flag: `i`)
//! let result: Vec<_> = ["Inge", "Petra", "Paul", "Egon", "Peter"]
//!         .into_iter()
//!         .filter(query("contains:i 'e'").unwrap())
//!         .collect();
//!
//! assert_eq!(vec!["Inge", "Petra", "Egon", "Peter"], result);
//! ```
//!
//! ### Option queries:
//!
//! ```
//! use fltrs::query;
//!
//! let result: Vec<Option<char>> = [None, Some('a'), None, Some('b'), Some('c'), Some('a')]
//!         .into_iter()
//!         .filter(query(" != 'a' and not = none ").unwrap())
//!         .collect();
//!
//! assert_eq!(vec![Some('b'), Some('c')], result);
//! ```
//!
//! ### Nested and Not queries:
//!
//! ```
//! use fltrs::query;
//!
//! let result: Vec<_> = [3, 2, 1, 4, 5, 7, 5, 4, 3]
//!         .into_iter()
//!         .filter(query("(= 1 or = 5) and > 1").unwrap())
//!         .collect();
//!
//! assert_eq!(vec![5, 5], result);
//!```
//!
//!```
//! use fltrs::query;
//!
//! let result: Vec<_> = [3, 2, 1, 4, 5, 7, 5, 4, 3]
//!         .into_iter()
//!         .filter(query("not( (= 1 or = 5) and > 1)").unwrap())
//!         .collect();
//!
//! assert_eq!(vec![3, 2, 1, 4, 7, 4, 3], result);
//! ```
//!
//! ### Fltrs supported queries on structs too.
//!
//! This is possible, if the struct implement the trait: [`PathResolver`].
//!
//! ```
//! use fltrs::{PathResolver, Filterable, query};
//!
//! #[derive(PartialEq, Debug)]
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
//! let result: Vec<Point> =
//!     [
//!       Point { name: "Point_1_3", x: 1, y: 3},
//!       Point { name: "Point_3_3", x: 3, y: 3},
//!       Point { name: "Point_2_6", x: 2, y: 6},
//!     ]
//!      .into_iter()
//!      .filter(query(r#"x one_of [3, 7]"#).unwrap())
//!      .collect();
//!
//! assert_eq!(vec![Point { name: "Point_3_3", x: 3, y: 3}], result);
//! ```
//!

pub mod error;
pub mod operator;
mod parser;
mod query;
mod scanner;
mod token;
pub mod value;

pub use crate::error::FltrError;
pub use crate::value::Value;

use crate::operator::{Operator, OperatorFn, Operators};
use crate::parser::{parse, AsValueFn, Parser};

/// The default [`core::result::Result`] with the error: [`FltrError`].
pub type Result<T> = core::result::Result<T, FltrError>;

/// Is a replacement for the [`std::fmt::Display`] trait.
/// It is not possible to implement `Display` for the enum [`std::option::Option`].
pub trait AsString {
    fn as_string(&self) -> String;
}

macro_rules! as_string {
    ( $($t:ty) + ) => {
    $(
        impl AsString for $t {
            fn as_string(&self) -> String {
                self.to_string()
            }
        }

        impl AsString for Option<$t> {
            fn as_string(&self) -> String {
                match self {
                    Some(v) => v.to_string(),
                    None => String::new(),
                }
            }
        }

    ) *

    }
}

as_string! { bool char &str String usize u8 u16 u32 u64 u128 isize i8 i16 i32 i64 f32 f64 }

/// Filterable means, the given value can be compared to [`Value`] and implement the trait [`core::fmt::Display`].
pub trait Filterable: PartialEq<Value> + PartialOrd<Value> + AsString {}

impl<V: PartialEq<Value> + PartialOrd<Value> + AsString> Filterable for V {}

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
/// use fltrs::{value::Value, PathResolver, Predicate, Query, Result, query, operator::FlagResolver};
///
/// fn upper_eq<PR: PathResolver>(fr: FlagResolver) -> Result<Predicate<PR>> {
///     Ok(Box::new(
///         move |pr| {
///             fr.handle(pr, |f, v| match v {
///                 Value::Text(t) => f.as_string().to_uppercase().eq(&t.to_uppercase()),
///                 _ => false,
///             })
///         }
///     ))
/// }
///
/// let query = Query::build()
///              .operators(&[("upper_eq", upper_eq)])
///              .query(r#" upper_eq "ab" "#)
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
        self.ops.ops = ops
            .iter()
            .map(|(n, op)| Operator::new(n, *op, &[]))
            .collect();
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
    use test_case::test_case;

    #[test]
    fn iter_char_space() -> Result<()> {
        let result: Vec<char> = [' ', 'a', ' ', 'b', ' ']
            .into_iter()
            .filter(query(r#"= ' '"#)?)
            .collect();
        assert_eq!(vec![' ', ' ', ' '], result);

        Ok(())
    }

    #[test_case(" > 1 " => vec![Some(2), Some(3)] ; "> 1" )]
    #[test_case(" one_of [1, 2] " => vec![Some(1), Some(2)] ; "one_of 1 2" )]
    #[test_case(" one_of [1, none] " => vec![None, Some(1), None, None] ; "one_of 1 none" )]
    #[test_case(" one_of [] " => Vec::<Option<i32>>::new() ; "one_of []" )]
    #[test_case(" = none" => vec![None, None, None] ; "eq none" )]
    #[test_case(" = None" => vec![None, None, None] ; "eq upper None" )]
    #[test_case(" = null" => vec![None, None, None] ; "eq null" )]
    #[test_case(" = Null" => vec![None, None, None] ; "eq upper Null" )]
    #[test_case(" not = none " => vec![Some(1), Some(2), Some(3)] ; "not none" )]
    #[test_case(" not < 2" => vec![None, None, Some(2), Some(3), None] ; "not less 2" )]
    #[test_case(" != 2" => vec![None, Some(1), None, Some(3), None] ; "neq 2" )]
    fn iter_option(query_str: &str) -> Vec<Option<i32>> {
        let result: Vec<Option<i32>> = [None, Some(1), None, Some(2), Some(3), None]
            .into_iter()
            .filter(query(query_str).unwrap())
            .collect();
        result
    }

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
        let result: Vec<_> = [1, 22, 333]
            .into_iter()
            .filter(query(r#"regex "[0-9]{2}""#)?)
            .collect();

        assert_eq!(vec![22, 333], result);
        Ok(())
    }

    #[cfg(feature = "regex")]
    #[test]
    fn iter_point_regex() -> Result<()> {
        let result: Vec<_> = [Point::new(22, 4), Point::new(3, 5)]
            .into_iter()
            .filter(query(r#"x regex "[0-9]{2}""#)?)
            .collect();

        assert_eq!(vec![Point::new(22, 4)], result);
        Ok(())
    }

    #[test]
    fn iter_point_fltrs() -> Result<()> {
        let result: Vec<_> = [Point::new(2, 4), Point::new(3, 5)]
            .into_iter()
            .filter(query("x > 1 and  y < 5")?)
            .collect();

        assert_eq!(vec![Point::new(2, 4)], result);
        Ok(())
    }

    #[test]
    fn iter_point_one_of() -> Result<()> {
        let result: Vec<_> = [Point::new(2, 4), Point::new(3, 5), Point::new(4, 6)]
            .into_iter()
            .filter(query("x one_of [1, 2, 7, 4]")?)
            .collect();

        assert_eq!(vec![Point::new(2, 4), Point::new(4, 6)], result);
        Ok(())
    }

    #[test]
    fn iter_str_empty() -> Result<()> {
        let result: Vec<&str> = ["", "abc", "", "xyz", ""]
            .into_iter()
            .filter(query(r#"= """#)?)
            .collect();

        assert_eq!(vec!["", "", ""], result);
        Ok(())
    }

    #[test]
    fn iter_str_not_empty() -> Result<()> {
        let result: Vec<&str> = ["", "abc", "", "xyz", ""]
            .into_iter()
            .filter(query(r#"not = "" "#)?)
            .collect();
        assert_eq!(vec!["abc", "xyz"], result);

        let result: Vec<&str> = ["", "abc", "", "xyz", ""]
            .into_iter()
            .filter(query(r#"not ( = "")"#)?)
            .collect();
        assert_eq!(vec!["abc", "xyz"], result);

        let result: Vec<&str> = ["", "abc", "", "xyz", ""]
            .into_iter()
            .filter(query(r#"!= "" "#)?)
            .collect();
        assert_eq!(vec!["abc", "xyz"], result);

        Ok(())
    }

    #[test]
    fn iter_str_one_of_empty() -> Result<()> {
        let result: Vec<&str> = ["", "abc", "", "xyz", ""]
            .into_iter()
            .filter(query(r#"one_of [""]"#)?)
            .collect();

        assert_eq!(vec!["", "", ""], result);
        Ok(())
    }

    #[test]
    fn iter_contains_case_intensitive() -> Result<()> {
        let result: Vec<&str> = ["abc", "aBc", "xyz", "Xyz", ""]
            .into_iter()
            .filter(query("contains:i 'b'")?)
            .collect();

        assert_eq!(vec!["abc", "aBc"], result);
        Ok(())
    }

    #[test]
    fn iter_starts_with_case_intensitive() -> Result<()> {
        let result: Vec<&str> = ["abc", "aBc", "xyz", "Xyz", ""]
            .into_iter()
            .filter(query(r#"starts_with:i 'x'"#)?)
            .collect();

        assert_eq!(vec!["xyz", "Xyz"], result);
        Ok(())
    }

    #[test]
    fn iter_ends_with_case_intensitive() -> Result<()> {
        let result: Vec<&str> = ["abC", "aBc", "xyz", "Xyz", ""]
            .into_iter()
            .filter(query(r#"ends_with:i "bc""#)?)
            .collect();

        assert_eq!(vec!["abC", "aBc"], result);
        Ok(())
    }

    #[test]
    fn iter_greater_char_case_intensitive() -> Result<()> {
        let result: Vec<_> = ['b', 'B', 'x', 'X']
            .into_iter()
            .filter(query(">:i 'w'")?)
            .collect();

        assert_eq!(vec!['x', 'X'], result);
        Ok(())
    }

    #[test]
    fn iter_str_one_of_case_intensitive() -> Result<()> {
        let result: Vec<&str> = ["", "aBc", "xyz", "abC", ""]
            .into_iter()
            .filter(query(r#"one_of:i ["abc", "sdf"]"#)?)
            .collect();

        assert_eq!(vec!["aBc", "abC"], result);
        Ok(())
    }

    #[test]
    fn iter_greater_int_case_intensitive_err() {
        assert_eq!(
            query::<i32>(">:i 2").err().unwrap(),
            FltrError("the flag: 'i' supported only 'String' and 'char' values, not: '2'".into())
        )
    }

    #[test]
    fn iter_int_one_of_case_intensitive_err() {
        assert_eq!(
            query::<i32>("one_of:i [7, 9]").err().unwrap(),
            FltrError(
                "the flag: 'i' supported only 'String' and 'char' values, not: '[Int(7), Int(9)]'"
                    .into()
            )
        )
    }

    #[test]
    fn iter_len_case_intensitive_err() {
        assert_eq!(
            query::<&str>("len:i 3").err().unwrap(),
            FltrError("the flag: 'i' is for operator 'len' not supported".into())
        )
    }
}
