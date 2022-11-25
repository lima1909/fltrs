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
//! use fltrs::{path_resolver, Filterable, query};
//!
//! #[derive(PartialEq, Debug)]
//! struct Point {
//!     name: &'static str,
//!     x:    i32,
//!     y:    i32,
//! }
//!
//! path_resolver!(Point: name, x, y);
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
#![allow(clippy::type_complexity)]
pub mod error;
pub mod operator;
mod parser;
mod query;
mod scanner;
mod token;
pub mod value;

pub use crate::error::FltrError;
pub use crate::value::Value;

use crate::operator::{Operator, Operators, PredicateFn};
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
///     fn pathes() -> &'static [&'static str] {
///         &["name", "x", "y"]
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
/// ```

pub trait PathResolver {
    /// Returns all possible pathes (fields) of the given struct.
    fn pathes() -> &'static [&'static str];
    /// The value of the struct field for the given index of [`PathResolver::pathes`] .
    fn value(&self, idx: usize) -> &dyn Filterable;

    fn idx(path: &str) -> Result<usize> {
        Self::pathes()
            .iter()
            .enumerate()
            .find(|(_, p)| **p == path)
            .map(|(idx, _)| idx)
            .ok_or_else(|| FltrError(format!("invalid path: '{}'", path)))
    }
}

impl<F: Filterable> PathResolver for F {
    fn pathes() -> &'static [&'static str] {
        &["", "self"]
    }

    fn value(&self, _idx: usize) -> &dyn Filterable {
        self
    }
}

/// Short way to implement the trai: [`PathResolver`].
///
/// ### Example short, with macro:
/// ```
/// use fltrs::{path_resolver, Filterable};
///
/// struct Point {
///     name: &'static str,
///     x:    i32,
///     y:    i32,
/// }
///
/// path_resolver!(Point: name, x, y);
/// ```
///
/// ### Example long:
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
///     fn pathes() -> &'static [&'static str] {
///         &["name", "x", "y"]
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
/// ```

#[macro_export]
macro_rules! path_resolver {
    ( $t:ty : $path:ident ) => {
        path_resolver!(# $t : $path => 0);
    };

    ( $t:ty : $path0:ident, $path1:ident ) => {
        path_resolver!(# $t : $path0 => 0, $path1 => 1);
    };

    ( $t:ty : $path0:ident, $path1:ident, $path2:ident ) => {
        path_resolver!(# $t : $path0 => 0, $path1 => 1, $path2 => 2);
    };

    ( $t:ty : $path0:ident, $path1:ident, $path2:ident, $path3:ident ) => {
        path_resolver!(# $t : $path0 => 0, $path1 => 1, $path2 => 2, $path3 => 3);
    };

    ( $t:ty : $path0:ident, $path1:ident, $path2:ident, $path3:ident, $path4:ident ) => {
        path_resolver!(# $t : $path0 => 0, $path1 => 1, $path2 => 2, $path3 => 3, $path4 => 4);
    };


    ( # $t:ty : $($path:ident => $idx:expr), + ) => {
        impl $crate::PathResolver for $t {

            fn pathes() -> &'static [&'static str] {
                &[ $( (stringify!($path)), )+ ]
            }

            fn value(&self, idx: usize) -> &dyn Filterable {
                match idx {
                    $( $idx => &self.$path, )+
                    _ => unreachable!("invalid index: {}", idx),
                }
            }
        }
    };
}

/// A Predicate is an boxed [`core::ops::Fn`].
pub type Predicate<PR> = Box<dyn Fn(&PR) -> bool>;

/// Create a [`Predicate`] with which you can
/// execute a filter on a given iterator, created with `into_iter` ([`std::iter#the-three-forms-of-iteration`]).
///
/// ### Example
/// ```
/// use fltrs::query;
///
/// assert_eq!(
///     ["Inge", "Paul", "Peter", "Jasmin", "Ina", "Mario"]
///            .into_iter()
///            .filter(query(r#"starts_with "In" or starts_with 'P'"#).unwrap())
///            .collect::<Vec<&str>>()
///            .as_slice(),
///     ["Inge", "Paul", "Peter", "Ina"]
/// );
/// ```
pub fn query<PR: PathResolver + 'static>(query: &str) -> Result<Predicate<PR>> {
    crate::query::query(parse(query)?, &Operators::default())
}

/// Create a [`Predicate`] with which you can
/// execute a filter on a given iterator, created with `iter` ([`std::iter#the-three-forms-of-iteration`]).
///
/// ### Example
/// ```
/// use fltrs::query_ref;
///
/// assert_eq!(
///     ["Inge", "Paul", "Peter", "Jasmin", "Ina", "Mario"]
///            .iter()
///            .filter(query_ref(r#"starts_with "In" or starts_with 'P'"#).unwrap())
///            .collect::<Vec<&&str>>()
///            .as_slice(),
///     [&"Inge", &"Paul", &"Peter", &"Ina"]
/// );
/// ```
pub fn query_ref<PR: PathResolver + 'static>(query: &str) -> Result<impl Fn(&&PR) -> bool> {
    let f = crate::query::query(parse(query)?, &Operators::default())?;
    Ok(move |pr: &&PR| f(*pr))
}

/// The Query is an builder to configure the [`query()`]. It is possible, to extend the Operators in the modul: [`mod@crate::operator`].
///
/// ### Example
///
/// Create your own operator:
///
/// ```
/// use fltrs::{Filterable, PathResolver, Predicate, Query, Result, query};
/// use fltrs::operator::PredicateFn;
/// use fltrs::value::Value;
///
/// fn upper_eq(v: Value) -> Result<PredicateFn> {
///   match v {
///     Value::Text(t) => Ok(Box::new(move |f: &dyn Filterable| {
///         f.as_string().to_uppercase().eq(&t.to_uppercase())
///     })),
///     _ => Ok(Box::new(move |_: &dyn Filterable| false)),
///   }
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

pub struct Query {
    ops: Operators,
    as_value_fn: Vec<(&'static str, AsValueFn)>,
}

impl Query {
    pub fn build() -> Self {
        Self {
            ops: Operators::default(),
            as_value_fn: vec![],
        }
    }

    pub fn operators(mut self, ops: &[(&'static str, fn(Value) -> Result<PredicateFn>)]) -> Self {
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

    pub fn query<PR: PathResolver + 'static>(&self, query: &str) -> Result<Predicate<PR>> {
        let mut p = Parser::new(query);
        p.ops = self.ops.ops_names();
        p.as_value_fns = self.as_value_fn.clone();
        crate::query::query(p.parse()?, &self.ops)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    #[test]
    fn simple_exec_query() -> Result<()> {
        assert!(query(r#" = "abc" "#)?(&"abc"));
        Ok(())
    }

    #[test]
    fn iter_string_ref() {
        let v = vec![String::from("a"), String::from("b"), String::from("c")];
        let result: Vec<&String> = v.iter().filter(query_ref(r#" = "a" "#).unwrap()).collect();
        assert_eq!(vec![&String::from("a")], result);
    }

    #[test]
    fn iter_str_ref() {
        let result: Vec<&&str> = ["a", "b", "c"]
            .iter()
            .filter(query_ref(r#" = "a" "#).unwrap())
            .collect();
        assert_eq!(vec![&"a"], result);
    }

    #[test]
    fn iter_u8_ref() {
        let result: Vec<&u8> = [1, 5, 4]
            .iter()
            .filter(query_ref(" != 5 ").unwrap())
            .collect();
        assert_eq!(vec![&1, &4], result);
    }

    #[test]
    fn iter_char_space() -> Result<()> {
        let result: Vec<char> = [' ', 'a', ' ', 'b', ' ']
            .into_iter()
            .filter(query(r#"= ' '"#)?)
            .collect();
        assert_eq!(vec![' ', ' ', ' '], result);

        Ok(())
    }

    #[derive(PartialEq, Debug)]
    struct Car {
        name: String,
        ps: u16,
    }

    impl Car {
        fn new(name: &str, ps: u16) -> Self {
            Self {
                name: name.to_owned(),
                ps,
            }
        }
    }

    path_resolver!(Car: name, ps);

    #[test]
    fn iter_cars() -> Result<()> {
        let cars = [Car::new("Porsche", 345), Car::new("Audi", 234)];

        let result: Vec<_> = cars
            .iter()
            .filter(query_ref(" name starts_with:i 'p' ")?)
            .collect();
        assert_eq!(vec![&Car::new("Porsche", 345)], result);

        let result: Vec<_> = cars.iter().filter(query_ref(" ps < 300")?).collect();
        assert_eq!(vec![&Car::new("Audi", 234)], result);

        Ok(())
    }

    #[test_case(" > 1 " => vec![Some(2), Some(3)] ; "> 1" )]
    #[test_case(" one_of [1, 2] " => vec![Some(1), Some(2)] ; "one_of 1 2" )]
    #[test_case(" one_of [1, none] " => vec![None, Some(1), None, None] ; "one_of 1 none" )]
    #[test_case(" one_of [] " => Vec::<Option<i32>>::new() ; "one_of []" )]
    #[test_case(" one_of 1" => vec![Some(1)] ; "one_of 1" )]
    #[test_case(" one_of none" => vec![None, None, None] ; "one_of none" )]
    #[test_case(" = none" => vec![None, None, None] ; "eq none" )]
    #[test_case(" = None" => vec![None, None, None] ; "eq upper None" )]
    #[test_case(" = null" => vec![None, None, None] ; "eq null" )]
    #[test_case(" = Null" => vec![None, None, None] ; "eq upper Null" )]
    #[test_case(" not = none " => vec![Some(1), Some(2), Some(3)] ; "not none" )]
    #[test_case(" not < 2" => vec![None, None, Some(2), Some(3), None] ; "not less 2" )]
    #[test_case(" != 2" => vec![None, Some(1), None, Some(3), None] ; "neq 2" )]
    #[test_case(" != '2'" => vec![None, Some(1), None, Some(2), Some(3), None] ; "neq char 2" )]
    fn into_iter_option(query_str: &str) -> Vec<Option<i32>> {
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

    path_resolver!(Point: x, y);

    #[cfg(feature = "regex")]
    #[test]
    fn into_iter_regex() -> Result<()> {
        let result: Vec<_> = [1, 22, 333]
            .into_iter()
            .filter(query(r#"regex "[0-9]{2}""#)?)
            .collect();

        assert_eq!(vec![22, 333], result);
        Ok(())
    }

    #[cfg(feature = "regex")]
    #[test]
    fn into_iter_point_regex() -> Result<()> {
        let result: Vec<_> = [Point::new(22, 4), Point::new(3, 5)]
            .into_iter()
            .filter(query(r#"x regex "[0-9]{2}""#)?)
            .collect();

        assert_eq!(vec![Point::new(22, 4)], result);
        Ok(())
    }

    #[test]
    fn into_iter_point_fltrs() -> Result<()> {
        let result: Vec<_> = [Point::new(2, 4), Point::new(3, 5)]
            .into_iter()
            .filter(query("x > 1 and  y < 5")?)
            .collect();

        assert_eq!(vec![Point::new(2, 4)], result);
        Ok(())
    }

    #[test]
    fn into_iter_point_one_of() -> Result<()> {
        let result: Vec<_> = [Point::new(2, 4), Point::new(3, 5), Point::new(4, 6)]
            .into_iter()
            .filter(query("x one_of [1, 2, 7, 4]")?)
            .collect();

        assert_eq!(vec![Point::new(2, 4), Point::new(4, 6)], result);
        Ok(())
    }

    #[test]
    fn into_iter_str_empty() -> Result<()> {
        let result: Vec<&str> = ["", "abc", "", "xyz", ""]
            .into_iter()
            .filter(query(r#"= """#)?)
            .collect();

        assert_eq!(vec!["", "", ""], result);
        Ok(())
    }

    #[test]
    fn into_iter_str_not_empty() -> Result<()> {
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
    fn into_iter_str_one_of_empty() -> Result<()> {
        let result: Vec<&str> = ["", "abc", "", "xyz", ""]
            .into_iter()
            .filter(query(r#"one_of [""]"#)?)
            .collect();

        assert_eq!(vec!["", "", ""], result);
        Ok(())
    }

    #[test]
    fn into_iter_contains_case_intensitive() -> Result<()> {
        let result: Vec<&str> = ["abc", "aBc", "xyz", "Xyz", ""]
            .into_iter()
            .filter(query("contains:i 'b'")?)
            .collect();

        assert_eq!(vec!["abc", "aBc"], result);
        Ok(())
    }

    #[test]
    fn into_iter_starts_with_case_intensitive() -> Result<()> {
        let result: Vec<&str> = ["abc", "aBc", "xyz", "Xyz", ""]
            .into_iter()
            .filter(query(r#"starts_with:i 'x'"#)?)
            .collect();

        assert_eq!(vec!["xyz", "Xyz"], result);
        Ok(())
    }

    #[test]
    fn into_iter_ends_with_case_intensitive() -> Result<()> {
        let result: Vec<&str> = ["abC", "aBc", "xyz", "Xyz", ""]
            .into_iter()
            .filter(query(r#"ends_with:i "bc""#)?)
            .collect();

        assert_eq!(vec!["abC", "aBc"], result);
        Ok(())
    }

    #[test]
    fn into_iter_greater_char_case_intensitive() -> Result<()> {
        let result: Vec<_> = ['b', 'B', 'x', 'X']
            .into_iter()
            .filter(query(">:i 'w'")?)
            .collect();

        assert_eq!(vec!['x', 'X'], result);
        Ok(())
    }

    #[test]
    fn into_iter_str_one_of_case_intensitive() -> Result<()> {
        let result: Vec<&str> = ["", "aBc", "xyz", "abC", ""]
            .into_iter()
            .filter(query(r#"one_of:i ["abc", "sdf"]"#)?)
            .collect();

        assert_eq!(vec!["aBc", "abC"], result);
        Ok(())
    }

    #[test]
    fn into_iter_greater_int_case_intensitive_err() {
        assert_eq!(
            query::<i32>(">:i 2").err().unwrap(),
            FltrError("the operation '>' with the flag: 'i' supported only 'String' and 'char' values, not: '2'".into())
        )
    }

    #[test]
    fn into_iter_int_one_of_case_intensitive_err() {
        assert_eq!(
            query::<i32>("one_of:i [7, 9]").err().unwrap(),
            FltrError(
                "the operation 'one_of' with the flag: 'i' supported only 'String' and 'char' values, not: '[Int(7), Int(9)]'"
                    .into()
            )
        )
    }

    #[test]
    fn into_iter_len_case_intensitive_err() {
        assert_eq!(
            query::<&str>("len:i 3").err().unwrap(),
            FltrError("the operation 'len' with the flag: 'i' supported only 'String' and 'char' values, not: '3'".into())
        )
    }
}
