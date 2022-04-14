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
