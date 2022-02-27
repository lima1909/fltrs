pub mod error;
pub mod operator;
mod parser;
mod runtime;
mod scanner;
mod token;
pub mod value;

use core::fmt::Display;

use crate::error::FltrError;
use crate::value::Value;

/// This is the default Result for the Filter trait definition.
/// The return value can be an `Ok(T)` or an error `Err(FltrError)`.
pub type Result<T> = core::result::Result<T, FltrError>;

pub trait Filterable: PartialEq<Value> + PartialOrd<Value> + Display {}

pub trait PathResolver {
    fn path_to_index(&self, path: &str) -> Option<usize>;
    fn value(&self, idx: usize) -> &dyn Filterable;
}

impl Filterable for String {}
impl Filterable for &str {}
impl Filterable for char {}
impl Filterable for bool {}
impl Filterable for u32 {}
impl Filterable for i32 {}
impl Filterable for i64 {}
impl Filterable for f32 {}
impl Filterable for f64 {}
