pub mod error;
pub mod operator;
mod parser;
mod runtime;
mod scanner;
mod token;
pub mod value;

use crate::error::FltrError;
use crate::value::RefValue;

/// This is the default Result for the Filter trait definition.
/// The return value can be an `Ok(T)` or an error `Err(FltrError)`.
pub type Result<T> = core::result::Result<T, FltrError>;

pub trait PathResolver {
    fn path_to_index(&self, path: &str) -> Option<usize>;
    fn value(&self, idx: usize) -> RefValue;
}
