pub mod error;
pub mod operator;
mod parser;
pub mod runtime;
mod runtime_new;
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

// TODO: remove this function, temporary for benchmarking
pub fn parse(input: &str) -> std::result::Result<crate::token::Exp, crate::error::ParseError> {
    crate::parser::parse(input)
}

// TODO: remove this function, temporary for benchmarking
pub fn create_path_executor<'a, Arg: 'a>(
    exp: crate::token::Exp,
    ops: &'a crate::operator::Operators,
) -> Box<dyn crate::runtime::Executor<'a, Arg> + 'a>
where
    Arg: PathResolver,
{
    crate::runtime::create_path_executor(exp, ops)
}

pub fn exec<'a, Arg: 'a>(
    input: &str,
    ops: &'a crate::operator::Operators,
) -> impl crate::runtime::Executor<'a, Arg>
where
    Arg: PathResolver + 'a,
{
    use runtime::*;

    let exp = parse(input).unwrap();
    Runtime::<Arg>::new::<PathExecutor>(exp, ops)
}

pub fn exec_new<'a, Arg: 'a>(
    input: &str,
    ops: &'a crate::operator::Operators,
) -> Box<dyn crate::runtime::Executor<'a, Arg> + 'a>
where
    Arg: PathResolver + 'a,
{
    let exp = parse(input).unwrap();
    crate::runtime_new::new::<crate::runtime::PathExecutor, Arg>(exp, ops)
}
