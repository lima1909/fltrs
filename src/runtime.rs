#![allow(dead_code)] // TODO: remove this

use crate::error::FltrError;
use crate::operator::{OperatorFn, Operators};
use crate::token::{Exp, Filter};
use crate::value::{RefValue, Value};
use crate::{PathResolver, Result};

// one filter
// two filter: OR or AND
// ...
// filter
// or
// and
// |
// | | or
// ||  and
// | || and or
// || | and or
// || || and and or
// | || || and and or or

#[allow(unused_variables)]
fn create_or<'a, Arg>(
    exp: Exp,
    ops: &'a Operators<RefValue<'a>>,
) -> Or<ExecForObjectPath<'a>, ExecForObjectPath<'a>> {
    let mut it = exp.ands.into_iter();

    let first = match &it.next().unwrap().filter {
        // Filter::Predicate(p) => ExecForValue::new(p.value.clone(), ops.get(&p.op).unwrap()),
        Filter::Predicate(p) => ExecForObjectPath::new(
            p.path.clone().unwrap(),
            p.value.clone(),
            ops.get(&p.op).unwrap(),
        ),
        _ => todo!(),
    };

    let filter = match &it.next().unwrap().filter {
        // Filter::Predicate(p) => ExecForValue::new(p.value.clone(), ops.get(&p.op).unwrap()),
        Filter::Predicate(p) => ExecForObjectPath::new(
            p.path.clone().unwrap(),
            p.value.clone(),
            ops.get(&p.op).unwrap(),
        ),
        _ => todo!(),
    };

    Or(first, filter)
}

pub trait Executor<'a, Arg> {
    fn prepare(&mut self, _arg: &'a Arg) -> Result<bool> {
        Ok(true)
    }

    fn exec(&self, arg: &'a Arg) -> bool;
}

pub(crate) struct ExecForValue<'a, Arg> {
    value: Value,
    f: &'a OperatorFn<Arg>,
}

impl<'a, Arg> ExecForValue<'a, Arg> {
    pub(crate) fn new(value: Value, f: &'a OperatorFn<Arg>) -> Self {
        Self { value, f }
    }
}

impl<'a, Arg> Executor<'a, Arg> for ExecForValue<'a, Arg> {
    fn exec(&self, arg: &'a Arg) -> bool {
        (self.f)(arg, &self.value)
    }
}

struct ExecForObjectPath<'a> {
    path: String,
    index: usize,
    value: Value,
    f: &'a OperatorFn<RefValue<'a>>,
}

impl<'a> ExecForObjectPath<'a> {
    pub(crate) fn new(path: String, value: Value, f: &'a OperatorFn<RefValue<'a>>) -> Self {
        Self {
            path,
            index: 0,
            value,
            f,
        }
    }
}

impl<'a, 'pr: 'a, PR: 'pr> Executor<'a, PR> for ExecForObjectPath<'a>
where
    PR: PathResolver,
{
    fn prepare(&mut self, pr: &'a PR) -> Result<bool> {
        if let Some(idx) = pr.path_to_index(&self.path) {
            self.index = idx;
            return Ok(true);
        }
        Err(FltrError(format!("invalid path: {}", self.path)))
    }

    fn exec(&self, pr: &'a PR) -> bool {
        let arg = pr.value(self.index);
        (self.f)(&arg, &self.value)
    }
}

pub struct Or<L, R>(pub L, pub R);

impl<'a, Arg, L, R> Executor<'a, Arg> for Or<L, R>
where
    L: Executor<'a, Arg>,
    R: Executor<'a, Arg>,
{
    fn prepare(&mut self, arg: &'a Arg) -> Result<bool> {
        self.0.prepare(arg)?;
        self.1.prepare(arg)
    }

    fn exec(&self, arg: &'a Arg) -> bool {
        self.0.exec(arg) || self.1.exec(arg)
    }
}

pub struct And<L, R>(pub L, pub R);

impl<'a, Arg, L, R> Executor<'a, Arg> for And<L, R>
where
    L: Executor<'a, Arg>,
    R: Executor<'a, Arg>,
{
    fn prepare(&mut self, arg: &'a Arg) -> Result<bool> {
        self.0.prepare(arg)?;
        self.1.prepare(arg)
    }

    fn exec(&self, arg: &'a Arg) -> bool {
        self.0.exec(arg) && self.1.exec(arg)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::operator::Operators;
    use crate::parser::{parse, predicate, Parser};
    use test_case::test_case;

    #[test_case("= 7", true; "eq 7")]
    #[test_case("> 6", true; "gt 6")]
    #[test_case("< 8", true; "lt 8")]
    #[test_case("len 1 as usize", true; "len 1 usize")]
    #[test_case("len 1", true; "len 1")]
    #[test_case(r#"starts_with "7""#, true; "starts_with 7")]
    #[test_case("self != 8", true; "self ne 8")]
    #[test_case("= 8", false; "eq 8")]
    #[test_case("> 7", false; "gt 7")]
    #[test_case("< 7", false; "lt 7")]
    fn simple_exec_i32(input: &str, expect: bool) {
        let mut parser = Parser::new(input);
        let p = predicate()(&mut parser).unwrap();
        let ops = Operators::default();
        let e = ExecForValue::new(p.value.clone(), ops.get(&p.op).unwrap());
        assert_eq!(expect, e.exec(&7));
    }

    #[test_case(r#"= "Jasmin""#, true; "eq Jasmin")]
    #[test_case(r#"< "jasmin""#, true; "lt jasmin")]
    #[test_case(r#"> "Ina""#, true; "lt Ina")]
    #[test_case(r#"len 6"#, true; "len 6")]
    #[test_case(r#"starts_with "J""#, true; "starts_with J")]
    fn simple_exec_string(input: &str, expect: bool) {
        // assert!('c' > 'C');
        let mut parser = Parser::new(input);
        let p = predicate()(&mut parser).unwrap();
        let ops = Operators::default();
        let e = ExecForValue::new(p.value.clone(), ops.get(&p.op).unwrap());
        assert_eq!(expect, e.exec(&"Jasmin"));
    }

    struct Car<'a> {
        name: &'a str,
        ps: i32,
        size: i32,
    }

    impl PathResolver for Car<'_> {
        fn path_to_index(&self, path: &str) -> Option<usize> {
            match path {
                "name" => Some(0),
                "ps" => Some(1),
                "size" => Some(2),
                _ => None,
            }
        }

        fn value(&self, idx: usize) -> RefValue {
            [(&self.name).into(), (self.ps).into(), (self.size).into()][idx]
        }
    }

    #[test_case("size = 54", true; "size eq 54")]
    #[test_case("size len 2", true; "len eq 2")]
    #[test_case("ps > 140", true; "ps gt 140")]
    #[test_case(r#"name = "BMW""#, true; "name eq BMW")]
    #[test_case(r#"name < "bmw""#, true; "name lt bmw")]
    fn path_exec(input: &str, expect: bool) {
        let mut parser = Parser::new(input);
        let p = predicate()(&mut parser).unwrap();
        let ops = Operators::default();
        let mut e = ExecForObjectPath::new(
            p.path.clone().unwrap(),
            p.value.clone(),
            ops.get(&p.op).unwrap(),
        );

        let car = Car {
            name: "BMW",
            ps: 142,
            size: 54,
        };
        assert_eq!(Ok(true), e.prepare(&car));
        assert_eq!(expect, e.exec(&car));
    }

    #[test_case(r#"name = "BMW" or ps > 100"#, true; "name eq BMW or ps gt 100")]
    #[test_case(r#"name = "Audi" or ps > 100"#, true; "name eq Audi or ps gt 100")]
    #[test_case(r#"name = "Audi" or size = 200"#, false; "name eq Audi or size eq 200")]
    fn first_try_to_exec(input: &str, expect: bool) {
        let ops = Operators::default();
        let exp = parse(input).unwrap();
        let mut or = create_or::<Car>(exp, &ops);

        let car = Car {
            name: "BMW",
            ps: 142,
            size: 54,
        };

        let _ = or.prepare(&car).unwrap();

        assert_eq!(expect, or.exec(&car));
    }
}
