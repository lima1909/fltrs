#![allow(dead_code)] // TODO: remove this

use crate::error::FltrError;
use crate::operator::{OperatorFn, Operators};
use crate::token::{Ands, Exp, Filter};
use crate::value::{RefValue, Value};
use crate::{PathResolver, Result};

#[allow(unused_variables)]
fn create_path_executor<'a, Arg: 'a>(
    exp: Exp,
    ops: &'a Operators<RefValue<'a>>,
) -> Box<dyn Executor<'a, Arg> + 'a>
where
    Arg: PathResolver,
{
    if exp.ands.len() == 1 && exp.ands[0].is_or() {
        // only one filter
        return Box::new(ExecForObjectPath::from_filter(
            exp.ands.into_iter().next().unwrap().filter,
            ops,
        ));
    } else {
        let mut it = exp.ands.into_iter();

        let mut ors;
        let first_ands = create_path_and_executor(it.next().unwrap(), ops);
        if let Some(ands) = it.next() {
            ors = Box::new(Or(first_ands, create_path_and_executor(ands, ops)));
        } else {
            return first_ands;
        }

        for ands in it {
            ors = Box::new(Or(create_path_and_executor(ands, ops), ors));
        }
        ors
    }
}

fn create_path_and_executor<'a, Arg: 'a>(
    ands: Ands,
    ops: &'a Operators<RefValue<'a>>,
) -> Box<dyn Executor<'a, Arg> + 'a>
where
    Arg: PathResolver,
{
    let mut it = ands.next.into_iter();

    let mut and;
    let filter = Box::new(ExecForObjectPath::from_filter(ands.filter, ops));
    if let Some(a) = it.next() {
        and = And(filter, Box::new(ExecForObjectPath::from_filter(a, ops)));
    } else {
        return filter;
    }

    for next in it {
        and = And(
            Box::new(and),
            Box::new(ExecForObjectPath::from_filter(next, ops)),
        );
    }
    Box::new(and)
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

    pub(crate) fn from_filter(filter: Filter, ops: &'a Operators<Arg>) -> Self {
        match filter {
            Filter::Predicate(p) => ExecForValue::new(p.value.clone(), ops.get(&p.op).unwrap()),
            _ => todo!(),
        }
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

    pub(crate) fn from_filter(filter: Filter, ops: &'a Operators<RefValue<'a>>) -> Self {
        match filter {
            Filter::Predicate(p) => ExecForObjectPath::new(
                p.path.clone().unwrap(),
                p.value.clone(),
                ops.get(&p.op).unwrap(),
            ),
            _ => todo!(),
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

pub struct Or<'a, Arg>(
    pub Box<dyn Executor<'a, Arg> + 'a>,
    pub Box<dyn Executor<'a, Arg> + 'a>,
);

impl<'a, Arg> Executor<'a, Arg> for Or<'a, Arg> {
    fn prepare(&mut self, arg: &'a Arg) -> Result<bool> {
        self.0.prepare(arg)?;
        self.1.prepare(arg)
    }

    fn exec(&self, arg: &'a Arg) -> bool {
        self.0.exec(arg) || self.1.exec(arg)
    }
}

pub struct And<'a, Arg>(
    pub Box<dyn Executor<'a, Arg> + 'a>,
    pub Box<dyn Executor<'a, Arg> + 'a>,
);

impl<'a, Arg> Executor<'a, Arg> for And<'a, Arg> {
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

    #[test_case(r#"name = "BMW" "#, true; "name eq BMW")]
    #[test_case(r#"name starts_with "BM" "#, true; "name starts_with BM")]
    #[test_case(r#"name != "Audi" "#, true; "name ne Audi")]
    fn create_path_executor_simple(input: &str, expect: bool) {
        let ops = Operators::default();
        let exp = parse(input).unwrap();

        let car = Car {
            name: "BMW",
            ps: 142,
            size: 54,
        };

        let mut ex = create_path_executor(exp, &ops);
        let _ = ex.prepare(&car).unwrap();
        assert_eq!(expect, ex.exec(&car));
    }

    #[test_case(r#"name = "BMW" and ps > 100"#, true; "name eq BMW and ps gt 100")]
    #[test_case(r#"name = "BMW" and ps > 100 and size len 2"#, true; "name eq BMW and ps gt 100 and size len 2")]
    fn create_path_executor_ands(input: &str, expect: bool) {
        let ops = Operators::default();
        let exp = parse(input).unwrap();

        let car = Car {
            name: "BMW",
            ps: 142,
            size: 54,
        };

        let mut ex = create_path_executor(exp, &ops);
        let _ = ex.prepare(&car).unwrap();
        assert_eq!(expect, ex.exec(&car));
    }

    #[test_case(r#"name = "BMW" or ps > 100"#, true; "name eq BMW or ps gt 100")]
    #[test_case(r#"name = "Audi" or ps > 100"#, true; "name eq Audi or ps gt 100")]
    #[test_case(r#"name = "Audi" or size = 200 or ps = 142"#, true; "3 ors")]
    #[test_case(r#"name = "Audi" or size = 200"#, false; "name eq Audi or size eq 200")]
    fn create_path_executor_ors(input: &str, expect: bool) {
        let ops = Operators::default();
        let exp = parse(input).unwrap();

        let car = Car {
            name: "BMW",
            ps: 142,
            size: 54,
        };

        let mut ex = create_path_executor(exp, &ops);
        let _ = ex.prepare(&car).unwrap();
        assert_eq!(expect, ex.exec(&car));
    }

    #[test_case(r#"name = "BMW" and ps != 100 or size = 54"#, true; "and or")]
    #[test_case(r#"name = "Audi" or ps = 142 and size = 54 or size > 100"#, true; "false || true && true || false -> true")]
    #[test_case(r#"name = "BMW" or ps = 142 and size = 158 or size > 100"#, true; "true || true && false || false -> true")]
    fn create_path_executor_ors_ands(input: &str, expect: bool) {
        let ops = Operators::default();
        let exp = parse(input).unwrap();

        let car = Car {
            name: "BMW",
            ps: 142,
            size: 54,
        };

        let mut ex = create_path_executor(exp, &ops);
        let _ = ex.prepare(&car).unwrap();
        assert_eq!(expect, ex.exec(&car));
    }
}
