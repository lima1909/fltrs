#![allow(dead_code)] // TODO: remove this

use crate::error::FltrError;
use crate::operator::{OperatorFn, Operators};
use crate::value::{Predicate, RefValue, Value};
use crate::{PathResolver, Result};

pub trait Executor<'a, Arg> {
    fn prepare(&mut self, _arg: &'a Arg) -> Result<bool> {
        Ok(true)
    }

    fn exec(&self, arg: &'a Arg) -> bool;
}

struct SimpleExec<'a, Arg> {
    value: &'a Value,
    f: &'a OperatorFn<Arg>,
}

impl<'a, Arg> SimpleExec<'a, Arg> {
    pub(crate) fn new(p: &'a Predicate, ops: &'a Operators<Arg>) -> Self {
        Self {
            value: &p.value,
            f: ops.get(&p.op).unwrap(),
        }
    }
}

impl<'a, Arg> Executor<'a, Arg> for SimpleExec<'a, Arg> {
    fn exec(&self, arg: &'a Arg) -> bool {
        (self.f)(arg, self.value)
    }
}

struct PathExec<'a> {
    path: &'a str,
    index: usize,
    value: &'a Value,
    f: &'a OperatorFn<RefValue<'a>>,
}

impl<'a> PathExec<'a> {
    pub(crate) fn new(p: &'a Predicate, ops: &'a Operators<RefValue<'a>>) -> Self {
        Self {
            path: p.path.as_ref().unwrap(),
            index: 0,
            value: &p.value,
            f: ops.get(&p.op).unwrap(),
        }
    }
}

impl<'a, 'pr: 'a, PR: 'pr> Executor<'a, PR> for PathExec<'a>
where
    PR: PathResolver,
{
    fn prepare(&mut self, pr: &'a PR) -> Result<bool> {
        if let Some(idx) = pr.path_to_index(self.path) {
            self.index = idx;
            return Ok(true);
        }
        Err(FltrError(format!("invalid path: {}", self.path)))
    }

    fn exec(&self, pr: &'a PR) -> bool {
        let arg = pr.value(self.index);
        (self.f)(&arg, self.value)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::operator::Operators;
    use crate::parser::{predicate, Parser};
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
        let e = SimpleExec::new(&p, &ops);
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
        let e = SimpleExec::new(&p, &ops);
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
        let mut e = PathExec::new(&p, &ops);

        let car = Car {
            name: "BMW",
            ps: 142,
            size: 54,
        };
        assert_eq!(Ok(true), e.prepare(&car));
        assert_eq!(expect, e.exec(&car));
    }
}
