#![allow(dead_code)] // TODO: remove this

use crate::error::FltrError;
use crate::operator::{OperatorFn, Operators};
use crate::value::{Predicate, Value};
use crate::{PathResolver, Result};

pub trait Executor<Arg> {
    fn prepare(&mut self, _arg: &Arg) -> Result<bool> {
        Ok(true)
    }

    fn exec(&self, arg: &Arg) -> bool;
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

impl<'a, Arg> Executor<Arg> for SimpleExec<'a, Arg> {
    fn exec(&self, arg: &Arg) -> bool {
        (self.f)(arg, self.value)
    }
}

struct PathExec<'a, Arg> {
    path: &'a str,
    index: usize,
    value: &'a Value,
    f: &'a OperatorFn<Arg>,
}

impl<'a, Arg> PathExec<'a, Arg> {
    pub(crate) fn new(p: &'a Predicate, ops: &'a Operators<Arg>) -> Self {
        Self {
            path: p.path.as_ref().unwrap(),
            index: 0,
            value: &p.value,
            f: ops.get(&p.op).unwrap(),
        }
    }
}

impl<'a, Arg, PR> Executor<PR> for PathExec<'a, Arg>
where
    PR: PathResolver<Arg>,
{
    fn prepare(&mut self, pr: &PR) -> Result<bool> {
        if let Some(idx) = pr.path_to_index(self.path) {
            self.index = idx;
            return Ok(true);
        }
        Err(FltrError(format!("invalid path: {}", self.path)))
    }

    fn exec(&self, pr: &PR) -> bool {
        let arg = pr.value(self.index);
        (self.f)(arg, self.value)
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

    struct Car {
        name: String,
        ps: i32,
        size: i32,
    }

    impl PathResolver<i32> for Car {
        fn path_to_index(&self, path: &str) -> Option<usize> {
            match path {
                "ps" => Some(0),
                "size" => Some(1),
                _ => None,
            }
        }

        fn value(&self, idx: usize) -> &i32 {
            [&self.ps, &self.size][idx]
        }
    }

    impl PathResolver<String> for Car {
        fn path_to_index(&self, path: &str) -> Option<usize> {
            if path == "name" {
                return Some(0);
            }
            None
        }

        fn value(&self, idx: usize) -> &String {
            if idx == 0 {
                return &self.name;
            }
            &self.name
        }
    }

    #[test_case("size = 54", true; "size eq 54")]
    #[test_case("size len 2", true; "len eq 2")]
    #[test_case("ps > 140", true; "ps gt 140")]
    // #[test_case(r#"name = "BMW""#, true; "name eq BMW")]
    fn path_exec(input: &str, expect: bool) {
        let mut parser = Parser::new(input);
        let p = predicate()(&mut parser).unwrap();
        let ops = Operators::default();
        let mut e = PathExec::<i32>::new(&p, &ops);

        let car = Car {
            name: String::from("BMW"),
            ps: 142,
            size: 54,
        };
        assert_eq!(Ok(true), e.prepare(&car));
        assert_eq!(expect, e.exec(&car));
    }
}
