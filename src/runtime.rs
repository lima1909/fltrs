#![allow(dead_code)] // TODO: remove this

use crate::operator::{OperatorFn, Operators};
use crate::value::{Predicate, Value};
use crate::Result;

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

// struct PathExec<'a, Arg> {
//     path: &'a str,
//     index: usize,
//     value: &'a Value,
//     f: OperatorFn<Arg>,
// }

// impl<'a, Arg, PR> Executor<PR> for PathExec<'a, Arg>
// where
//     PR: PathResolver<Arg>,
// {
//     fn prepare(&mut self, pr: &PR) -> Result<bool> {
//         if let Some(idx) = pr.path_to_index(self.path) {
//             self.index = idx;
//             return Ok(true);
//         }
//         Err(FltrError(format!("invalid path: {}", self.path)))
//     }

//     fn exec(&self, pr: &PR) -> bool {
//         (self.f)(self.value, pr.value(self.index))
//     }
// }

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
        let mut parser = Parser::new(input);
        let p = predicate()(&mut parser).unwrap();
        let ops = Operators::default();
        let e = SimpleExec::new(&p, &ops);
        assert_eq!(expect, e.exec(&"Jasmin"));
    }
}
