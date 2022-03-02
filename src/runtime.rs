#![allow(dead_code)] // TODO: remove this

use crate::error::FltrError;
use crate::operator::{OperatorFn, Operators};
use crate::token::{Ands, Exp, Filter, Predicate};
use crate::value::Value;
use crate::{Filterable, PathResolver, Result};

pub(crate) fn create_path_executor<'a, Arg: 'a>(
    exp: Exp,
    ops: &'a Operators,
) -> Box<dyn Executor<'a, Arg> + 'a>
where
    Arg: PathResolver,
{
    if exp.ands.len() == 1 && exp.ands[0].is_or() {
        // only one filter
        Box::new(PathExecutor::from_filter(
            exp.ands.into_iter().next().unwrap().filter,
            ops,
        ))
    } else if exp.ands.len() == 1 {
        // only and filters
        Box::new(create_path_and_executor(
            exp.ands.into_iter().next().unwrap(),
            ops,
        ))
    } else {
        let mut it = exp.ands.into_iter();

        let a1 = it.next().unwrap();
        let a2 = it.next().unwrap();

        let mut ors: Box<dyn Executor<Arg>>;
        if a1.is_or() && a2.is_or() {
            ors = Box::new(Or::<PathExecutor, PathExecutor>(
                PathExecutor::from_filter(a1.filter, ops),
                PathExecutor::from_filter(a2.filter, ops),
            ));
        } else if a1.is_or() && !a2.is_or() {
            ors = Box::new(Or::<PathExecutor, And<'a, Arg>>(
                PathExecutor::from_filter(a1.filter, ops),
                create_path_and_executor(a2, ops),
            ));
        } else if !a1.is_or() && a2.is_or() {
            ors = Box::new(Or::<PathExecutor, And<'a, Arg>>(
                PathExecutor::from_filter(a2.filter, ops),
                create_path_and_executor(a1, ops),
            ));
        } else {
            ors = Box::new(Or::<And<'a, Arg>, And<'a, Arg>>(
                create_path_and_executor(a1, ops),
                create_path_and_executor(a2, ops),
            ));
        }

        for ands in it {
            if ands.is_or() {
                ors = Box::new(Or::<PathExecutor, Box<dyn Executor<Arg>>>(
                    PathExecutor::from_filter(ands.filter, ops),
                    ors,
                ));
            } else {
                ors = Box::new(Or::<And<'a, Arg>, Box<dyn Executor<Arg>>>(
                    create_path_and_executor(ands, ops),
                    ors,
                ));
            }
        }
        ors
    }
}

fn create_path_and_executor<'a, Arg: 'a>(ands: Ands, ops: &'a Operators) -> And<'a, Arg>
where
    Arg: PathResolver,
{
    let mut it = ands.next.into_iter();

    let mut and = And(
        PathExecutor::from_filter(ands.filter, ops),
        Box::new(PathExecutor::from_filter(it.next().unwrap(), ops)),
    );

    for next in it {
        and = And(PathExecutor::from_filter(next, ops), Box::new(and));
    }
    and
}

pub(crate) struct Runtime<'a, Arg> {
    executor: Box<dyn Executor<'a, Arg> + 'a>,
}

impl<'a, Arg: 'a> Runtime<'a, Arg> {
    pub(crate) fn new<P: FromPredicate<'a, Arg>>(exp: Exp, ops: &'a Operators) -> Self {
        let len = exp.ands.len();
        let mut it = exp.ands.into_iter();

        if len == 1 {
            let ands = it.next().expect("expect at least one Ands");
            if ands.is_or() {
                // only ONE filter
                Self {
                    executor: Runtime::match_filter::<P>(ands.filter, ops),
                }
            } else {
                // only AND filters
                Self {
                    executor: Runtime::ands::<P>(ands, ops),
                }
            }
        } else {
            // at least two ANDs
            let a1 = it.next().unwrap();
            let a2 = it.next().unwrap();

            let mut ors: Box<dyn Executor<Arg>>;
            if a1.is_or() && a2.is_or() {
                ors = Box::new(Or2(
                    Runtime::match_filter::<P>(a1.filter, ops),
                    Runtime::match_filter::<P>(a2.filter, ops),
                ));
            } else if a1.is_or() && !a2.is_or() {
                ors = Box::new(Or2(
                    Runtime::match_filter::<P>(a1.filter, ops),
                    Runtime::ands::<P>(a2, ops),
                ));
            } else if !a1.is_or() && a2.is_or() {
                ors = Box::new(Or2(
                    Runtime::match_filter::<P>(a2.filter, ops),
                    Runtime::ands::<P>(a1, ops),
                ));
            } else {
                ors = Box::new(Or2(
                    Runtime::ands::<P>(a1, ops),
                    Runtime::ands::<P>(a2, ops),
                ));
            }

            for ands in it {
                if ands.is_or() {
                    ors = Box::new(Or2(Runtime::match_filter::<P>(ands.filter, ops), ors));
                } else {
                    ors = Box::new(Or2(Runtime::ands::<P>(ands, ops), ors));
                }
            }
            Self { executor: ors }
        }
    }

    fn ands<P: FromPredicate<'a, Arg>>(
        ands: Ands,
        ops: &'a Operators,
    ) -> Box<dyn Executor<'a, Arg> + 'a> {
        let mut it = ands.next.into_iter();

        let mut and = And2(
            Runtime::match_filter::<P>(ands.filter, ops),
            Runtime::match_filter::<P>(it.next().unwrap(), ops),
        );

        for next in it {
            and = And2(Runtime::match_filter::<P>(next, ops), Box::new(and));
        }

        Box::new(and)
    }

    fn match_filter<P: FromPredicate<'a, Arg>>(
        filter: Filter,
        ops: &'a Operators,
    ) -> Box<dyn Executor<'a, Arg> + 'a> {
        match filter {
            Filter::Predicate(p) => Box::new(P::from_predicate(p, ops)),
            Filter::Nested(exp) => Box::new(Runtime::<Arg>::new::<P>(exp, ops)),
            Filter::Not(exp) => Box::new(Not(Runtime::<Arg>::new::<P>(exp, ops))),
        }
    }
}

impl<'a, Arg: 'a> Executor<'a, Arg> for Runtime<'a, Arg> {
    fn prepare(&mut self, arg: &'a Arg) -> Result<bool> {
        self.executor.prepare(arg)
    }

    fn exec(&self, arg: &'a Arg) -> bool {
        self.executor.exec(arg)
    }
}

struct Not<'a, Arg>(Runtime<'a, Arg>);

impl<'a, Arg: 'a> Executor<'a, Arg> for Not<'a, Arg> {
    fn prepare(&mut self, arg: &'a Arg) -> Result<bool> {
        self.0.prepare(arg)
    }

    fn exec(&self, arg: &'a Arg) -> bool {
        !self.0.exec(arg)
    }
}

// TODO. remove
pub struct Or<L, R>(L, R);

impl<'a, L, R, Arg> Executor<'a, Arg> for Or<L, R>
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

pub struct Or2<'a, Arg>(
    pub Box<dyn Executor<'a, Arg> + 'a>,
    pub Box<dyn Executor<'a, Arg> + 'a>,
);

impl<'a, Arg> Executor<'a, Arg> for Or2<'a, Arg> {
    fn prepare(&mut self, arg: &'a Arg) -> Result<bool> {
        self.0.prepare(arg)?;
        self.1.prepare(arg)
    }

    fn exec(&self, arg: &'a Arg) -> bool {
        self.0.exec(arg) || self.1.exec(arg)
    }
}

pub struct And2<'a, Arg>(
    pub Box<dyn Executor<'a, Arg> + 'a>,
    pub Box<dyn Executor<'a, Arg> + 'a>,
);

impl<'a, Arg> Executor<'a, Arg> for And2<'a, Arg> {
    fn prepare(&mut self, arg: &'a Arg) -> Result<bool> {
        self.0.prepare(arg)?;
        self.1.prepare(arg)
    }

    fn exec(&self, arg: &'a Arg) -> bool {
        self.0.exec(arg) && self.1.exec(arg)
    }
}

// TODO. remove
pub struct And<'a, Arg>(pub PathExecutor, pub Box<dyn Executor<'a, Arg> + 'a>);

impl<'a, Arg> Executor<'a, Arg> for And<'a, Arg>
where
    Arg: PathResolver,
{
    fn prepare(&mut self, arg: &'a Arg) -> Result<bool> {
        self.0.prepare(arg)?;
        self.1.prepare(arg)
    }

    fn exec(&self, arg: &'a Arg) -> bool {
        self.0.exec(arg) && self.1.exec(arg)
    }
}

pub trait Executor<'a, Arg> {
    fn prepare(&mut self, _arg: &'a Arg) -> Result<bool> {
        Ok(true)
    }

    fn exec(&self, arg: &'a Arg) -> bool;
}

impl<'a, Arg> Executor<'a, Arg> for Box<dyn Executor<'a, Arg> + 'a> {
    fn prepare(&mut self, arg: &'a Arg) -> Result<bool> {
        self.as_mut().prepare(arg)
    }

    fn exec(&self, arg: &'a Arg) -> bool {
        self.as_ref().exec(arg)
    }
}

pub(crate) trait FromPredicate<'a, Arg> {
    fn from_predicate(p: Predicate, ops: &'a Operators) -> Box<dyn Executor<Arg> + 'a>;
}

pub(crate) struct ValueExecutor {
    value: Value,
    f: OperatorFn,
}

impl ValueExecutor {
    #[inline]
    pub(crate) fn new(value: Value, f: OperatorFn) -> Self {
        Self { value, f }
    }

    // TODO: remove this function
    pub(crate) fn from_filter(filter: Filter, ops: Operators) -> Self {
        match filter {
            Filter::Predicate(p) => ValueExecutor::new(p.value.clone(), ops.get(&p.op).unwrap()),
            _ => todo!(),
        }
    }
}

impl<'a, Arg> FromPredicate<'a, Arg> for ValueExecutor
where
    Arg: Filterable,
{
    fn from_predicate(p: Predicate, ops: &'a Operators) -> Box<dyn Executor<Arg> + 'a> {
        Box::new(Self::new(
            p.value,
            ops.get(&p.op)
                .expect("Predicate must be contains a valid operation"),
        ))
    }
}

impl<'a, Arg> Executor<'a, Arg> for ValueExecutor
where
    Arg: Filterable,
{
    fn exec(&self, arg: &'a Arg) -> bool {
        (self.f)(arg, &self.value)
    }
}

pub struct PathExecutor {
    path: String,
    index: usize,
    value: Value,
    f: OperatorFn,
}

impl PathExecutor {
    pub(crate) fn new(path: String, value: Value, f: OperatorFn) -> Self {
        Self {
            path,
            index: 0,
            value,
            f,
        }
    }

    // TODO: remove this function
    pub(crate) fn from_filter(filter: Filter, ops: &Operators) -> Self {
        match filter {
            Filter::Predicate(p) => PathExecutor::new(
                p.path.clone().unwrap(),
                p.value.clone(),
                ops.get(&p.op).unwrap(),
            ),
            _ => todo!(),
        }
    }
}

impl<'a, Arg> FromPredicate<'a, Arg> for PathExecutor
where
    Arg: PathResolver + 'a,
{
    fn from_predicate(p: Predicate, ops: &'a Operators) -> Box<dyn Executor<Arg> + 'a> {
        Box::new(Self::new(
            p.path.expect("Predicate must be contains a path"),
            p.value,
            ops.get(&p.op)
                .expect("Predicate must be contains a valid operation"),
        ))
    }
}

impl<'a, 'pr: 'a, PR: 'pr> Executor<'a, PR> for PathExecutor
where
    PR: PathResolver,
{
    fn prepare(&mut self, pr: &'a PR) -> Result<bool> {
        if let Some(idx) = pr.path_to_index(&self.path) {
            self.index = idx;
            return Ok(true);
        }

        if self.path.is_empty() {
            Err(FltrError(format!(
                "by value: '{}', expected not empty path",
                self.value
            )))
        } else {
            Err(FltrError(format!("invalid path: '{}'", self.path)))
        }
    }

    fn exec(&self, pr: &'a PR) -> bool {
        let arg = pr.value(self.index);
        (self.f)(arg, &self.value)
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
    fn value_executor_i32(input: &str, expect: bool) {
        let mut parser = Parser::new(input);
        let p = predicate()(&mut parser).unwrap();
        let ops = Operators::default();
        let e = ValueExecutor::new(p.value.clone(), ops.get(&p.op).unwrap());
        assert_eq!(expect, e.exec(&7));
    }

    #[test_case(r#"= "Jasmin""#, true; "eq Jasmin")]
    #[test_case(r#"< "jasmin""#, true; "lt jasmin")]
    #[test_case(r#"> "Ina""#, true; "lt Ina")]
    #[test_case(r#"len 6"#, true; "len 6")]
    #[test_case(r#"starts_with "J""#, true; "starts_with J")]
    fn value_executor_string(input: &str, expect: bool) {
        // assert!('c' > 'C');
        let mut parser = Parser::new(input);
        let p = predicate()(&mut parser).unwrap();
        let ops = Operators::default();
        let e = ValueExecutor::new(p.value.clone(), ops.get(&p.op).unwrap());
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

        fn value(&self, idx: usize) -> &dyn Filterable {
            match idx {
                1 => &self.ps,
                2 => &self.size,
                _ => &self.name,
            }
        }
    }

    #[test_case("size = 54", true; "size eq 54")]
    #[test_case("size len 2", true; "len eq 2")]
    #[test_case("ps > 140", true; "ps gt 140")]
    #[test_case(r#"name = "BMW""#, true; "name eq BMW")]
    #[test_case(r#"name < "bmw""#, true; "ne bmw")]
    fn path_exec(input: &str, expect: bool) {
        let ops = Operators::default();
        let mut parser = Parser::new(input);
        let p = predicate()(&mut parser).unwrap();
        let mut e = PathExecutor::new(
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
    fn rt_path_executor_simple(input: &str, expect: bool) {
        let ops = Operators::default();
        let exp = parse(input).unwrap();

        let car = Car {
            name: "BMW",
            ps: 142,
            size: 54,
        };

        let mut rt = Runtime::new::<PathExecutor>(exp, &ops);
        let _ = rt.prepare(&car).unwrap();
        assert_eq!(expect, rt.exec(&car));
    }

    #[test_case(r#"name = "BMW" and ps > 100"#, true; "name eq BMW and ps gt 100")]
    #[test_case(r#"name = "BMW" and ps > 100 and size len 2"#, true; "name eq BMW and ps gt 100 and size len 2")]
    fn rt_path_executor_ands(input: &str, expect: bool) {
        let ops = Operators::default();
        let exp = parse(input).unwrap();

        let car = Car {
            name: "BMW",
            ps: 142,
            size: 54,
        };

        let mut rt = Runtime::new::<PathExecutor>(exp, &ops);
        let _ = rt.prepare(&car).unwrap();
        assert_eq!(expect, rt.exec(&car));
    }

    #[test_case(r#"name = "BMW" or ps > 100"#, true; "name eq BMW or ps gt 100")]
    #[test_case(r#"name = "Audi" or ps > 100"#, true; "name eq Audi or ps gt 100")]
    #[test_case(r#"name = "Audi" or size = 200 or ps = 142"#, true; "3 ors")]
    #[test_case(r#"name = "Audi" or size = 200"#, false; "name eq Audi or size eq 200")]
    fn rt_path_executor_ors(input: &str, expect: bool) {
        let ops = Operators::default();
        let exp = parse(input).unwrap();

        let car = Car {
            name: "BMW",
            ps: 142,
            size: 54,
        };

        let mut rt = Runtime::new::<PathExecutor>(exp, &ops);
        let _ = rt.prepare(&car).unwrap();
        assert_eq!(expect, rt.exec(&car));
    }

    #[test_case(r#"name = "BMW" and ps != 100 or size = 54"#, true; "and or")]
    #[test_case(r#"name = "Audi" or ps = 142 and size = 54 or size > 100"#, true; "false || true && true || false -> true")]
    #[test_case(r#"name = "BMW" or ps = 142 and size = 158 or size > 100"#, true; "true || true && false || false -> true")]
    fn rt_path_executor_ors_ands(input: &str, expect: bool) {
        let ops = Operators::default();
        let exp = parse(input).unwrap();

        let car = Car {
            name: "BMW",
            ps: 142,
            size: 54,
        };

        let mut rt = Runtime::new::<PathExecutor>(exp, &ops);
        let _ = rt.prepare(&car).unwrap();
        assert_eq!(expect, rt.exec(&car));
    }

    #[test_case(r#"= "bmw""#, FltrError("by value: 'bmw', expected not empty path".into()); "empty path")]
    #[test_case(r#"foo = "bmw""#, FltrError("invalid path: 'foo'".into()); "invalid path")]
    fn no_path_exec(input: &str, err: FltrError) {
        let ops = Operators::default();
        let mut parser = Parser::new(input);
        let p = predicate()(&mut parser).unwrap();
        let mut e = PathExecutor::new(
            p.path.unwrap_or_default(),
            p.value.clone(),
            ops.get(&p.op).unwrap(),
        );

        let car = Car {
            name: "BMW",
            ps: 142,
            size: 54,
        };
        assert_eq!(err, e.prepare(&car).err().unwrap());
    }

    #[test_case(r#"= "Jasmin""#, true; "eq Jasmin")]
    #[test_case(r#"< "jasmin""#, true; "lt jasmin")]
    #[test_case(r#"> "Ina""#, true; "lt Ina")]
    #[test_case(r#"len 6"#, true; "len 6")]
    #[test_case(r#"not(len 9)"#, true; "not len 9")]
    #[test_case(r#"starts_with "J""#, true; "starts_with J")]
    #[test_case(r#"(!= "Inge")"#, true; "nested ne Inge")]
    #[test_case(r#"!= "Inge" and != "Paul""#, true; "ne Inge and ne Paul")]
    #[test_case(r#"= "Paul" or = "Jasmin""#, true; "eq Paul or eq Jasmin")]
    #[test_case(r#"!= "Inge" and != "Paul" and != "Peter""#, true; "ne Inge and ne Paul and ne Peter")]
    #[test_case(r#"!= "Inge" and (!= "Paul" and != "Peter")"#, true; "nested ne Inge and ne Paul and ne Peter")]
    fn rt_executor_value_string(input: &str, expect: bool) {
        let ops = Operators::default();
        let exp = parse(input).unwrap();
        let rt = Runtime::new::<ValueExecutor>(exp, &ops);
        assert_eq!(expect, rt.exec(&"Jasmin"));
    }

    #[test_case(r#"name = "BMW" "#, true; "name eq BMW")]
    #[test_case(r#"name != "Audi" "#, true; "name ne Audi")]
    #[test_case(r#"not(name = "Audi") "#, true; "not name eq Audi")]
    #[test_case(r#"not(name = "BMW") "#, false; "not name eq BMW")]
    #[test_case(r#"ps >= 142 "#, true; "ps ge 142")]
    #[test_case(r#"ps > 141 "#, true; "ps gt 141")]
    #[test_case(r#"not(size > 141)"#, true; "not size gt 141")]
    #[test_case(r#"(size <= 141)"#, true; "nested size le 141")]
    #[test_case(r#"size <= 54 and ps >= 142"#, true; "size le 54 and ps ge 142")]
    #[test_case(r#"size < 54 or ps >= 142"#, true; "size lt 54 or ps ge 142")]
    #[test_case(r#"size = 54 and ps = 142 and name = "BMW" "#, true; "size le 54 and ps ge 142 and name eq BMW")]
    #[test_case(r#"size = 54 and (ps = 142 and name = "BMW") "#, true; "nested size eq 54 and ps ge 142 and name eq BMW")]
    #[test_case(r#"size = 54 and (ps = 500 or name = "BMW") "#, true; "nested size eq 54 and ps eq 500 or name eq BMW")]
    fn rt_executor_path_string(input: &str, expect: bool) {
        let car = Car {
            name: "BMW",
            ps: 142,
            size: 54,
        };

        let ops = Operators::default();
        let exp = parse(input).unwrap();
        let mut rt = Runtime::new::<PathExecutor>(exp, &ops);
        let _ = rt.prepare(&car);
        assert_eq!(expect, rt.exec(&car));
    }
}
