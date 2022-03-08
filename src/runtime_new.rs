#![allow(dead_code)] // TODO: remove this

use std::marker::PhantomData;

use crate::operator::Operators;
use crate::runtime::{Executor, FromPredicate};
use crate::token::{Ands, Exp, Filter};
use crate::Result;

type BoxExecutor<'a, Arg> = Box<dyn Executor<'a, Arg> + 'a>;

pub struct Runtime<E, Arg> {
    exec: E,
    _arg: PhantomData<Arg>,
}

impl<'a, E, Arg> Runtime<E, Arg>
where
    E: Executor<'a, Arg>,
{
    pub fn new(exec: E) -> Runtime<impl Executor<'a, Arg>, Arg> {
        Runtime {
            exec,
            _arg: PhantomData,
        }
    }
}

impl<'a, E, Arg> Executor<'a, Arg> for Runtime<E, Arg>
where
    E: Executor<'a, Arg>,
{
    fn prepare(&mut self, arg: &'a Arg) -> Result<bool> {
        self.exec.prepare(arg)
    }

    fn exec(&self, arg: &'a Arg) -> bool {
        self.exec.exec(arg)
    }
}

struct Not<'a, Arg>(BoxExecutor<'a, Arg>);

impl<'a, Arg: 'a> Executor<'a, Arg> for Not<'a, Arg> {
    fn prepare(&mut self, arg: &'a Arg) -> Result<bool> {
        self.0.prepare(arg)
    }

    fn exec(&self, arg: &'a Arg) -> bool {
        !self.0.exec(arg)
    }
}

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

pub struct And<L, R>(L, R);

impl<'a, L, R, Arg> Executor<'a, Arg> for And<L, R>
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

pub(crate) fn new<'a, P: 'a, Arg: 'a>(exp: Exp, ops: &'a Operators) -> BoxExecutor<'a, Arg>
where
    P: FromPredicate<'a, Arg>,
{
    let len = exp.ands.len();
    let mut it = exp.ands.into_iter();

    if len == 1 {
        let ands = it.next().expect("expect at least one Ands");
        if ands.is_or() {
            // only ONE filter
            match_filter::<P, Arg>(ands.filter, ops)
        } else {
            // only AND filters
            new_ands::<P, Arg>(ands, ops)
        }
    } else {
        // at least two ANDs
        let a1 = it.next().unwrap();
        let a2 = it.next().unwrap();

        let mut ors: Box<dyn Executor<Arg>>;
        if a1.is_or() && a2.is_or() {
            ors = match_or::<P, Arg>(a1.filter, a2.filter, ops);
        } else if a1.is_or() && !a2.is_or() {
            ors = match_filter_or::<P, Arg>(a1.filter, new_ands::<P, Arg>(a2, ops), ops);
        } else if !a1.is_or() && a2.is_or() {
            ors = match_filter_or::<P, Arg>(a2.filter, new_ands::<P, Arg>(a1, ops), ops);
        } else {
            ors = Box::new(Or(new_ands::<P, Arg>(a1, ops), new_ands::<P, Arg>(a2, ops)));
        }

        for ands in it {
            if ands.is_or() {
                ors = match_filter_or::<P, Arg>(ands.filter, ors, ops);
            } else {
                ors = Box::new(Or(new_ands::<P, Arg>(ands, ops), ors));
            }
        }

        ors
    }
}

fn new_ands<'a, P: 'a, Arg: 'a>(ands: Ands, ops: &'a Operators) -> BoxExecutor<'a, Arg>
where
    P: FromPredicate<'a, Arg>,
{
    let mut it = ands.next.into_iter();

    let mut and = match_and::<P, Arg>(ands.filter, it.next().unwrap(), ops);
    for next in it {
        and = match_filter_and::<P, Arg>(next, and, ops);
    }

    and
}

fn new_ands_predicate<'a, P: 'a, Arg: 'a>(
    ands: Ands,
    ops: &'a Operators,
) -> And<P, BoxExecutor<'a, Arg>>
where
    P: FromPredicate<'a, Arg>,
{
    let ands_str = ands.to_string();
    let mut it = ands.next.into_iter();

    if let Filter::Predicate(l) = ands.filter {
        if let Filter::Predicate(r) = it.next().unwrap() {
            let mut and: And<P, BoxExecutor<'a, Arg>> = And(
                P::from_predicate(l, ops),
                Box::new(P::from_predicate(r, ops)),
            );

            for next in it {
                if let Filter::Predicate(p) = next {
                    and = And(P::from_predicate(p, ops), Box::new(and));
                }
            }

            return and;
        }
    }
    // this should be unreachable code!!!
    panic!("nested ands are not expected: {}", ands_str)
}

fn match_filter<'a, P: 'a, Arg: 'a>(f: Filter, ops: &'a Operators) -> BoxExecutor<'a, Arg>
where
    P: FromPredicate<'a, Arg>,
{
    match f {
        Filter::Predicate(p) => Box::new(P::from_predicate(p, ops)),
        Filter::Nested(exp) => new::<P, Arg>(exp, ops),
        Filter::Not(exp) => Box::new(Not(new::<P, Arg>(exp, ops))),
    }
}

fn match_or<'a, P: 'a, Arg: 'a>(l: Filter, r: Filter, ops: &'a Operators) -> BoxExecutor<'a, Arg>
where
    P: FromPredicate<'a, Arg>,
{
    match (l, r) {
        (Filter::Predicate(lp), Filter::Predicate(rp)) => {
            Box::new(Or(P::from_predicate(lp, ops), P::from_predicate(rp, ops)))
        }
        (Filter::Predicate(p), Filter::Nested(exp))
        | (Filter::Nested(exp), Filter::Predicate(p)) => {
            Box::new(Or(P::from_predicate(p, ops), new::<P, Arg>(exp, ops)))
        }
        (Filter::Predicate(p), Filter::Not(exp)) | (Filter::Not(exp), Filter::Predicate(p)) => {
            Box::new(Or(P::from_predicate(p, ops), Not(new::<P, Arg>(exp, ops))))
        }
        (Filter::Nested(lexp), Filter::Nested(rexp)) => {
            Box::new(Or(new::<P, Arg>(lexp, ops), new::<P, Arg>(rexp, ops)))
        }
        (Filter::Not(lexp), Filter::Not(rexp)) => Box::new(Or(
            Not(new::<P, Arg>(lexp, ops)),
            Not(new::<P, Arg>(rexp, ops)),
        )),
        (Filter::Nested(lexp), Filter::Not(rexp)) | (Filter::Not(rexp), Filter::Nested(lexp)) => {
            Box::new(Or(new::<P, Arg>(lexp, ops), Not(new::<P, Arg>(rexp, ops))))
        }
    }
}

fn match_filter_or<'a, P: 'a, Arg: 'a>(
    f: Filter,
    and: BoxExecutor<'a, Arg>,
    ops: &'a Operators,
) -> BoxExecutor<'a, Arg>
where
    P: FromPredicate<'a, Arg>,
{
    match f {
        Filter::Predicate(p) => Box::new(Or(P::from_predicate(p, ops), and)),
        Filter::Nested(exp) => Box::new(Or(new::<P, Arg>(exp, ops), and)),
        Filter::Not(exp) => Box::new(Or(Not(new::<P, Arg>(exp, ops)), and)),
    }
}

fn match_and<'a, P: 'a, Arg: 'a>(l: Filter, r: Filter, ops: &'a Operators) -> BoxExecutor<'a, Arg>
where
    P: FromPredicate<'a, Arg>,
{
    match (l, r) {
        (Filter::Predicate(lp), Filter::Predicate(rp)) => {
            Box::new(And(P::from_predicate(lp, ops), P::from_predicate(rp, ops)))
        }
        (Filter::Predicate(p), Filter::Nested(exp))
        | (Filter::Nested(exp), Filter::Predicate(p)) => {
            Box::new(And(P::from_predicate(p, ops), new::<P, Arg>(exp, ops)))
        }
        (Filter::Predicate(p), Filter::Not(exp)) | (Filter::Not(exp), Filter::Predicate(p)) => {
            Box::new(And(P::from_predicate(p, ops), Not(new::<P, Arg>(exp, ops))))
        }
        (Filter::Nested(lexp), Filter::Nested(rexp)) => {
            Box::new(And(new::<P, Arg>(lexp, ops), new::<P, Arg>(rexp, ops)))
        }
        (Filter::Not(lexp), Filter::Not(rexp)) => Box::new(And(
            Not(new::<P, Arg>(lexp, ops)),
            Not(new::<P, Arg>(rexp, ops)),
        )),
        (Filter::Nested(lexp), Filter::Not(rexp)) | (Filter::Not(rexp), Filter::Nested(lexp)) => {
            Box::new(And(new::<P, Arg>(lexp, ops), Not(new::<P, Arg>(rexp, ops))))
        }
    }
}

fn match_filter_and<'a, P: 'a, Arg: 'a>(
    f: Filter,
    and: BoxExecutor<'a, Arg>,
    ops: &'a Operators,
) -> BoxExecutor<'a, Arg>
where
    P: FromPredicate<'a, Arg>,
{
    match f {
        Filter::Predicate(p) => Box::new(And(P::from_predicate(p, ops), and)),
        Filter::Nested(exp) => Box::new(And(new::<P, Arg>(exp, ops), and)),
        Filter::Not(exp) => Box::new(And(Not(new::<P, Arg>(exp, ops)), and)),
    }
}

#[cfg(test)]
mod test {

    // use super::*;
    use crate::parser::parse;
    // use crate::runtime::{Executor, ValueExecutor};

    #[test]
    fn walk_or() {
        let _exp = parse("= 5 or = 7").unwrap();
    }
}
