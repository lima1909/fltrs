use std::ops::Deref;

use crate::operator::Operators;
use crate::token::{Exp, Filter};
use crate::value::Value;
use crate::PathResolver;

pub fn query<PR: PathResolver + 'static>(exp: Exp, _ops: &Operators, pr: &PR) -> Box<dyn Exec<PR>> {
    if exp.ands.is_empty() {
        panic!("empty expression is not allowed")
    }

    let mut it = exp.ands.into_iter();
    let mut query;

    let ands = it.next().expect("expect at least one Ands");
    if ands.is_or() {
        // only ONE filter
        query = from_filter(ands.filter, pr)
    } else {
        // only AND filters
        todo!()
    }

    for ands in it {
        if ands.is_or() {
            let ex = from_filter(ands.filter, pr);
            let f: Box<dyn Fn(&PR) -> bool> = Box::new(move |pr| ex.exec(pr) || query.exec(pr));
            query = Box::new(f) as Box<dyn Exec<PR>>;
        } else {
        }
    }

    query
}

fn from_filter<PR: PathResolver + 'static>(filter: Filter, pr: &PR) -> Box<dyn Exec<PR>> {
    match filter {
        Filter::Predicate(p) => {
            let path = p.path.clone().unwrap();
            let idx = pr.path_to_index(&path).unwrap();
            let f = new::<PR>(&p.op, idx, p.value);
            Box::new(f)
        }
        Filter::Not(exp) => Box::new(Not(query(exp, &Operators::default(), pr))),
        Filter::Nested(exp) => Box::new(Query(query(exp, &Operators::default(), pr))),
    }
}

pub trait Exec<PR: PathResolver> {
    fn exec(&self, pr: &PR) -> bool;
}

impl<PR: PathResolver> Exec<PR> for Box<dyn Exec<PR>> {
    fn exec(&self, pr: &PR) -> bool {
        self.deref().exec(pr)
    }
}

impl<PR: PathResolver> Exec<PR> for Box<dyn Fn(&PR) -> bool> {
    fn exec(&self, pr: &PR) -> bool {
        (self)(pr)
    }
}

impl<PR: PathResolver> Exec<PR> for dyn Fn(&PR) -> bool {
    fn exec(&self, pr: &PR) -> bool {
        (self)(pr)
    }
}

impl<PR: PathResolver> Exec<PR> for &dyn Fn(&PR) -> bool {
    fn exec(&self, pr: &PR) -> bool {
        (self)(pr)
    }
}

struct Not<E>(E);

impl<PR: PathResolver, E: Exec<PR>> Exec<PR> for Not<E> {
    fn exec(&self, pr: &PR) -> bool {
        !self.0.exec(pr)
    }
}

struct Query<E>(E);

impl<PR: PathResolver, E: Exec<PR>> Exec<PR> for Query<E> {
    fn exec(&self, pr: &PR) -> bool {
        self.0.exec(pr)
    }
}

fn new<PR: PathResolver + 'static>(op: &str, idx: usize, v: Value) -> Box<dyn Fn(&PR) -> bool> {
    //Box<dyn Fn(&PR) -> bool> {
    match op {
        "=" => Box::new(move |pr| *pr.value(idx) == v),
        "!=" => Box::new(move |pr| *pr.value(idx) != v),
        _ => unimplemented!(),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{operator::Operators, parse, Filterable};

    struct Example {
        name: String,
        x: i32,
    }

    impl PathResolver for Example {
        fn path_to_index(&self, path: &str) -> Option<usize> {
            match path {
                "name" => Some(0),
                _ => Some(1),
            }
        }

        fn value(&self, idx: usize) -> &dyn Filterable {
            match idx {
                1 => &self.x,
                _ => &self.name,
            }
        }
    }

    #[test]
    fn exec() {
        let e = Example {
            name: String::from("Paul"),
            x: 42,
        };

        let ops = Operators::default();
        let exp = parse(r#"x = 45 or name = "Paul" "#).unwrap();

        let ex = query(exp, &ops, &e);
        assert!(ex.exec(&e));
    }
}
