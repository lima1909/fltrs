use crate::operator::{OperatorFn, Operators};
use crate::token::{Exp, Filter};
use crate::value::Value;
use crate::PathResolver;

pub struct Exec {
    ors: [PathExecuter; 2],
}

impl Exec {
    pub fn prepare<PR: PathResolver>(exp: Exp, ops: Operators, pr: &PR) -> Self {
        // let len = exp.ands.len();
        let mut it = exp.ands.into_iter();

        // if len == 1 {
        //     let ands = it.next().expect("expect at least one Ands");
        //     if ands.is_or() {
        //         // only ONE filter
        //         // Self {
        //         //     ors: vec![PathExecuter::from_filter::<PR>(ands.filter, &ops, &pr)],
        //         // }
        //         todo!()
        //     } else {
        //         // only AND filters
        //         todo!()
        //     }
        // } else {
        // at least two ANDs
        let a1 = it.next().unwrap();
        let a2 = it.next().unwrap();

        if a1.is_or() && a2.is_or() {
            return Self {
                ors: [
                    PathExecuter::from_filter::<PR>(a1.filter, &ops, pr),
                    PathExecuter::from_filter::<PR>(a2.filter, &ops, pr),
                ],
            };
        }
        todo!()
        // }
    }

    pub fn exec<PR: PathResolver>(&self, pr: &PR) -> bool {
        for pe in &self.ors {
            if pe.exec(pr) {
                return true;
            }
        }
        false
    }
}

struct PathExecuter {
    idx: usize,
    val: Value,
    f: OperatorFn,
}

impl PathExecuter {
    fn from_filter<PR: PathResolver>(f: Filter, ops: &Operators, pr: &PR) -> Self {
        if let Filter::Predicate(p) = f {
            return Self {
                idx: pr.path_to_index(p.path.as_ref().unwrap()).unwrap(),
                val: p.value,
                f: ops.get(&p.op).unwrap(),
            };
        }
        unimplemented!()
    }

    #[inline(always)]
    pub fn exec<PR: PathResolver>(&self, pr: &PR) -> bool {
        (self.f)(pr.value(self.idx), &self.val)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{operator::Operators, Filterable};

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
        let exp = crate::parse(r#"name != "Inge" or name = "Paul" "#).unwrap();

        let ex = Exec::prepare(exp, ops, &e);
        assert!(ex.exec(&e));
    }
}
