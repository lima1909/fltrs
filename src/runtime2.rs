use crate::operator::Operators;
use crate::runtime::{ExecForObjectPath, Executor};
use crate::token::{Exp, Filter};
use crate::value::RefValue;
use crate::{PathResolver, Result};

pub(crate) fn create_path_executor<'a, Arg: 'a>(
    exp: Exp,
    ops: &'a Operators<RefValue<'a>>,
) -> Runtime<'a>
where
    Arg: PathResolver,
{
    let mut it = exp.ands.into_iter();
    let ands = it.next().unwrap();
    let mut rt;
    if ands.is_or() {
        rt = Runtime::new(ands.filter, ops)
    } else {
        rt = Runtime::new(ands.filter, ops);
        for f in ands.next {
            rt.and(f);
        }
    }

    for ands in it {
        if ands.is_or() {
            rt.or(ands.filter);
        } else {
            rt.and(ands.filter);
            for f in ands.next {
                rt.and(f);
            }
        }
    }

    rt
}
pub struct Runtime<'a> {
    pos: usize,
    ors: Vec<Ands<'a>>,
    ops: &'a Operators<RefValue<'a>>,
}

impl<'a> Runtime<'a> {
    fn new(f: Filter, ops: &'a Operators<RefValue<'a>>) -> Self {
        Self {
            pos: 0,
            ors: vec![Ands::new(f, ops)],
            ops,
        }
    }

    fn and(&mut self, filter: Filter) {
        self.ors[self.pos].append(filter);
    }

    fn or(&mut self, filter: Filter) {
        self.pos += 1;
        self.ors[self.pos] = Ands::new(filter, self.ops);
    }
}

impl<'a, Arg> Executor<'a, Arg> for Runtime<'a>
where
    Arg: PathResolver,
{
    fn prepare(&mut self, arg: &'a Arg) -> Result<bool> {
        for ands in &mut self.ors {
            ands.prepare(arg)?;
        }
        Ok(true)
    }

    fn exec(&self, arg: &'a Arg) -> bool {
        for ands in &self.ors {
            // one OR is true, all OR are true
            if ands.exec(arg) {
                return true;
            }
        }

        false
    }
}

struct Ands<'a> {
    or: ExecForObjectPath<'a>,
    ands: Vec<ExecForObjectPath<'a>>,
    ops: &'a Operators<RefValue<'a>>,
}

impl<'a> Ands<'a> {
    fn new(f: Filter, ops: &'a Operators<RefValue<'a>>) -> Self {
        Self {
            or: ExecForObjectPath::from_filter(f, ops),
            ands: Vec::new(),
            ops,
        }
    }

    fn append(&mut self, f: Filter) {
        self.ands.push(ExecForObjectPath::from_filter(f, self.ops));
    }
}

impl<'a, Arg> Executor<'a, Arg> for Ands<'a>
where
    Arg: PathResolver,
{
    fn prepare(&mut self, arg: &'a Arg) -> Result<bool> {
        for p in &mut self.ands {
            p.prepare(arg)?;
        }
        Ok(true)
    }

    fn exec(&self, arg: &'a Arg) -> bool {
        let result = self.or.exec(arg);
        if !result {
            return false; // if first AND filter is false, ALL AND filter are false
        }

        for p in &self.ands {
            if !p.exec(arg) {
                return false; // if it is false, all AND filter are false
            }
        }
        result
    }
}
