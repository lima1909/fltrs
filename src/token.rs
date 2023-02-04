use crate::Value;

use core::fmt::{Debug, Display};

#[derive(PartialEq, PartialOrd, Debug)]
pub struct Exp {
    index: usize,
    pub(crate) ands: Vec<Ands>,
    pub(crate) observer: String,
}

impl Display for Exp {
    fn fmt(&self, fm: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.ands.is_empty() {
            return write!(fm, "empty or");
        }

        let len = self.index + 1;
        for i in 0..len {
            let ands = self.ands.get(i).unwrap();
            if i == 0 {
                write!(fm, "{ands}")?;
            } else {
                write!(fm, " or {ands}")?;
            }
        }
        Ok(())
    }
}

impl Exp {
    pub(crate) fn new(f: Filter, observer: String) -> Self {
        let mut ors = Self {
            index: 0,
            ands: vec![],
            observer,
        };
        ors.ands.push(Ands::new(f));
        ors
    }

    pub(crate) fn or(&mut self, f: Filter) {
        self.ands.push(Ands::new(f));
        self.index += 1;
    }

    pub(crate) fn and(&mut self, f: Filter) {
        let and = self.ands.get_mut(self.index).unwrap();
        and.push(f);
    }
}

#[derive(PartialEq, PartialOrd, Debug)]
pub(crate) struct Ands {
    pub(crate) filter: Filter,
    pub(crate) next: Vec<Filter>,
}

impl Display for Ands {
    fn fmt(&self, fm: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(fm, "{}", self.filter)?;
        for pos in 0..self.next.len() {
            write!(fm, " and {}", self.next.get(pos).unwrap())?;
        }
        Ok(())
    }
}

impl Ands {
    pub(crate) fn new(f: Filter) -> Self {
        Self {
            filter: f,
            next: vec![],
        }
    }

    pub(crate) fn push(&mut self, f: Filter) {
        self.next.push(f);
    }

    pub(crate) fn is_or(&self) -> bool {
        self.next.is_empty()
    }
}

#[derive(PartialEq, PartialOrd, Debug)]
pub(crate) enum Filter {
    Predicate(Predicate),
    Not(Exp),
    Nested(Exp),
}

impl Display for Filter {
    fn fmt(&self, fm: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Filter::Predicate(p) => write!(fm, "{p}"),
            Filter::Not(exp) => write!(fm, "not ({exp})"),
            Filter::Nested(exp) => write!(fm, "({exp})"),
        }
    }
}

#[derive(PartialEq, PartialOrd, Debug)]
pub(crate) struct Predicate {
    pub(crate) path: Option<String>,
    pub(crate) op: Op,
    pub(crate) value: Value,
}

impl Display for Predicate {
    fn fmt(&self, fm: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(p) = &self.path {
            write!(fm, "{p} ")?;
        }

        write!(fm, "{} {}", &self.op, &self.value)
    }
}

#[derive(Eq, PartialEq, PartialOrd, Debug)]
pub struct Op {
    pub name: String,
    pub flag: Option<char>,
}

impl Op {
    pub(crate) fn new(name: &str, flag: Option<char>) -> Self {
        Self {
            name: name.to_owned(),
            flag,
        }
    }

    pub(crate) fn from_str(name: &str) -> Self {
        Self {
            name: name.to_owned(),
            flag: None,
        }
    }
}

impl Display for Op {
    fn fmt(&self, fm: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(flag) = &self.flag {
            write!(fm, "{}:{flag} ", self.name)?;
        }

        write!(fm, "{}", self.name)
    }
}
