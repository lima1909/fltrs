#![allow(dead_code)] // TODO: remove this

use crate::value::Value;
use core::fmt::{Debug, Display};

// TODO: remove 'pub', temporary for benchmarking
#[derive(PartialEq, PartialOrd, Debug)]
pub struct Exp {
    is_nested: bool,
    index: usize,
    pub(crate) ands: Vec<Ands>,
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
                write!(fm, "{}", ands)?;
            } else {
                write!(fm, " or {}", ands)?;
            }
        }
        Ok(())
    }
}

impl Exp {
    pub(crate) fn new(f: Filter) -> Self {
        let mut ors = Self {
            is_nested: false,
            index: 0,
            ands: vec![],
        };
        ors.set_nested(&f);
        ors.ands.push(Ands::new(f));
        ors
    }

    pub(crate) fn or(&mut self, f: Filter) {
        self.set_nested(&f);
        self.ands.push(Ands::new(f));
        self.index += 1;
    }

    pub(crate) fn and(&mut self, f: Filter) {
        self.set_nested(&f);
        let and = self.ands.get_mut(self.index).unwrap();
        and.push(f);
    }

    fn set_nested(&mut self, f: &Filter) {
        if !self.is_nested {
            match f {
                Filter::Nested(_) | Filter::Not(_) => self.is_nested = true,
                _ => {}
            }
        }
    }

    pub(crate) fn is_nested(&self) -> bool {
        self.is_nested
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
            Filter::Predicate(p) => write!(fm, "{}", p),
            Filter::Not(exp) => write!(fm, "not({})", exp),
            Filter::Nested(exp) => write!(fm, "({})", exp),
        }
    }
}

#[derive(PartialEq, PartialOrd, Debug)]
pub(crate) struct Predicate {
    pub(crate) path: Option<String>,
    pub(crate) op: String,
    pub(crate) value: Value,
}

impl Display for Predicate {
    fn fmt(&self, fm: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(p) = &self.path {
            write!(fm, "{} ", p)?;
        }
        write!(fm, "{} {}", &self.op, &self.value)
    }
}

impl Predicate {
    pub(crate) fn has_path(&self) -> bool {
        self.path.is_some()
    }
}
