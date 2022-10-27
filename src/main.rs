#![allow(dead_code)]
use std::collections::HashMap;
use std::fmt::{Debug, Display};

// struct And {
//     left: Box<dyn Fn(&dyn Filterable) -> bool>,
//     right: Box<dyn Fn(&dyn Filterable) -> bool>,
// }

// impl And {
//     fn exec(&self, arg: &dyn Filterable) -> bool {
//         (self.left)(arg) && (self.right)(arg)
//     }

//     fn exec_observer<O: Observer>(&self, arg: &dyn Filterable, o: &O) -> bool {
//         let result = (self.left)(arg) && (self.right)(arg);
//         o.link("AND", arg, result);
//         result
//     }
// }

pub type PredicateFn = Box<dyn Fn(&dyn Filterable) -> bool>;

/// Is the function for the given operator.
/// e.g: Op: "=" -> function: |a,b| a == b
type OpFn = fn(inner: &Value, arg: &dyn Filterable) -> bool;

fn ops(op: &'static str) -> OpFn {
    let mut ops: HashMap<&str, OpFn> = HashMap::new();
    ops.insert("=", |inner: &Value, arg: &dyn Filterable| arg == inner);
    ops.insert("!=", |inner: &Value, arg: &dyn Filterable| arg != inner);
    ops.insert("len", op_len);

    *ops.get(op).unwrap()
}

fn op_len(inner: &Value, arg: &dyn Filterable) -> bool {
    if let Value::Int(l) = inner {
        return arg.to_string().len() == *l as usize;
    }
    false
}

type FnFactory = fn(inner: Value, opfn: OpFn) -> PredicateFn;

const NO_FLAG: char = ' ';

fn flags(flag: Option<char>) -> FnFactory {
    if let Some(f) = flag {
        let mut flags: HashMap<char, FnFactory> = HashMap::new();
        flags.insert('i', flag_uppercase);
        flags.insert(NO_FLAG, no_flag);

        return flags.get(&f).cloned().unwrap();
    }
    no_flag
}

fn flag_uppercase(inner: Value, opfn: OpFn) -> PredicateFn {
    let inner = Value::Text((inner).to_string().to_ascii_uppercase());
    Box::new(move |f: &dyn Filterable| (opfn)(&inner, &f.to_string().to_ascii_uppercase()))
}

fn no_flag(inner: Value, opfn: OpFn) -> PredicateFn {
    Box::new(move |f: &dyn Filterable| (opfn)(&inner, f))
}

fn predicate(inner: Value, op: &'static str, flag: Option<char>) -> PredicateFn {
    let opfn = ops(op);
    let factory = flags(flag);
    (factory)(inner, opfn)
}

#[allow(clippy::type_complexity)]
fn predicate_observer<'a, O: Observer>(
    inner: Value,
    op: &'static str,
    flag: Option<char>,
    o: &'a O,
) -> Box<dyn Fn(&dyn Filterable) -> bool + 'a> {
    let opfn = ops(op);
    let factory = flags(flag);
    let f = (factory)(inner.clone(), opfn);

    Box::new(move |arg: &dyn Filterable| {
        let result = (f)(arg);
        o.predicate(op, &inner, arg, result);
        result
    })
}

fn and(left: PredicateFn, right: PredicateFn) -> PredicateFn {
    Box::new(move |arg: &dyn Filterable| left(arg) && right(arg))
}

fn and_observer<'a, O: Observer>(
    left: PredicateFn,
    right: PredicateFn,
    o: &'a O,
) -> Box<dyn Fn(&dyn Filterable) -> bool + 'a> {
    Box::new(move |arg: &dyn Filterable| {
        let result = left(arg) && right(arg);
        o.link("AND", arg, result);
        result
    })
}

fn or(left: PredicateFn, right: PredicateFn) -> PredicateFn {
    Box::new(move |arg: &dyn Filterable| left(arg) || right(arg))
}

trait Observer {
    fn predicate(&self, op: &str, inner: &Value, arg: &dyn Filterable, result: bool);
    fn link(&self, link: &str, arg: &dyn Filterable, result: bool);
}

struct DebugObserver;

// 5 [= 5] (true) 5 [= 6] (false) and (false)
// "Blub" [=i "blub"] ["BLUB" = "BLUB"] (true)
impl Observer for DebugObserver {
    fn predicate(&self, op: &str, inner: &Value, arg: &dyn Filterable, result: bool) {
        println!("{op} {inner} [{arg} -> {result}]");
    }

    fn link(&self, link: &str, _arg: &dyn Filterable, result: bool) {
        println!("{link} [{result}]");
    }
}

fn main() {
    println!("Lets go ...");

    let f0 = predicate(Value::Text(String::from("Blub")), "=", None);
    assert!(f0(&"Blub"));

    let debug = DebugObserver {};
    let f1 = predicate_observer(Value::Text(String::from("blub")), "=", Some('i'), &debug);
    assert!(f1(&"Blub"));

    let f2 = predicate_observer(Value::Int(4), "len", None, &debug);
    assert!(f2(&"Blub"));

    // println!("-------------------");
    // and(f1, f2);
}

// ---------------------------------------------------------
#[derive(Clone, Debug)]
pub enum Value {
    Int(i32),
    Text(String),
}

impl Display for Value {
    fn fmt(&self, fm: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(v) => write!(fm, "{}", v),
            Value::Text(v) => write!(fm, "{}", v),
        }
    }
}

impl PartialEq<Value> for i32 {
    fn eq(&self, other: &Value) -> bool {
        if let Value::Int(v) = other {
            return v == self;
        }
        false
    }
}

impl PartialEq<Value> for String {
    fn eq(&self, other: &Value) -> bool {
        if let Value::Text(t) = other {
            return t == self;
        }
        false
    }
}

impl PartialEq<Value> for str {
    fn eq(&self, other: &Value) -> bool {
        if let Value::Text(t) = other {
            return t == self;
        }
        false
    }
}

impl PartialEq<Value> for &str {
    fn eq(&self, other: &Value) -> bool {
        if let Value::Text(t) = other {
            return t == self;
        }
        false
    }
}

impl PartialEq<String> for Value {
    fn eq(&self, s: &String) -> bool {
        if let Value::Text(t) = self {
            return t == s;
        }
        false
    }
}

impl PartialEq<str> for Value {
    fn eq(&self, s: &str) -> bool {
        if let Value::Text(t) = self {
            return t == s;
        }
        false
    }
}

impl PartialEq<&str> for Value {
    fn eq(&self, s: &&str) -> bool {
        if let Value::Text(t) = self {
            return t == s;
        }
        false
    }
}

pub trait Filterable: PartialEq<Value> + Display {}

impl<V: PartialEq<Value> + Display> Filterable for V {}

// trait Executor<Arg>: Clone {
//     fn exec(&self, arg: &Arg) -> bool;
// }

// #[derive(Clone)]
// struct And<L, R> {
//     left: L,
//     right: R,
// }

// impl<L, R, Arg> Executor<Arg> for And<L, R>
// where
//     Arg: PartialEq,
//     L: Executor<Arg>,
//     R: Executor<Arg>,
// {
//     fn exec(&self, arg: &Arg) -> bool {
//         self.left.exec(arg) && self.right.exec(arg)
//     }
// }

// trait Query {
//     fn and<Q>(self, other: Q) -> And<Self, Q>
//     where
//         Self: Sized,
//     {
//         And {
//             left: self,
//             right: other,
//         }
//     }
// }
