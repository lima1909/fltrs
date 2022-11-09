#![allow(dead_code)]
use std::collections::HashMap;
use std::fmt::{Debug, Display};

pub type PredicateFn = Box<dyn Fn(&dyn Filterable) -> bool>;

/// Is the function for the given operator.
/// e.g: Op: "=" -> function: |a,b| a == b
type OpFn = fn(inner: &Value, arg: &dyn Filterable) -> bool;

// ------------
fn eq(v: Value) -> PredicateFn {
    Box::new(move |arg: &dyn Filterable| arg == &v)
}

fn neq(v: Value) -> PredicateFn {
    Box::new(move |arg: &dyn Filterable| arg != &v)
}

fn len(v: Value) -> PredicateFn {
    let Value::Int(l) = v else {
        return Box::new(move |_: &dyn Filterable| false);
    };
    Box::new(move |arg: &dyn Filterable| arg.to_string().len() == l as usize)
}

#[cfg(feature = "regex")]
fn regex(v: Value) -> PredicateFn {
    let regex = regex::Regex::new(&v.to_string()).unwrap();
    Box::new(move |arg: &dyn Filterable| regex.is_match(&arg.to_string()))
}

type Value2ValueFN = fn(Value) -> Value;
type PredicateFn2PredicateFn = fn(PredicateFn) -> PredicateFn;

fn flags_x(flag: Option<char>) -> (Value2ValueFN, PredicateFn2PredicateFn) {
    let mut flags: HashMap<char, (Value2ValueFN, PredicateFn2PredicateFn)> = HashMap::new();

    if let Some(f) = flag {
        flags.insert(
            'i',
            (
                |v| Value::Text((v).to_string().to_ascii_uppercase()),
                |f| Box::new(move |arg: &dyn Filterable| f(&arg.to_string().to_ascii_uppercase())),
            ),
        );
        flags.insert(NO_FLAG, (|v| v, |f| f));

        return flags.get(&f).cloned().unwrap();
    }
    (|v| v, |f| f)
}

fn execs<'a>(
    op: &'a str,
    v: Value,
    flag: Option<char>,
    o: Option<&'a dyn Observer>,
) -> PredicateFnLt<'a> {
    let (value_fn, predicate_fn) = flags_x(flag);
    let value = value_fn(v);

    let mut execs: HashMap<&str, fn(Value) -> PredicateFn> = HashMap::new();
    execs.insert("=", eq);
    execs.insert("!=", neq);
    execs.insert("len", len);
    #[cfg(feature = "regex")]
    execs.insert("regex", regex);

    let op_fn = execs.get(op).unwrap();
    let predicate = op_fn(value.clone());
    let predicate = predicate_fn(predicate);

    if let Some(obs) = o {
        return Box::new(move |arg: &dyn Filterable| {
            let result = (predicate)(arg);
            obs.predicate(op, &value, arg, result);
            result
        });
    }
    predicate
}

// ------------

fn ops(op: &'static str) -> OpFn {
    let mut ops: HashMap<&str, OpFn> = HashMap::new();
    ops.insert("=", |inner: &Value, arg: &dyn Filterable| arg == inner);
    ops.insert("!=", |inner: &Value, arg: &dyn Filterable| arg != inner);
    ops.insert("len", op_len);
    #[cfg(feature = "regex")]
    ops.insert("regex", regex_old);

    *ops.get(op).unwrap()
}

fn op_len(inner: &Value, arg: &dyn Filterable) -> bool {
    if let Value::Int(l) = inner {
        return arg.to_string().len() == *l as usize;
    }
    false
}

#[cfg(feature = "regex")]
fn regex_old(inner: &Value, arg: &dyn Filterable) -> bool {
    let rg = regex::Regex::new(&inner.to_string()).unwrap();
    rg.is_match(&arg.to_string())
}

pub type PredicateFnLt<'a> = Box<dyn Fn(&dyn Filterable) -> bool + 'a>;

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

fn predicate<'a>(
    inner: Value,
    op: &'static str,
    flag: Option<char>,
    o: Option<&'a dyn Observer>,
) -> PredicateFnLt<'a> {
    let opfn = ops(op);
    let factory = flags(flag);

    if let Some(o) = o {
        let f = (factory)(inner.clone(), opfn);

        return Box::new(move |arg: &dyn Filterable| {
            let result = (f)(arg);
            o.predicate(op, &inner, arg, result);
            result
        });
    }

    (factory)(inner, opfn)
}

fn and<'a>(
    left: &'a PredicateFnLt<'a>,
    right: &'a PredicateFnLt<'a>,
    o: Option<&'a dyn Observer>,
) -> PredicateFnLt<'a> {
    if let Some(o) = o {
        return Box::new(move |arg: &dyn Filterable| {
            let result = left(arg) && right(arg);
            o.link("AND", arg, result);
            result
        });
    }
    Box::new(move |arg: &dyn Filterable| left(arg) && right(arg))
}

fn or<'a>(
    left: &'a PredicateFnLt<'a>,
    right: &'a PredicateFnLt<'a>,
    o: Option<&'a dyn Observer>,
) -> PredicateFnLt<'a> {
    if let Some(o) = o {
        return Box::new(move |arg: &dyn Filterable| {
            let result = left(arg) || right(arg);
            o.link("OR", arg, result);
            result
        });
    }
    Box::new(move |arg: &dyn Filterable| left(arg) || right(arg))
}

fn observers(observer: &str) -> fn() -> Box<dyn Observer> {
    let mut observers: HashMap<&str, fn() -> Box<dyn Observer>> = HashMap::new();
    observers.insert("debug", || Box::new(DebugObserver {}));

    observers.get(&observer).cloned().unwrap()
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
        print!("{arg}: {op} {inner} [{result}] ");
    }

    fn link(&self, link: &str, _arg: &dyn Filterable, result: bool) {
        print!("{link} [{result}] ");
    }
}

fn main() {
    println!("Lets go ...");

    let debug = observers("debug")();
    let debug = debug.as_ref();

    // ------------------
    let f = execs("len", Value::Int(4), None, Some(debug));
    assert!(f(&"Blub"));

    #[cfg(feature = "regex")]
    {
        let f = execs("regex", Value::Text(String::from("B.*")), None, Some(debug));
        assert!(f(&"Blub"));
    }
    let f = execs(
        "=",
        Value::Text(String::from("bLUb")),
        Some('i'),
        Some(debug),
    );
    assert!(f(&"Blub"));

    // ------------------

    #[cfg(feature = "regex")]
    {
        let reg = predicate(Value::Text(String::from("B.*")), "regex", None, None);
        assert!(reg(&"Blub"));
        assert!(reg(&"B"));
    }

    let f0 = predicate(Value::Text(String::from("Blub")), "=", None, None);
    assert!(f0(&"Blub"));

    let f1 = predicate(
        Value::Text(String::from("blub")),
        "=",
        Some('i'),
        Some(debug),
    );
    let f2 = predicate(Value::Int(3), "len", None, Some(debug));

    println!("\n-------------------");
    assert!(!and(&f1, &f2, Some(debug))(&"Blub"));
    println!();
    assert!(!and(&f2, &f1, Some(debug))(&"Blub"));

    println!();
    assert!(or(&f1, &f2, Some(debug))(&"Blub"));
    println!();
    assert!(or(&f2, &f1, Some(debug))(&"Blub"));
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
