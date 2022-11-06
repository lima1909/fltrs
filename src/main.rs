#![allow(dead_code)]
use std::collections::HashMap;
use std::fmt::{Debug, Display};

/// Is the function for the given operator.
/// e.g: Op: "=" -> function: |a,b| a == b
type OpFn = fn(inner: &Value, arg: &dyn Filterable) -> bool;

// ------------
// op: len -> Value::Int? => l
// op: regex -> Value::to_string => Regex
// flag: i -> Value::to_string::to_ascii_uppercase => Value::Text
//
// Value -> irgendwas?
//
// result: Box<dyn Fn(&dyn Filterable) -> bool>
// trait Executor {
//     fn exec(&self, arg: &dyn Filterable) -> bool;
// }

trait FuncFactory {
    fn create(&self) -> PredicateFn;
}

struct Len(Option<usize>);

impl Len {
    fn new(v: Value) -> Self {
        if let Value::Int(l) = v {
            return Self(Some(l as usize));
        }
        Self(None)
    }
}

// impl Executor for Len {
//     fn exec(&self, arg: &dyn Filterable) -> bool {
//         self.0.map(|l| arg.to_string().len() == l).unwrap_or(false)
//     }
// }

impl FuncFactory for Len {
    fn create(&self) -> Box<dyn Fn(&dyn Filterable) -> bool> {
        let len = self.0;
        Box::new(move |arg: &dyn Filterable| {
            len.map(|l| arg.to_string().len() == l).unwrap_or(false)
        })
    }
}

#[cfg(feature = "regex")]
struct Regex(regex::Regex);

#[cfg(feature = "regex")]
impl Regex {
    fn new(v: Value) -> Self {
        Self(regex::Regex::new(&v.to_string()).unwrap())
    }
}

// impl Executor for Regex {
//     fn exec(&self, arg: &dyn Filterable) -> bool {
//         self.0.is_match(&arg.to_string())
//     }
// }
#[cfg(feature = "regex")]
impl FuncFactory for Regex {
    fn create(&self) -> Box<dyn Fn(&dyn Filterable) -> bool> {
        let regex = self.0.clone();
        Box::new(move |arg: &dyn Filterable| regex.is_match(&arg.to_string()))
    }
}

struct Eq {
    value: Value,
    flag: Option<char>,
}

impl Eq {
    fn new(v: Value, flag: Option<char>) -> Self {
        if let Some(f) = flag {
            if f == 'i' {
                return Self {
                    value: Value::Text((v).to_string().to_ascii_uppercase()),
                    flag,
                };
            }
        }
        Self { value: v, flag }
    }
}

// impl Executor for Eq {
//     fn exec(&self, arg: &dyn Filterable) -> bool {
//         if let Some(f) = self.flag {
//             if f == 'i' {
//                 return arg.to_string().to_ascii_uppercase() == self.value;
//             }
//         }
//         arg == &self.value
//     }
// }

impl FuncFactory for Eq {
    fn create(&self) -> Box<dyn Fn(&dyn Filterable) -> bool> {
        let value = self.value.clone();
        if let Some(f) = self.flag {
            if f == 'i' {
                return Box::new(move |arg: &dyn Filterable| {
                    arg.to_string().to_ascii_uppercase() == value
                });
            }
        }
        Box::new(move |arg: &dyn Filterable| arg == &value)
    }
}

// ---
// fn execs(op: &'static str, v: Value, flag: Option<char>) -> Box<dyn Executor + 'static> {
//     let mut execs: HashMap<&str, fn(Value, Option<char>) -> Box<dyn Executor>> = HashMap::new();

//     execs.insert("=", |v: Value, flag: Option<char>| {
//         Box::new(Eq::new(v, flag))
//     });
//     execs.insert("len", |v: Value, _| Box::new(Len::new(v)));
//     #[cfg(feature = "regex")]
//     execs.insert("regex", |v: Value, _| Box::new(Regex::new(v)));

//     let f = execs.get(op).unwrap();
//     f(v, flag)
// }
type FnFuncFactory = fn(Value, Option<char>) -> Box<dyn FuncFactory>;
fn execs(op: &'static str, v: Value, flag: Option<char>) -> PredicateFn {
    let mut execs: HashMap<&str, FnFuncFactory> = HashMap::new();

    execs.insert("=", |v: Value, flag: Option<char>| {
        Box::new(Eq::new(v, flag))
    });
    execs.insert("len", |v: Value, _| Box::new(Len::new(v)));
    #[cfg(feature = "regex")]
    execs.insert("regex", |v: Value, _| Box::new(Regex::new(v)));

    let f = execs.get(op).unwrap();
    f(v, flag).create()
}

// ------------

fn ops(op: &'static str) -> OpFn {
    let mut ops: HashMap<&str, OpFn> = HashMap::new();
    ops.insert("=", |inner: &Value, arg: &dyn Filterable| arg == inner);
    ops.insert("!=", |inner: &Value, arg: &dyn Filterable| arg != inner);
    ops.insert("len", op_len);
    #[cfg(feature = "regex")]
    ops.insert("regex", regex);

    *ops.get(op).unwrap()
}

fn op_len(inner: &Value, arg: &dyn Filterable) -> bool {
    if let Value::Int(l) = inner {
        return arg.to_string().len() == *l as usize;
    }
    false
}

#[cfg(feature = "regex")]
fn regex(inner: &Value, arg: &dyn Filterable) -> bool {
    let rg = regex::Regex::new(&inner.to_string()).unwrap();
    println!("-----CALL----");
    rg.is_match(&arg.to_string())
}

pub type PredicateFn = Box<dyn Fn(&dyn Filterable) -> bool>;
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
    fn predicate(&self, op: &str, inner: &Value, _arg: &dyn Filterable, result: bool) {
        print!("{op} {inner} [{result}] ");
    }

    fn link(&self, link: &str, _arg: &dyn Filterable, result: bool) {
        print!("{link} [{result}] ");
    }
}

fn main() {
    println!("Lets go ...");

    // ------------------
    let f = execs("len", Value::Int(4), None);
    assert!(f(&"Blub"));
    let f = execs("regex", Value::Text(String::from("B.*")), None);
    assert!(f(&"Blub"));
    let f = execs("=", Value::Text(String::from("bLUb")), Some('i'));
    assert!(f(&"Blub"));

    // ------------------

    #[cfg(feature = "regex")]
    {
        let reg = predicate(Value::Text(String::from("B.*")), "regex", None, None);
        assert!(reg(&"Blub"));
        assert!(reg(&"B"));
    }

    let debug = observers("debug")();
    let debug = debug.as_ref();

    let f0 = predicate(Value::Text(String::from("Blub")), "=", None, None);
    assert!(f0(&"Blub"));

    let f1 = predicate(
        Value::Text(String::from("blub")),
        "=",
        Some('i'),
        Some(debug),
    );
    let f2 = predicate(Value::Int(3), "len", None, Some(debug));

    println!("-------------------");
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
