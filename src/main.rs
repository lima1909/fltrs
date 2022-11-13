#![allow(clippy::type_complexity)]
#[cfg(feature = "regex")]
use fltrs::AsString;
use fltrs::{operator::text_value_to_uppercase, path_resolver, Filterable, PathResolver, Value};

pub type PredicateFn = Box<dyn Fn(&dyn Filterable) -> bool>;

static DEFAULT_OBSERVERS: &[(&str, fn() -> Box<dyn Observer>)] =
    &[("debug", || Box::new(DebugObserver {}))];

fn observers(observer: &str) -> Option<Box<dyn Observer>> {
    DEFAULT_OBSERVERS
        .iter()
        .find(|(name, _)| *name == observer)
        .cloned()
        .map(|(_, f)| f())
}

static DEFAULT_FLAGS: &[(char, fn(Value) -> Value, fn(PredicateFn) -> PredicateFn)] = &[(
    'i',
    |v| text_value_to_uppercase(&v).unwrap(),
    |f| Box::new(move |arg: &dyn Filterable| f(&arg.as_string().to_ascii_uppercase())),
)];

fn flags(v: Value, flag: Option<char>) -> (Value, fn(PredicateFn) -> PredicateFn) {
    if let Some(f) = flag {
        return DEFAULT_FLAGS
            .iter()
            .find(|(flag, _, _)| flag == &f)
            .cloned()
            .map(|(_, vfn, pfn)| (vfn(v.clone()), pfn))
            .unwrap_or((v, |f| f));
    }
    (v, |f| f)
}

static DEFAULT_OPS: &[(&str, fn(Value) -> PredicateFn)] = &[
    ("=", eq),
    ("!=", neq),
    ("len", len),
    #[cfg(feature = "regex")]
    {
        ("regex", regex)
    },
];

fn ops(op: &str, v: Value) -> Option<PredicateFn> {
    DEFAULT_OPS
        .iter()
        .find(|(dop, _)| *dop == op)
        .map(|(_, factory)| factory(v))
}

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
    Box::new(move |arg: &dyn Filterable| arg.as_string().len() == l as usize)
}

#[cfg(feature = "regex")]
fn regex(v: Value) -> PredicateFn {
    let regex = regex::Regex::new(&v.as_string()).unwrap();
    Box::new(move |arg: &dyn Filterable| regex.is_match(&arg.as_string()))
}

pub type PredicateFnLt<'a, PR> = Box<dyn Fn(&PR) -> bool + 'a>;

fn create<'a, PR: PathResolver>(
    path: &'a str,
    op: &'a str,
    flag: Option<char>,
    v: Value,
    o: Option<&'a dyn Observer>,
) -> PredicateFnLt<'a, PR> {
    let (value, predicate_fn) = flags(v, flag);
    let predicate = ops(op, value.clone()).unwrap();
    let predicate = predicate_fn(predicate);

    let idx = PR::idx(path).unwrap();

    if let Some(obs) = o {
        return Box::new(move |pr: &PR| {
            let arg = pr.value(idx);
            let result = (predicate)(arg);
            obs.predicate(op, &value, arg, result);
            result
        });
    }

    Box::new(move |pr: &PR| predicate(pr.value(idx)))
}

fn and<'a, PR>(
    left: &'a PredicateFnLt<'a, PR>,
    right: &'a PredicateFnLt<'a, PR>,
    o: Option<&'a dyn Observer>,
) -> PredicateFnLt<'a, PR> {
    if let Some(o) = o {
        return Box::new(move |arg: &PR| {
            let result = left(arg) && right(arg);
            o.link("AND", result);
            result
        });
    }
    Box::new(move |arg: &PR| left(arg) && right(arg))
}

fn or<'a, PR>(
    left: &'a PredicateFnLt<'a, PR>,
    right: &'a PredicateFnLt<'a, PR>,
    o: Option<&'a dyn Observer>,
) -> PredicateFnLt<'a, PR> {
    if let Some(o) = o {
        return Box::new(move |arg: &PR| {
            let result = left(arg) || right(arg);
            o.link("OR", result);
            result
        });
    }
    Box::new(move |arg: &PR| left(arg) || right(arg))
}

trait Observer {
    fn predicate(&self, op: &str, inner: &Value, arg: &dyn Filterable, result: bool);
    fn link(&self, link: &str, result: bool);
}

struct DebugObserver;

// 5 [= 5] (true) 5 [= 6] (false) and (false)
// "Blub" [=i "blub"] ["BLUB" = "BLUB"] (true)
impl Observer for DebugObserver {
    fn predicate(&self, op: &str, inner: &Value, arg: &dyn Filterable, result: bool) {
        print!("{}: {op} {inner} [{result}] ", arg.as_string());
    }

    fn link(&self, link: &str, result: bool) {
        print!("{link} [{result}] ");
    }
}

fn main() {
    println!("Lets go ...");

    let debug = observers("debug").unwrap();
    let debug = debug.as_ref();

    // ------------------
    let len = create("", "len", None, Value::Int(4), Some(debug));

    #[cfg(feature = "regex")]
    {
        let rgx = create(
            "",
            "regex",
            None,
            Value::Text(String::from("B.*")),
            Some(debug),
        );
        assert!(and(&rgx, &len, Some(debug))(&"Blub"));
        println!();
    }
    let eq = create(
        "self",
        "=",
        Some('i'),
        Value::Text(String::from("bLUb")),
        Some(debug),
    );
    assert!(and(&and(&len, &eq, Some(debug)), &eq, Some(debug))(&"Blub"));
    println!();

    let eq = create(
        "name",
        "=",
        Some('i'),
        Value::Text("Mario".to_string()),
        Some(debug),
    );
    let neq = create("x", "!=", None, Value::Int(1), Some(debug));

    assert!((or(&neq, &eq, Some(debug)))(&Point {
        name: "MariO",
        x: 1,
        y: 2
    }));
}

struct Point {
    name: &'static str,
    x: i32,
    y: i32,
}

path_resolver!(Point: name, x, y);
