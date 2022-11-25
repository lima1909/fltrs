//! The operator mod contains operators, from which the predicates are defined.
//!
//! ### Overview:
//!
//! | operator      | meaning                           | example                        | supported flags |
//! |---------------|-----------------------------------|--------------------------------|-----------------|
//! | `=` or `==`   | equal                             | `= 5` or `name = "Peter"`      | i               |
//! | `!=`          | not equal                         | `!= 5` or `name != "Peter"`    | i               |
//! | `<`           | less                              | `< 5`                          | i               |
//! | `<=`          | less equal                        | `<= 5`                         | i               |
//! | `>`           | greater                           | `> 5`                          | i               |
//! | `>=`          | greater equal                     | `>= 5`                         | i               |
//! | `contains`    | string contains other string/char | `name contains "Pe"`           | i               |
//! | `starts_with` | string starts with string/char    | `name starts_with "Pe"`        | i               |
//! | `ends_with`   | string ends with string/char      | `name ends_with "er"`          | i               |
//! | `one_of`      | one element from given list       | `x one_of [1, 3, 7]`           | i               |
//! | `len`         | length of an string               | `name len 5`                   |                 |
//! | `regex`       | regexpression (feature = "regex") | `x regex "[0-9]{2}"`           |                 |
//!
//! ### Flags
//!
//! | flag | meaning          | example                             | hint                                                      |
//! |------|------------------|-------------------------------------|-----------------------------------------------------------|
//! | `i`  | case insensitive |`=:i "ab"`<br /> (`ab, aB, Ab, AB`)  | this flag supported only `String`, `str` or `char` values |
//!
//!
#![allow(clippy::type_complexity)]
use crate::{AsString, Filterable, FltrError, Result, Value};

pub type PredicateFn = Box<dyn Fn(&dyn Filterable) -> bool>;

static DEFAULT_FLAGS: &[(
    char,
    fn(Value) -> Option<Value>,
    fn(PredicateFn) -> PredicateFn,
)] = &[(
    'i',
    |v| text_value_to_uppercase(&v),
    |f| Box::new(move |arg: &dyn Filterable| f(&arg.as_string().to_ascii_uppercase())),
)];

pub fn text_value_to_uppercase(value: &Value) -> Option<Value> {
    match value {
        Value::Char(c) => Some(Value::Text(c.to_ascii_uppercase().as_string())),
        Value::Text(t) => Some(Value::Text(t.to_ascii_uppercase())),
        Value::List(l) => {
            let mut result = vec![];
            for v in l {
                if let Some(x) = text_value_to_uppercase(v) {
                    result.push(x);
                } else {
                    return None;
                }
            }
            Some(Value::List(result))
        }
        _ => None,
    }
}

pub fn flags(v: Value, flag: Option<char>) -> Result<(Value, fn(PredicateFn) -> PredicateFn)> {
    if let Some(f) = flag {
        for (flag, vfn, pfn) in DEFAULT_FLAGS {
            if flag == &f {
                let value = vfn(v.clone()).ok_or_else(|| {
                    FltrError(format!(
                        "the flag: '{f}' supported only 'String' and 'char' values, not: '{v}'"
                    ))
                })?;
                return Ok((value, *pfn));
            }
        }
        return Err(FltrError(format!("the flag: '{f}' is not supported",)));
    }

    Ok((v, |f| f))
}

static DEFAULT_OPS: &[Operator] = &[
    Operator::new("==", eq, &['i']),
    Operator::new("=", eq, &['i']),
    Operator::new("!=", ne, &['i']),
    Operator::new("<=", le, &['i']),
    Operator::new("<", lt, &['i']),
    Operator::new(">=", ge, &['i']),
    Operator::new(">", gt, &['i']),
    Operator::new("contains", contains, &['i']),
    Operator::new("starts_with", starts_with, &['i']),
    Operator::new("ends_with", ends_with, &['i']),
    Operator::new("one_of", one_of, &['i']),
    Operator::new("len", len, &[]),
    #[cfg(feature = "regex")]
    (Operator::new("regex", regex, &[])),
];

#[derive(Clone)]
pub struct Operator {
    name: &'static str,
    f: fn(Value) -> Result<PredicateFn>,
    flags: &'static [char],
}

impl Operator {
    pub const fn new(
        name: &'static str,
        f: fn(Value) -> Result<PredicateFn>,
        flags: &'static [char],
    ) -> Self {
        Self { name, f, flags }
    }

    pub const fn flags(&self) -> &[char] {
        self.flags
    }
}

pub struct Operators {
    pub(crate) ops: Vec<Operator>,
}

impl Default for Operators {
    fn default() -> Self {
        Self {
            ops: Vec::from(DEFAULT_OPS),
        }
    }
}

impl Operators {
    pub fn ops(&self, op: &str, v: Value) -> Result<PredicateFn> {
        self.ops
            .iter()
            .find(|dop| dop.name == op)
            .map(|dop| (dop.f)(v))
            .unwrap_or_else(|| Err(FltrError(format!("invalid operation: '{}'", op))))
    }

    pub fn ops_names(&self) -> Vec<&'static str> {
        self.ops.iter().map(|dop| dop.name).collect()
    }
}

///////////////////////////////////////////////////////////////////////////////
// Operator implementations
///////////////////////////////////////////////////////////////////////////////
fn eq(v: Value) -> Result<PredicateFn> {
    Ok(Box::new(move |arg: &dyn Filterable| arg == &v))
}

fn ne(v: Value) -> Result<PredicateFn> {
    Ok(Box::new(move |arg: &dyn Filterable| arg != &v))
}

fn le(v: Value) -> Result<PredicateFn> {
    Ok(Box::new(move |arg: &dyn Filterable| arg.le(&v)))
}

fn lt(v: Value) -> Result<PredicateFn> {
    Ok(Box::new(move |arg: &dyn Filterable| arg.lt(&v)))
}

fn ge(v: Value) -> Result<PredicateFn> {
    Ok(Box::new(move |arg: &dyn Filterable| arg.ge(&v)))
}

fn gt(v: Value) -> Result<PredicateFn> {
    Ok(Box::new(move |arg: &dyn Filterable| arg.gt(&v)))
}

fn len(v: Value) -> Result<PredicateFn> {
    let Value::Int(l) = v else {
        return Ok(Box::new(move |_: &dyn Filterable| false));
    };
    Ok(Box::new(move |arg: &dyn Filterable| {
        arg.as_string().len() == l as usize
    }))
}

fn contains(v: Value) -> Result<PredicateFn> {
    match v {
        Value::Text(t) => Ok(Box::new(move |f: &dyn Filterable| {
            f.as_string().contains(&t)
        })),
        Value::Char(c) => Ok(Box::new(move |f: &dyn Filterable| {
            f.as_string().contains(c)
        })),
        _ => Ok(Box::new(move |_: &dyn Filterable| false)),
    }
}

fn starts_with(v: Value) -> Result<PredicateFn> {
    match v {
        Value::Text(t) => Ok(Box::new(move |f: &dyn Filterable| {
            f.as_string().starts_with(&t)
        })),
        Value::Char(c) => Ok(Box::new(move |f: &dyn Filterable| {
            f.as_string().starts_with(c)
        })),
        _ => Ok(Box::new(move |_: &dyn Filterable| false)),
    }
}

fn ends_with(v: Value) -> Result<PredicateFn> {
    match v {
        Value::Text(t) => Ok(Box::new(move |f: &dyn Filterable| {
            f.as_string().ends_with(&t)
        })),
        Value::Char(c) => Ok(Box::new(move |f: &dyn Filterable| {
            f.as_string().ends_with(c)
        })),
        _ => Ok(Box::new(move |_: &dyn Filterable| false)),
    }
}

fn one_of(v: Value) -> Result<PredicateFn> {
    if let Value::List(vs) = v {
        return Ok(Box::new(move |f: &dyn Filterable| {
            vs.iter().any(|value| f.eq(value))
        }));
    }
    Ok(Box::new(move |f: &dyn Filterable| f.eq(&v)))
}

#[cfg(feature = "regex")]
fn regex(v: Value) -> Result<PredicateFn> {
    let regex = regex::Regex::new(&v.as_string()).or_else(|e| Err(FltrError(e.to_string())))?;
    Ok(Box::new(move |arg: &dyn Filterable| {
        regex.is_match(&arg.as_string())
    }))
}

#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    #[test]
    fn get() {
        assert!(Operators::default().ops("=", Value::Bool(true)).is_ok());
        assert!(Operators::default().ops("foo", Value::Bool(true)).is_err());
    }

    #[test]
    fn exec_bool() {
        let ne = Operators::default().ops("!=", Value::Bool(false)).unwrap();
        assert!((ne)(&true));
    }

    #[test]
    fn exec_len_string() {
        let len = Operators::default().ops("len", Value::Int(4)).unwrap();
        assert!((len)(&String::from("Paul")));
    }

    #[test]
    fn exec_len_str() {
        let len = Operators::default().ops("len", Value::Int(4)).unwrap();
        assert!((len)(&"Paul"));
    }

    #[test]
    fn exec_contains_str() {
        let starts_with = Operators::default()
            .ops("contains", Value::Text("au".into()))
            .unwrap();
        assert!((starts_with)(&"Paul"));
    }

    #[test]
    fn exec_contains_char() {
        let contains = Operators::default()
            .ops("contains", Value::Char('u'))
            .unwrap();
        assert!((contains)(&"Paul"));
    }

    #[test]
    fn exec_starts_with_str() {
        let starts_with = Operators::default()
            .ops("starts_with", Value::Text("Pa".into()))
            .unwrap();
        assert!((starts_with)(&"Paul"));
    }

    #[test]
    fn exec_starts_with_char() {
        let starts_with = Operators::default()
            .ops("starts_with", Value::Char('P'))
            .unwrap();
        assert!((starts_with)(&"Paul"));
    }

    #[test]
    fn exec_ends_with_str() {
        let ends_with = Operators::default()
            .ops("ends_with", Value::Text("aul".into()))
            .unwrap();
        assert!((ends_with)(&"Paul"));
    }

    #[test]
    fn exec_ends_with_char() {
        let ends_with = Operators::default()
            .ops("ends_with", Value::Char('l'))
            .unwrap();
        assert!((ends_with)(&"Paul"));

        let ends_with = Operators::default()
            .ops("ends_with", Value::Char('x'))
            .unwrap();
        assert!(!(ends_with)(&"Paul"));
    }

    #[test]
    fn exec_one_of_str() {
        let one_of = Operators::default()
            .ops(
                "one_of",
                Value::List(vec![Value::Text("Inge".into()), Value::Text("Paul".into())]),
            )
            .unwrap();
        assert!((one_of)(&"Paul"));
    }

    #[test_case("==",  'f', Value::Char('f')  ; "eqeq 'f'")]
    #[test_case("=",  'f', Value::Char('f')  ; "eq 'f'")]
    #[test_case("!=",  'g', Value::Char('f')  ; "ne 'g'")]
    #[test_case(">",  'g', Value::Char('f')  ; "gt 'g'")]
    #[test_case("<",  'a', Value::Char('f')  ; "lt 'a'")]
    fn ops_char(op: &str, arg: char, val: Value) {
        let exec = Operators::default().ops(op, val).unwrap();
        assert!((exec)(&arg));
    }

    #[test_case("=",  4.2, Value::Float(4.2)  ; "eq 4.2")]
    #[test_case("!=",  4.2, Value::Float(5.3)  ; "ne 4.2")]
    #[test_case(">",  4.2, Value::Float(3.1)  ; "gt 4.2")]
    #[test_case("<",  4.2, Value::Float(5.3)  ; "lt 4.2")]
    fn ops_f32(op: &str, arg: f32, val: Value) {
        let exec = Operators::default().ops(op, val).unwrap();
        assert!((exec)(&arg));
    }

    #[cfg(feature = "regex")]
    #[test_case("[0-9]{2}-[0-9]{1}-[0-9]{2}", "34-5-67" => Ok(true)  ; "34-5-67")]
    #[test_case("[0-9]{2}-[0-9]{1}-[0-9]{2}", "1-1-1" => Ok(false)  ; "1-1-1")]
    #[test_case("[0-9]{2-[0-9]{1}-[0-9]{2}", "1-1-1" => Err(FltrError("regex parse error:\n    [0-9]{2-[0-9]{1}-[0-9]{2}\n         ^^\nerror: unclosed counted repetition".into()))  ; "error")]
    fn ops_regex(regex: &str, input: &str) -> Result<bool> {
        let exec = Operators::default().ops("regex", Value::Text(regex.to_string()))?;
        Ok((exec)(&input))
    }

    #[test_case(Value::Text("a".into()) => Some(Value::Text("A".into())) ; "Text: a -> A")]
    #[test_case(Value::Text("ab_c".into()) => Some(Value::Text("AB_C".into())))]
    #[test_case(Value::Text("A".into()) => Some(Value::Text("A".into())))]
    #[test_case(Value::Text("_:-,;".into()) => Some(Value::Text("_:-,;".into())))]
    #[test_case(Value::Text("7".into()) => Some(Value::Text("7".into())))]
    #[test_case(Value::Char('a') => Some(Value::Text("A".into())); "Char: a -> A")]
    #[test_case(Value::Char('A') => Some(Value::Text("A".into())))]
    #[test_case(Value::List(vec![Value::Text("42".into())]) => Some(Value::List(vec![Value::Text("42".into())])))]
    #[test_case(Value::List(vec![Value::Text("x".into()), Value::Text("y".into())]) => Some(Value::List(vec![Value::Text("X".into()), Value::Text("Y".into())])))]
    #[test_case(Value::List(vec![Value::Text("x_7".into()), Value::Text(" y ".into())]) => Some(Value::List(vec![Value::Text("X_7".into()), Value::Text(" Y ".into())])))]
    #[test_case(Value::List(vec![Value::List(vec![Value::Text("x".into()), Value::Text("y".into())])])  => Some(Value::List(vec![Value::List(vec![Value::Text("X".into()), Value::Text("Y".into())])])))]
    #[test_case(Value::Bool(true) => None)]
    #[test_case(Value::Int(42) => None)]
    #[test_case(Value::Float(4.2) => None)]
    #[test_case(Value::List(vec![Value::Text("x".into()), Value::Int(7)]) => None)]
    fn text_value_to_uppercase_check(v: Value) -> Option<Value> {
        text_value_to_uppercase(&v)
    }
}
