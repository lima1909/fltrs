//! The operator mod contains operators, from which the predicates are defined.
//!
//! ### Overview:
//!
//! | operator      | meaning                           | example                        |
//! |---------------|---------------------------------- |------------------------------- |
//! | `=` or `==`   | equal                             | `= 5` or `name = "Peter"`      |
//! | `!=`          | not equal                         | `!= 5` or `name != "Peter"`    |
//! | `<`           | less                              | `< 5`                          |
//! | `<=`          | less equal                        | `<= 5`                         |
//! | `>`           | greater                           | `> 5`                          |
//! | `>=`          | greater equal                     | `>= 5`                         |
//! | `len`         | length of an string               | `name len 5`                   |
//! | `is_empty`    | string is empty                   | `name is_empty` or `name = ""` |
//! | `contains`    | string contains other string/char | `name contains "Pe"`           |
//! | `starts_with` | string starts with string/char    | `name starts_with "Pe"`        |
//! | `ends_with`   | string ends with string/char      | `name ends_with "er"`          |
//! | `one_of`      | one element from given list       | `x one_of [1, 3, 7]`           |
//! | `regex`       | regexpression (feature = "regex") | `x regex "[0-9]{2}"`           |
//!
use crate::token::Op;
use crate::value::Value;
use crate::{FltrError, PathResolver, Predicate, Result};

pub type OperatorFn<PR> = fn(idx: usize, v: Value) -> Result<Predicate<PR>>;

pub struct Operators<PR> {
    pub(crate) op: Vec<(&'static str, OperatorFn<PR>)>,
}

impl<PR: PathResolver> Default for Operators<PR> {
    fn default() -> Self {
        Self {
            op: vec![
                ("==", eq as OperatorFn<PR>),
                ("=", eq),
                ("!=", ne),
                ("<=", le),
                ("<", lt),
                (">=", ge),
                (">", gt),
                ("len", len),
                ("is_empty", is_empty),
                ("contains", contains),
                ("starts_with", starts_with),
                ("ends_with", ends_with),
                ("one_of", one_of),
                #[cfg(feature = "regex")]
                ("regex", regex),
            ],
        }
    }
}

impl<PR: PathResolver> Operators<PR> {
    pub fn get(&self, op: &Op, idx: usize, v: Value) -> Result<Predicate<PR>> {
        for (n, f) in &self.op {
            if n == &op.name {
                return f(idx, v);
            }
        }
        Err(FltrError(format!("invalid operation: '{}'", op)))
    }

    pub fn get_ops_names(&self) -> Vec<&'static str> {
        self.op.iter().map(|(s, _)| *s).collect()
    }
}

fn eq<PR: PathResolver>(idx: usize, v: Value) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| pr.value(idx) == &v))
}

fn ne<PR: PathResolver>(idx: usize, v: Value) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| pr.value(idx) != &v))
}

fn le<PR: PathResolver>(idx: usize, v: Value) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| pr.value(idx).le(&v)))
}

fn lt<PR: PathResolver>(idx: usize, v: Value) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| pr.value(idx).lt(&v)))
}

fn ge<PR: PathResolver>(idx: usize, v: Value) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| pr.value(idx).ge(&v)))
}

fn gt<PR: PathResolver>(idx: usize, v: Value) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| pr.value(idx).gt(&v)))
}

fn len<PR: PathResolver>(idx: usize, v: Value) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| match v {
        Value::Int(l) => pr.value(idx).as_string().len() == l as usize,
        _ => false,
    }))
}

fn is_empty<PR: PathResolver>(idx: usize, v: Value) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| match &v {
        Value::Null => pr.value(idx).as_string().is_empty(),
        _ => false,
    }))
}

fn contains<PR: PathResolver>(idx: usize, v: Value) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| match &v {
        Value::Text(t) => pr.value(idx).as_string().contains(t),
        Value::Char(c) => pr.value(idx).as_string().contains(*c),
        _ => false,
    }))
}

fn starts_with<PR: PathResolver>(idx: usize, v: Value) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| match &v {
        Value::Text(t) => pr.value(idx).as_string().starts_with(t),
        Value::Char(c) => pr.value(idx).as_string().starts_with(*c),
        _ => false,
    }))
}

fn ends_with<PR: PathResolver>(idx: usize, v: Value) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| match &v {
        Value::Text(t) => pr.value(idx).as_string().ends_with(t),
        Value::Char(c) => pr.value(idx).as_string().ends_with(*c),
        _ => false,
    }))
}

fn one_of<PR: PathResolver>(idx: usize, v: Value) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| {
        if let Value::List(vs) = &v {
            return vs.iter().filter(|v| pr.value(idx).eq(*v)).count() > 0;
        }
        pr.value(idx).eq(&v)
    }))
}

#[cfg(feature = "regex")]
fn regex<PR: PathResolver>(idx: usize, v: Value) -> Result<Predicate<PR>> {
    let rg = regex::Regex::new(&v.to_string()).or_else(|e| Err(FltrError(e.to_string())))?;
    Ok(Box::new(move |pr| rg.is_match(&pr.value(idx).as_string())))
}

#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    #[test]
    fn get() {
        let op = Operators::<bool>::default();
        assert!(op.get(&Op::from_str("="), 0, Value::Bool(true)).is_ok());
        assert!(op.get(&Op::from_str("foo"), 0, Value::Bool(true)).is_err());
    }

    #[test]
    fn exec_bool() {
        let op = Operators::default();
        let ne = op.get(&Op::from_str("!="), 0, Value::Bool(false)).unwrap();
        assert!((ne)(&true));
    }

    #[test]
    fn exec_len_string() {
        let op = Operators::default();
        let len = op.get(&Op::from_str("len"), 0, Value::Int(4)).unwrap();
        assert!((len)(&String::from("Paul")));
    }

    #[test]
    fn exec_len_str() {
        let op = Operators::default();
        let len = op.get(&Op::from_str("len"), 0, Value::Int(4)).unwrap();
        assert!((len)(&"Paul"));
    }

    #[test]
    fn exec_contains_str() {
        let op = Operators::default();
        let starts_with = op
            .get(&Op::from_str("contains"), 0, Value::Text("au".into()))
            .unwrap();
        assert!((starts_with)(&"Paul"));
    }

    #[test]
    fn exec_contains_char() {
        let op = Operators::default();
        let contains = op
            .get(&Op::from_str("contains"), 0, Value::Char('u'))
            .unwrap();
        assert!((contains)(&"Paul"));
    }

    #[test]
    fn exec_starts_with_str() {
        let op = Operators::default();
        let starts_with = op
            .get(&Op::from_str("starts_with"), 0, Value::Text("Pa".into()))
            .unwrap();
        assert!((starts_with)(&"Paul"));
    }

    #[test]
    fn exec_starts_with_char() {
        let op = Operators::default();
        let starts_with = op
            .get(&Op::from_str("starts_with"), 0, Value::Char('P'))
            .unwrap();
        assert!((starts_with)(&"Paul"));
    }

    #[test]
    fn exec_ends_with_str() {
        let op = Operators::default();
        let ends_with = op
            .get(&Op::from_str("ends_with"), 0, Value::Text("aul".into()))
            .unwrap();
        assert!((ends_with)(&"Paul"));
    }

    #[test]
    fn exec_ends_with_char() {
        let op = Operators::default();
        let ends_with = op
            .get(&Op::from_str("ends_with"), 0, Value::Char('l'))
            .unwrap();
        assert!((ends_with)(&"Paul"));

        let ends_with = op
            .get(&Op::from_str("ends_with"), 0, Value::Char('x'))
            .unwrap();
        assert!(!(ends_with)(&"Paul"));
    }

    #[test]
    fn exec_one_of_str() {
        let op = Operators::default();
        let one_of = op
            .get(
                &Op::from_str("one_of"),
                0,
                Value::List(vec![Value::Text("Inge".into()), Value::Text("Paul".into())]),
            )
            .unwrap();
        assert!((one_of)(&"Paul"));
    }

    #[test_case(Op::from_str("=="),  'f', Value::Char('f')  ; "eqeq 'f'")]
    #[test_case(Op::from_str("="),  'f', Value::Char('f')  ; "eq 'f'")]
    #[test_case(Op::from_str("!="),  'g', Value::Char('f')  ; "ne 'g'")]
    #[test_case(Op::from_str(">"),  'g', Value::Char('f')  ; "gt 'g'")]
    #[test_case(Op::from_str("<"),  'a', Value::Char('f')  ; "lt 'a'")]
    fn ops_char(op: Op, arg: char, val: Value) {
        let ops = Operators::default();
        let exec = ops.get(&op, 0, val).unwrap();
        assert!((exec)(&arg));
    }

    #[test_case(Op::from_str("="),  4.2, Value::Float(4.2)  ; "eq 4.2")]
    #[test_case(Op::from_str("!="),  4.2, Value::Float(5.3)  ; "ne 4.2")]
    #[test_case(Op::from_str(">"),  4.2, Value::Float(3.1)  ; "gt 4.2")]
    #[test_case(Op::from_str("<"),  4.2, Value::Float(5.3)  ; "lt 4.2")]
    fn ops_f32(op: Op, arg: f32, val: Value) {
        let ops = Operators::default();
        let exec = ops.get(&op, 0, val).unwrap();
        assert!((exec)(&arg));
    }

    #[cfg(feature = "regex")]
    #[test_case("[0-9]{2}-[0-9]{1}-[0-9]{2}", "34-5-67" => Ok(true)  ; "34-5-67")]
    #[test_case("[0-9]{2}-[0-9]{1}-[0-9]{2}", "1-1-1" => Ok(false)  ; "1-1-1")]
    #[test_case("[0-9]{2-[0-9]{1}-[0-9]{2}", "1-1-1" => Err(FltrError("regex parse error:\n    [0-9]{2-[0-9]{1}-[0-9]{2}\n         ^^\nerror: unclosed counted repetition".into()))  ; "error")]
    fn ops_regex(regex: &str, input: &str) -> Result<bool> {
        let ops = Operators::default();
        let exec = ops.get(&Op::from_str("regex"), 0, Value::Text(regex.to_string()))?;
        Ok((exec)(&input))
    }
}
