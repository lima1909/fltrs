use crate::value::Value;
use crate::{FltrError, PathResolver, Predicate, Result};

pub type OperatorFn<PR> = fn(idx: usize, v: Value) -> Result<Predicate<PR>>;

pub struct Operators<PR> {
    op: Vec<(&'static str, OperatorFn<PR>)>,
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
                ("starts_with", starts_with),
                ("one_of", one_of),
                #[cfg(feature = "regexp")]
                ("regex", regex),
            ],
        }
    }
}

impl<PR> Operators<PR> {
    pub fn get(&self, op: &str, idx: usize, v: Value) -> Result<Predicate<PR>> {
        for (n, f) in &self.op {
            if n == &op {
                return f(idx, v);
            }
        }
        Err(FltrError(format!("invalid operation: '{}'", op)))
    }

    pub fn starts_with_valid_op(&self, op: &str) -> Option<&str> {
        for (n, _) in &self.op {
            if op.starts_with(n) {
                return Some(n);
            }
        }
        None
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
        Value::Int(l) => pr.value(idx).to_string().len() == l as usize,
        _ => false,
    }))
}

fn starts_with<PR: PathResolver>(idx: usize, v: Value) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| {
        if let Value::Text(s) = &v {
            return pr.value(idx).to_string().starts_with(s);
        }
        false
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

#[cfg(feature = "regexp")]
fn regex<PR: PathResolver>(idx: usize, v: Value) -> Result<Predicate<PR>> {
    let rg = regex::Regex::new(&v.to_string()).or_else(|e| Err(FltrError(e.to_string())))?;
    Ok(Box::new(move |pr| rg.is_match(&pr.value(idx).to_string())))
}

#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    #[test]
    fn starts_with_valid_op() {
        let op = Operators::<bool>::default();
        assert_eq!(Some("=="), op.starts_with_valid_op("=="));
        assert_eq!(Some("="), op.starts_with_valid_op("="));
        assert_eq!(Some("="), op.starts_with_valid_op("=7"));
        assert_eq!(None, op.starts_with_valid_op("foo"));
    }

    #[test]
    fn get() {
        let op = Operators::<bool>::default();
        assert!(op.get("=", 0, Value::Bool(true)).is_ok());
        assert!(op.get("foo", 0, Value::Bool(true)).is_err());
    }

    #[test]
    fn exec_bool() {
        let op = Operators::default();
        let ne = op.get("!=", 0, Value::Bool(false)).unwrap();
        assert!((ne)(&true));
    }

    #[test]
    fn exec_len_string() {
        let op = Operators::default();
        let len = op.get("len", 0, Value::Int(4)).unwrap();
        assert!((len)(&String::from("Paul")));
    }

    #[test]
    fn exec_len_str() {
        let op = Operators::default();
        let len = op.get("len", 0, Value::Int(4)).unwrap();
        assert!((len)(&"Paul"));
    }

    #[test]
    fn exec_starts_with_str() {
        let op = Operators::default();
        let starts_with = op.get("starts_with", 0, Value::Text("Pa".into())).unwrap();
        assert!((starts_with)(&"Paul"));
    }

    #[test]
    fn exec_one_of_str() {
        let op = Operators::default();
        let one_of = op
            .get(
                "one_of",
                0,
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
        let ops = Operators::default();
        let exec = ops.get(op, 0, val).unwrap();
        assert!((exec)(&arg));
    }

    #[test_case("=",  4.2, Value::Float(4.2)  ; "eq 4.2")]
    #[test_case("!=",  4.2, Value::Float(5.3)  ; "ne 4.2")]
    #[test_case(">",  4.2, Value::Float(3.1)  ; "gt 4.2")]
    #[test_case("<",  4.2, Value::Float(5.3)  ; "lt 4.2")]
    fn ops_f32(op: &str, arg: f32, val: Value) {
        let ops = Operators::default();
        let exec = ops.get(op, 0, val).unwrap();
        assert!((exec)(&arg));
    }

    #[cfg(feature = "regexp")]
    #[test_case("[0-9]{2}-[0-9]{1}-[0-9]{2}", "34-5-67" => Ok(true)  ; "34-5-67")]
    #[test_case("[0-9]{2}-[0-9]{1}-[0-9]{2}", "1-1-1" => Ok(false)  ; "1-1-1")]
    #[test_case("[0-9]{2-[0-9]{1}-[0-9]{2}", "1-1-1" => Err(FltrError("regex parse error:\n    [0-9]{2-[0-9]{1}-[0-9]{2}\n         ^^\nerror: unclosed counted repetition".into()))  ; "error")]
    fn ops_regex(regex: &str, input: &str) -> Result<bool> {
        let ops = Operators::default();
        let exec = ops.get("regex", 0, Value::Text(regex.to_string()))?;
        Ok((exec)(&input))
    }
}
