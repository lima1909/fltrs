use crate::value::{Number, Value};
use crate::Filterable;

pub type OperatorFn = fn(arg: &dyn Filterable, v: &Value) -> bool;

pub struct Operators {
    op: Vec<(&'static str, OperatorFn)>,
}

impl Default for Operators {
    fn default() -> Self {
        Self {
            op: vec![
                ("=", eq as OperatorFn),
                ("!=", ne),
                ("<=", le),
                ("<", lt),
                (">=", ge),
                (">", gt),
                ("len", len),
                ("starts_with", starts_with),
                ("one_of", one_of),
            ],
        }
    }
}

impl Operators {
    pub fn get(&self, op: &str) -> Option<OperatorFn> {
        for (n, f) in &self.op {
            if n == &op {
                return Some(*f);
            }
        }
        None
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

#[inline(always)]
fn eq(arg: &dyn Filterable, v: &Value) -> bool {
    arg.eq(v)
}

#[inline(always)]
fn ne(arg: &dyn Filterable, v: &Value) -> bool {
    arg.ne(v)
}

#[inline(always)]
fn ge(arg: &dyn Filterable, v: &Value) -> bool {
    arg.ge(v)
}

#[inline(always)]
fn gt(arg: &dyn Filterable, v: &Value) -> bool {
    arg.gt(v)
}

#[inline(always)]
fn le(arg: &dyn Filterable, v: &Value) -> bool {
    arg.le(v)
}

#[inline(always)]
fn lt(arg: &dyn Filterable, v: &Value) -> bool {
    arg.lt(v)
}

fn len(arg: &dyn Filterable, v: &Value) -> bool {
    match v {
        Value::Number(Number::Usize(l)) => arg.to_string().len() == *l,
        Value::Number(Number::I32(l)) => arg.to_string().len() == *l as usize,
        _ => false,
    }
}

fn starts_with(arg: &dyn Filterable, v: &Value) -> bool {
    if let Value::Text(s) = v {
        return arg.to_string().starts_with(s);
    }
    false
}

fn one_of(arg: &dyn Filterable, v: &Value) -> bool {
    if let Value::List(vs) = v {
        return vs.iter().filter(|v| arg.eq(*v)).count() > 0;
    }
    arg.eq(v)
}

#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    #[test]
    fn starts_with_valid_op() {
        let op = Operators::default();
        assert_eq!(Some("="), op.starts_with_valid_op("="));
        assert_eq!(Some("="), op.starts_with_valid_op("=7"));
        assert_eq!(None, op.starts_with_valid_op("foo"));
    }

    #[test]
    fn get() {
        let op = Operators::default();
        assert!(op.get("=").is_some());
        assert!(op.get("foo").is_none());
    }

    #[test]
    fn exec_bool() {
        let op = Operators::default();
        let ne = op.get("!=").unwrap();
        assert!((ne)(&true, &Value::Bool(false)));
    }

    #[test]
    fn exec_len_string() {
        let op = Operators::default();
        let len = op.get("len").unwrap();
        assert!((len)(
            &String::from("Paul"),
            &Value::Number(Number::Usize(4))
        ));
    }

    #[test]
    fn exec_len_str() {
        let op = Operators::default();
        let len = op.get("len").unwrap();
        assert!((len)(&"Paul", &Value::Number(Number::Usize(4))));
    }

    #[test]
    fn exec_starts_with_str() {
        let op = Operators::default();
        let starts_with = op.get("starts_with").unwrap();
        assert!((starts_with)(&"Paul", &Value::Text("Pa".into())));
    }

    #[test]
    fn exec_one_of_str() {
        let op = Operators::default();
        let one_of = op.get("one_of").unwrap();
        assert!((one_of)(
            &"Paul",
            &Value::List(vec![Value::Text("Inge".into()), Value::Text("Paul".into())])
        ));

        assert!((one_of)(&"Paul", &Value::Text("Paul".into())));
    }

    #[test_case("=",  'f', Value::Char('f')  ; "eq 'f'")]
    #[test_case("!=",  'g', Value::Char('f')  ; "ne 'g'")]
    #[test_case(">",  'g', Value::Char('f')  ; "gt 'g'")]
    #[test_case("<",  'a', Value::Char('f')  ; "lt 'a'")]
    fn ops_char(op: &str, arg: char, val: Value) {
        let ops = Operators::default();
        let exec = ops.get(op).unwrap();
        assert!((exec)(&arg, &val));
    }

    #[test_case("=",  4.2, Value::Number(Number::F32(4.2))  ; "eq 4.2")]
    #[test_case("!=",  4.2, Value::Number(Number::F32(5.3))  ; "ne 4.2")]
    #[test_case(">",  4.2, Value::Number(Number::F32(3.1))  ; "gt 4.2")]
    #[test_case("<",  4.2, Value::Number(Number::F32(5.3))  ; "lt 4.2")]
    fn ops_f32(op: &str, arg: f32, val: Value) {
        let ops = Operators::default();
        let exec = ops.get(op).unwrap();
        assert!((exec)(&arg, &val));
    }
}
