use crate::value::{Number, Value};
use core::fmt::Display;

pub type OperatorFn<Arg> = fn(arg: &Arg, v: &Value) -> bool;

pub struct Operators<Arg> {
    op: Vec<(&'static str, OperatorFn<Arg>)>,
}

impl<Arg> Operators<Arg>
where
    Arg: PartialEq<Value> + PartialOrd<Value> + Display,
{
    pub fn new() -> Self {
        Self {
            op: vec![
                ("=", Arg::eq as OperatorFn<Arg>),
                ("!=", Arg::ne),
                ("<=", Arg::le),
                ("<", Arg::lt),
                (">=", Arg::gt),
                (">", Arg::gt),
                ("len", len),
                ("starts_with", starts_with),
            ],
        }
    }

    pub fn get(&self, op: &str) -> Option<&OperatorFn<Arg>> {
        for (n, f) in &self.op {
            if n == &op {
                return Some(f);
            }
        }
        None
    }

    pub fn is_valid(&self, op: &str) -> bool {
        for (n, _) in &self.op {
            if n == &op {
                return true;
            }
        }
        false
    }
}

fn len<Arg: ToString>(arg: &Arg, v: &Value) -> bool {
    if let Value::Number(Number::Usize(l)) = v {
        return arg.to_string().len() == *l;
    }
    false
}

fn starts_with<Arg: ToString>(arg: &Arg, v: &Value) -> bool {
    if let Value::String(s) = v {
        return arg.to_string().starts_with(s);
    }
    false
}

#[cfg(test)]
mod test {
    use super::*;
    use test_case::test_case;

    #[test]
    fn is_valid() {
        let op = Operators::<i32>::new();
        assert!(op.is_valid("="));
        assert!(!op.is_valid("foo"));
    }

    #[test]
    fn get() {
        let op = Operators::<i32>::new();
        assert!(op.get("=").is_some());
        assert!(op.get("foo").is_none());
    }

    #[test]
    fn exec_bool() {
        let op = Operators::new();
        let ne = op.get("!=").unwrap();
        assert!((ne)(&true, &Value::Bool(false)));
    }

    #[test]
    fn exec_len_string() {
        let op = Operators::new();
        let len = op.get("len").unwrap();
        assert!((len)(
            &String::from("Paul"),
            &Value::Number(Number::Usize(4))
        ));
    }

    #[test]
    fn exec_len_str() {
        let op = Operators::new();
        let len = op.get("len").unwrap();
        assert!((len)(&"Paul", &Value::Number(Number::Usize(4))));
    }

    #[test]
    fn exec_starts_with_str() {
        let op = Operators::new();
        let starts_with = op.get("starts_with").unwrap();
        assert!((starts_with)(&"Paul", &Value::String("Pa".into())));
    }

    #[test_case("=",  'f', Value::Char('f')  ; "eq 'f'")]
    #[test_case("!=",  'g', Value::Char('f')  ; "ne 'g'")]
    #[test_case(">",  'g', Value::Char('f')  ; "gt 'g'")]
    #[test_case("<",  'a', Value::Char('f')  ; "lt 'a'")]
    fn ops_char(op: &str, arg: char, val: Value) {
        let ops = Operators::new();
        let exec = ops.get(op).unwrap();
        assert!((exec)(&arg, &val));
    }

    #[test_case("=",  4.2, Value::Number(Number::F32(4.2))  ; "eq 4.2")]
    #[test_case("!=",  4.2, Value::Number(Number::F32(5.3))  ; "ne 4.2")]
    #[test_case(">",  4.2, Value::Number(Number::F32(3.1))  ; "gt 4.2")]
    #[test_case("<",  4.2, Value::Number(Number::F32(5.3))  ; "lt 4.2")]
    fn ops_f32(op: &str, arg: f32, val: Value) {
        let ops = Operators::new();
        let exec = ops.get(op).unwrap();
        assert!((exec)(&arg, &val));
    }
}