use crate::value::{Number, Value};
use std::fmt::Display;

pub type PredicateFn<V, A> = fn(val: &V, arg: &A) -> bool;

trait Predicate<V, A>
where
    V: PartialEq<A> + PartialOrd<A> + Display,
    A: ToValue + Display,
{
    fn exec(&self, pos: usize, f: PredicateFn<V, A>, val: &V) -> bool;
}

struct Example {
    name: String,
    x: i32,
}

impl<V> Predicate<V, i32> for Example
where
    V: PartialEq<i32> + PartialOrd<i32> + Display,
{
    fn exec(&self, _pos: usize, f: PredicateFn<V, i32>, val: &V) -> bool {
        f(val, &self.x)
    }
}

impl<V> Predicate<V, String> for Example
where
    V: PartialEq<String> + PartialOrd<String> + Display,
{
    fn exec(&self, _pos: usize, f: PredicateFn<V, String>, val: &V) -> bool {
        f(val, &self.name)
    }
}

trait ToValue {
    fn to_value(&self) -> Value;
}

impl ToValue for String {
    fn to_value(&self) -> Value {
        Value::Text(self.clone())
    }
}

impl ToValue for i32 {
    fn to_value(&self) -> Value {
        Value::Number(Number::I32(*self))
    }
}

impl ToValue for usize {
    fn to_value(&self) -> Value {
        Value::Number(Number::Usize(*self))
    }
}

pub struct Operators<V, A> {
    op: Vec<(&'static str, PredicateFn<V, A>)>,
}

impl<V, A> Default for Operators<V, A>
where
    V: PartialEq<A> + PartialOrd<A> + Display,
    A: ToValue + Display,
{
    fn default() -> Self {
        Self {
            op: vec![
                ("=", PartialEq::eq as PredicateFn<V, A>),
                ("!=", PartialEq::ne),
                ("<=", PartialOrd::le),
                ("<", PartialOrd::lt),
                (">=", PartialOrd::ge),
                (">", PartialOrd::gt),
                ("starts_with", starts_with),
                ("len", len),
                // ("one_of", one_of),
            ],
        }
    }
}

impl<V, A> Operators<V, A> {
    pub fn get(&self, op: &str) -> Option<PredicateFn<V, A>> {
        for (n, f) in &self.op {
            if n == &op {
                return Some(*f);
            }
        }
        None
    }
}

fn starts_with<V, A>(v: &V, a: &A) -> bool
where
    A: Display,
    V: Display,
{
    a.to_string().starts_with(&v.to_string())
}

fn len<V, A>(v: &V, a: &A) -> bool
where
    V: Display,
    A: ToValue,
{
    let a: Value = a.to_value();
    match a {
        Value::Number(Number::Usize(l)) => v.to_string().len() == l,
        Value::Number(Number::I32(l)) => v.to_string().len() == l as usize,
        _ => false,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn eq() {
        let e = Example {
            name: String::from("Paul"),
            x: 42,
        };

        let ops = Operators::default();

        assert!(e.exec(0, ops.get("=").unwrap(), &String::from("Paul")));
        assert!(e.exec(0, ops.get("!=").unwrap(), &String::from("Peter")));
        assert!(e.exec(0, ops.get("starts_with").unwrap(), &String::from("Pa")));
        // assert!(e.exec(0, ops.get("len").unwrap(), &4));

        assert!(e.exec(0, PartialOrd::lt, &5));

        fn always_true<V, A>(_v: &V, _a: &A) -> bool {
            dbg!(true)
        }

        assert!(e.exec(0, always_true, &5));
    }
}
