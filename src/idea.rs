use crate::value::{Number, Value};
use std::fmt::Display;

pub type PredicateFn<V, A> = fn(val: &V, arg: &A) -> bool;

trait Predicate<V, A> {
    fn exec(&self, pos: usize, f: PredicateFn<V, A>, arg: &A) -> bool;
}

struct Example {
    name: String,
    x: i32,
}

impl<A> Predicate<i32, A> for Example
// where
// A: PartialEq<i32> + PartialOrd<i32> + Display,
// A: ToValue,
{
    fn exec(&self, _pos: usize, f: PredicateFn<i32, A>, arg: &A) -> bool {
        f(&self.x, arg)
    }
}

impl<A> Predicate<String, A> for Example
// where
// A: PartialEq<String> + PartialOrd<String> + Display,
// A: ToValue,
{
    fn exec(&self, _pos: usize, f: PredicateFn<String, A>, arg: &A) -> bool {
        f(&self.name, arg)
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
    v.to_string().starts_with(&a.to_string())
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

        let ops = Operators::<String, _>::default();

        assert!(e.exec(0, ops.get("=").unwrap(), &String::from("Paul")));
        // assert!(e.exec(0, ops.get("!=").unwrap(), &String::from("Peter")));
        // assert!(e.exec(0, ops.get("starts_with").unwrap(), &String::from("Pa")));
        // // assert!(e.exec(0, ops.get("len").unwrap(), &4));

        // assert!(e.exec(0, PartialOrd::<i32>::lt, &50));
        let ops = Operators::<i32, _>::default();
        assert!(e.exec(0, ops.get("<").unwrap(), &50));

        // fn always_true<V, A>(_v: &V, _a: &A) -> bool {
        //     dbg!(true)
        // }

        // assert!(e.exec(0, always_true, &5));
    }
}
