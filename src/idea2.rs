use crate::{operator::OperatorFn, value::Value, Filterable};

pub type Args<'a> = &'a [&'a dyn Filterable];
pub type PredicateFn<'a> = &'a dyn Fn(Args<'a>) -> bool;

pub fn predicate<'a>(pos: usize, f: OperatorFn, val: Value) -> impl Fn(Args<'a>) -> bool + 'a {
    move |arg: Args<'a>| -> bool { f(arg[pos], &val) }
}

pub fn or<'a>(
    l: impl Fn(Args<'a>) -> bool + 'a,
    r: impl Fn(Args<'a>) -> bool + 'a,
) -> impl Fn(Args<'a>) -> bool + 'a {
    move |arg: Args<'a>| -> bool { (l)(arg) || (r)(arg) }
}

pub fn and<'a>(
    l: impl Fn(Args<'a>) -> bool + 'a,
    r: impl Fn(Args<'a>) -> bool + 'a,
) -> impl Fn(Args<'a>) -> bool + 'a {
    move |arg: Args<'a>| -> bool { (l)(arg) && (r)(arg) }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{operator::Operators, value::Number};

    struct Example {
        name: String,
        x: i32,
    }

    // impl crate::PathResolver for Example {
    //     fn path_to_index(&self, path: &str) -> Option<usize> {
    //         match path {
    //             "x" => Some(1),
    //             _ => Some(0),
    //         }
    //     }

    //     fn value(&self, idx: usize) -> &dyn crate::Filterable {
    //         match idx {
    //             1 => &self.x,
    //             _ => &self.name,
    //         }
    //     }
    // }

    #[test]
    fn first() {
        let e = Example {
            name: String::from("Paul"),
            x: 42,
        };
        let args: Args = &[&e.name, &e.x];

        let op = Operators::default();

        let paul = predicate(0, op.get("!=").unwrap(), Value::Text(String::from("Paul")));
        assert!(!(paul)(args));

        let x7 = predicate(1, op.get("!=").unwrap(), Value::Number(Number::I32(7)));
        assert!((x7)(args));

        let or1 = or(paul, x7);
        assert!((or1)(args));

        let inge = predicate(0, op.get("=").unwrap(), Value::Text(String::from("Inge")));
        assert!(!(inge)(args));

        let or2 = or(inge, or1);
        assert!((or2)(args));

        let x5 = predicate(1, op.get("!=").unwrap(), Value::Number(Number::I32(5)));
        assert!((x5)(args));

        let and = and(x5, or2);
        assert!((and)(args));
    }
}
