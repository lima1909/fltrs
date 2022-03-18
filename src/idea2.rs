use crate::{operator::OperatorFn, value::Value, PathResolver};

pub type PredicateFn<'a> = &'a dyn Fn(&dyn PathResolver) -> bool;

pub fn predicate(pos: usize, f: OperatorFn, val: Value) -> impl Fn(&dyn PathResolver) -> bool {
    move |arg: &dyn PathResolver| -> bool { f(arg.value(pos), &val) }
}

pub fn or<'a>(l: PredicateFn<'a>, r: PredicateFn<'a>) -> impl Fn(&dyn PathResolver) -> bool + 'a {
    move |arg: &dyn PathResolver| -> bool { (l)(arg) || (r)(arg) }
}

pub fn and<'a>(l: PredicateFn<'a>, r: PredicateFn<'a>) -> impl Fn(&dyn PathResolver) -> bool + 'a {
    move |arg: &dyn PathResolver| -> bool { (l)(arg) && (r)(arg) }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{operator::Operators, value::Number, Filterable};

    struct Example {
        name: String,
        x: i32,
    }

    impl PathResolver for Example {
        fn path_to_index(&self, path: &str) -> Option<usize> {
            match path {
                "x" => Some(1),
                _ => Some(0),
            }
        }

        fn value(&self, idx: usize) -> &dyn Filterable {
            match idx {
                1 => &self.x,
                _ => &self.name,
            }
        }
    }

    #[test]
    fn first() {
        let e = Example {
            name: String::from("Paul"),
            x: 42,
        };

        let op = Operators::default();

        let name = predicate(0, op.get("!=").unwrap(), Value::Text(String::from("Paul")));
        assert!(!(name)(&e));

        let x = predicate(1, op.get("!=").unwrap(), Value::Number(Number::I32(7)));
        assert!((x)(&e));

        let or1 = or(&name, &x);
        assert!((or1)(&e));

        let or2 = or(&name, &or1);
        assert!((or2)(&e));

        let and = and(&or1, &or2);
        assert!((and)(&e));
    }
}
