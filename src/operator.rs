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
//! | `len`         | length of an string               | `name len 5`                   |                 |
//! | `contains`    | string contains other string/char | `name contains "Pe"`           | i               |
//! | `starts_with` | string starts with string/char    | `name starts_with "Pe"`        | i               |
//! | `ends_with`   | string ends with string/char      | `name ends_with "er"`          | i               |
//! | `one_of`      | one element from given list       | `x one_of [1, 3, 7]`           | i               |
//! | `regex`       | regexpression (feature = "regex") | `x regex "[0-9]{2}"`           |                 |
//!
//! ### Flags
//!
//! | flag | meaning          | example                             | hint                                                                                                                    |
//! |------|------------------|-------------------------------------|-------------------------------------------------------------------------------------------------------------------------|
//! | `i`  | case insensitive |`=:i "ab"`<br /> (`ab, aB, Ab, AB`)  | is equivalent to a text comparison<br /> (greater and less for numbers **does not work**: 11 is less than 2 ==> "11" < "2") |
//!
//!
use crate::token::Op;
use crate::value::Value;
use crate::{Filterable, FltrError, PathResolver, Predicate, Result};

pub type OperatorFn<PR> = fn(fr: FlagResolver) -> Result<Predicate<PR>>;

pub struct Operator<PR> {
    name: &'static str,
    f: OperatorFn<PR>,
    flags: Vec<char>,
}

impl<PR> Operator<PR> {
    pub fn new(name: &'static str, f: OperatorFn<PR>, flags: &[char]) -> Self {
        Self {
            name,
            f,
            flags: Vec::from(flags),
        }
    }
}

pub struct Operators<PR> {
    pub(crate) ops: Vec<Operator<PR>>,
}

impl<PR: PathResolver> Default for Operators<PR> {
    fn default() -> Self {
        Self {
            ops: vec![
                (Operator::new("==", eq, &['i'])),
                (Operator::new("=", eq, &['i'])),
                (Operator::new("!=", ne, &['i'])),
                (Operator::new("<=", le, &['i'])),
                (Operator::new("<", lt, &['i'])),
                (Operator::new(">=", ge, &['i'])),
                (Operator::new(">", gt, &['i'])),
                (Operator::new("len", len, &[])),
                (Operator::new("contains", contains, &['i'])),
                (Operator::new("starts_with", starts_with, &['i'])),
                (Operator::new("ends_with", ends_with, &['i'])),
                (Operator::new("one_of", one_of, &['i'])),
                #[cfg(feature = "regex")]
                (Operator::new("regex", regex, &[])),
            ],
        }
    }
}

impl<PR: PathResolver> Operators<PR> {
    pub fn get(&self, op: &Op, idx: usize, v: Value) -> Result<Predicate<PR>> {
        if let Some(o) = self.ops.iter().find(|current| current.name == op.name) {
            let create = o.f;
            return create(FlagResolver::new(idx, v, op, &o.flags)?);
        }
        Err(FltrError(format!("invalid operation: '{}'", op)))
    }

    pub fn get_ops_names(&self) -> Vec<&'static str> {
        self.ops.iter().map(|op| op.name).collect()
    }
}

pub struct FlagResolver {
    idx: usize,
    value: Value,
    flag: Option<char>,
}

impl FlagResolver {
    pub fn check_flag(value: Value, op: &Op, supported_flags: &[char]) -> Result<(Value, bool)> {
        if let Some(c) = op.flag {
            // TODO: check the Values (eg: only strings ...)
            if c == 'i' && supported_flags.contains(&c) {
                return Ok(value.to_uppercase());
            } else {
                return Err(FltrError(format!(
                    "the flag: '{c}' is for operator '{}' not supported",
                    op.name
                )));
            }
        }

        Ok((value, false))
    }

    pub fn new(idx: usize, value: Value, op: &Op, supported_flags: &[char]) -> Result<Self> {
        let (value, with_flag) = FlagResolver::check_flag(value, op, supported_flags)?;
        let mut flag = op.flag;
        if !with_flag {
            flag = None;
        }

        Ok(Self { idx, value, flag })
    }

    pub fn handle<PR: PathResolver, Handler: Fn(&dyn Filterable, &Value) -> bool>(
        &self,
        pr: &PR,
        h: Handler,
    ) -> bool {
        if let Some(f) = self.flag {
            if f == 'i' {
                return h(
                    &pr.value(self.idx).as_string().to_ascii_uppercase(),
                    &self.value,
                );
            }
        }

        h(pr.value(self.idx), &self.value)
    }
}

fn eq<PR: PathResolver>(fr: FlagResolver) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| fr.handle(pr, |f, v| f == v)))
}

fn ne<PR: PathResolver>(fr: FlagResolver) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| fr.handle(pr, |f, v| f != v)))
}

fn le<PR: PathResolver>(fr: FlagResolver) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| fr.handle(pr, |f, v| f.le(v))))
}

fn lt<PR: PathResolver>(fr: FlagResolver) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| fr.handle(pr, |f, v| f.lt(v))))
}

fn ge<PR: PathResolver>(fr: FlagResolver) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| fr.handle(pr, |f, v| f.ge(v))))
}

fn gt<PR: PathResolver>(fr: FlagResolver) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| fr.handle(pr, |f, v| f.gt(v))))
}

fn len<PR: PathResolver>(fr: FlagResolver) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| {
        fr.handle(pr, |f, v| match v {
            Value::Int(l) => f.as_string().len() == *l as usize,
            _ => false,
        })
    }))
}

fn contains<PR: PathResolver>(fr: FlagResolver) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| {
        fr.handle(pr, |f, v| match v {
            Value::Text(t) => f.as_string().contains(t),
            Value::Char(c) => f.as_string().contains(*c),
            _ => false,
        })
    }))
}

fn starts_with<PR: PathResolver>(fr: FlagResolver) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| {
        fr.handle(pr, |f, v| match v {
            Value::Text(t) => f.as_string().starts_with(t),
            Value::Char(c) => f.as_string().starts_with(*c),
            _ => false,
        })
    }))
}

fn ends_with<PR: PathResolver>(fr: FlagResolver) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| {
        fr.handle(pr, |f, v| match v {
            Value::Text(t) => f.as_string().ends_with(t),
            Value::Char(c) => f.as_string().ends_with(*c),
            _ => false,
        })
    }))
}

fn one_of<PR: PathResolver>(fr: FlagResolver) -> Result<Predicate<PR>> {
    Ok(Box::new(move |pr| {
        fr.handle(pr, |f, v| {
            if let Value::List(vs) = v {
                return vs.iter().filter(|value| f.eq(value)).count() > 0;
            }
            f.eq(v)
        })
    }))
}

#[cfg(feature = "regex")]
fn regex<PR: PathResolver>(fr: FlagResolver) -> Result<Predicate<PR>> {
    use crate::AsString;

    let rg = regex::Regex::new(&fr.value.as_string()).or_else(|e| Err(FltrError(e.to_string())))?;
    Ok(Box::new(move |pr| {
        fr.handle(pr, |f, _v| rg.is_match(&f.as_string()))
    }))
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
