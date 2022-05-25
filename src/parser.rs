use crate::operator::Operators;
use crate::scanner::{Result, Scanner};
use crate::token::{Exp, Filter, Predicate};
use crate::value::{str_to_number, Value, Value::*};

use std::{
    ops::{Deref, DerefMut},
    str::FromStr,
};

const KW_AS: &str = "as";
const KW_OR: &str = "or";
const KW_AND: &str = "and";
const KW_NOT: &str = "not";
const KW_NULL_NONE: &str = "none";
const KW_NULL_NULL: &str = "null";

pub type AsValueFn = fn(val: Value) -> Value;

pub(crate) struct Parser<'a> {
    exp: Option<Exp>,
    s: Scanner<'a>,
    pub(crate) ops: Vec<&'static str>,
    pub(crate) as_value_fns: Vec<(&'static str, AsValueFn)>,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Parser {
            exp: None,
            s: Scanner::new(input),
            ops: Operators::<bool>::default().get_ops_names(),
            as_value_fns: vec![],
        }
    }

    pub(crate) fn parse(mut self) -> Result<Exp> {
        let f = iws(&mut self, filter())?;
        self.exp = Some(Exp::new(f));

        loop {
            if self.s.look_str(KW_OR) {
                iws(&mut self, or())?;
            } else if self.s.look_str(KW_AND) {
                iws(&mut self, and())?;
            } else {
                let _ = self.take_while(is_ws);
                if self.is_done() {
                    break;
                }
                return Err(self.parse_err("expected key word 'or' or 'and'"));
            }
        }
        Ok(self.exp.take().unwrap())
    }

    pub(crate) fn get_fn(&self, p: &str) -> Option<&AsValueFn> {
        self.as_value_fns
            .iter()
            .find_map(|(tp, f)| if *tp == p { Some(f) } else { None })
    }

    #[allow(dead_code)]
    pub(crate) fn add_fn(&mut self, name: &'static str, f: AsValueFn) {
        self.as_value_fns.push((name, f));
    }

    pub fn starts_with_valid_op(&self, op: &str) -> Option<String> {
        for s in &self.ops {
            if op.starts_with(s) {
                return Some(s.to_string());
            }
        }
        None
    }
}

impl<'a> Deref for Parser<'a> {
    type Target = Scanner<'a>;

    fn deref(&self) -> &Self::Target {
        &self.s
    }
}

impl<'a> DerefMut for Parser<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.s
    }
}

///////////////////////////////////////////////////////////////////////////////
// Parser implementations
///////////////////////////////////////////////////////////////////////////////

pub fn parse(input: &str) -> Result<Exp> {
    Parser::new(input).parse()
}

pub(crate) fn or() -> impl FnMut(&mut Parser) -> Result<()> {
    |parser: &mut Parser| {
        loop {
            if !parser.take(KW_OR) {
                break;
            }
            let f = iws(parser, filter())?;
            parser.exp.as_mut().unwrap().or(f);
        }
        Ok(())
    }
}

pub(crate) fn and() -> impl FnMut(&mut Parser) -> Result<()> {
    |parser: &mut Parser| {
        loop {
            if !parser.take(KW_AND) {
                break;
            }
            let f = iws(parser, filter())?;
            parser.exp.as_mut().unwrap().and(f);
        }
        Ok(())
    }
}

pub(crate) fn filter() -> impl FnMut(&mut Parser) -> Result<Filter> {
    |parser: &mut Parser| {
        if parser.look_str(KW_NOT) {
            Ok(not()(parser)?)
        } else if parser.look_str("(") {
            Ok(nested()(parser)?)
        } else {
            Ok(Filter::Predicate(predicate()(parser)?))
        }
    }
}

pub(crate) fn not() -> impl FnMut(&mut Parser) -> Result<Filter> {
    |parser: &mut Parser| {
        if parser.take(KW_NOT) {
            parser.take_while(is_ws)?;
            if parser.look_str("(") {
                let input = parser.take_surround(&'(', &')')?;
                Ok(Filter::Not(parse(&input)?))
            } else {
                let p = predicate()(parser)?;
                Ok(Filter::Not(Exp::new(Filter::Predicate(p))))
            }
        } else {
            Err(parser.parse_err("expeted key word 'not'"))
        }
    }
}

pub(crate) fn nested() -> impl FnMut(&mut Parser) -> Result<Filter> {
    |parser: &mut Parser| {
        let input = parser.take_surround(&'(', &')')?;
        Ok(Filter::Nested(parse(&input)?))
    }
}

pub(crate) fn predicate() -> impl FnMut(&mut Parser) -> Result<Predicate> {
    |parser: &mut Parser| {
        let op_str;
        let mut p = None;

        match iws(parser, op()) {
            Ok(s) => op_str = s,
            Err(_) => {
                p = Some(iws(parser, path())?);
                op_str = iws(parser, op())?;
            }
        }

        Ok(Predicate {
            path: p,
            op: op_str,
            value: iws(parser, value_with_as())?,
        })
    }
}

pub(crate) fn op() -> impl FnMut(&mut Parser) -> Result<String> {
    |parser: &mut Parser| {
        let op_str = parser.s.look_while(is_not_ws)?;
        if let Some(op) = parser.starts_with_valid_op(&op_str) {
            parser.s.take(&op);
            return Ok(op);
        }
        Err(parser.parse_err(&format!("'{op_str}' is not a valid filter operation")))
    }
}

pub(crate) fn path() -> impl FnMut(&mut Parser) -> Result<String> {
    |parser: &mut Parser| parser.s.take_while(is_valid_path)
}

pub(crate) fn is_valid_path(pos: usize, c: &char) -> core::result::Result<bool, &str> {
    if c.is_whitespace() {
        Ok(false)
    } else if pos == 0 && !c.is_alphabetic() {
        Err("the first letter must be an alphabeic")
    } else if !(c.is_alphanumeric() || '_'.eq(c) || '.'.eq(c)) {
        Err("the letter must be alphanumeric, '_' or '.'")
    } else {
        Ok(true)
    }
}

pub(crate) fn value_with_as() -> impl FnMut(&mut Parser) -> Result<Value> {
    |parser: &mut Parser| {
        let v = value()(parser)?;
        let as_type = iws(parser, with_as())?;
        if as_type.is_empty() {
            return Ok(v);
        }

        match parser.get_fn(&as_type) {
            Some(f) => Ok((f)(v)),
            None => Err(parser.parse_err(&format!(
                "unknown function: '{}' for converting the value: '{}'",
                as_type, v
            ))),
        }
    }
}

pub(crate) fn value() -> impl FnMut(&mut Parser) -> Result<Value> {
    |parser: &mut Parser| match parser.s.look() {
        Some(c) => match c {
            '"' => text()(parser),
            '\'' => char()(parser),
            '[' => list()(parser),
            '-' | '0'..='9' => number()(parser),
            't' => Ok(Bool(map("true")(parser)?)),
            'f' => Ok(Bool(map("false")(parser)?)),
            'n' => null()(parser),
            _ => Ok(Null),
        },
        None => Ok(Null),
    }
}

#[inline]
pub(crate) fn null() -> impl FnMut(&mut Parser) -> Result<Value> {
    |parser: &mut Parser| {
        if parser.look_str(KW_NULL_NONE) {
            parser.take(KW_NULL_NONE);
            Ok(Null)
        } else if parser.look_str(KW_NULL_NULL) {
            parser.take(KW_NULL_NULL);
            Ok(Null)
        } else {
            Err(parser.parse_err("not a valid 'Null' keyword (none, null)"))
        }
    }
}

#[inline]
pub(crate) fn list() -> impl FnMut(&mut Parser) -> Result<Value> {
    |parser: &mut Parser| {
        parser.take("[");
        let mut values = vec![];
        loop {
            let v = iws(parser, value())?;
            values.push(v);
            if parser.take("]") {
                break;
            }
            if !parser.take(",") {
                return Err(parser.parse_err("expected comma as delimiter in List-Value"));
            }
        }

        Ok(List(values))
    }
}

#[inline]
pub(crate) fn text() -> impl FnMut(&mut Parser) -> Result<Value> {
    |parser: &mut Parser| Ok(Text(parser.take_surround(&'"', &'"')?))
}

#[inline]
pub(crate) fn char() -> impl FnMut(&mut Parser) -> Result<Value> {
    |parser: &mut Parser| {
        let r = parser.take_surround(&'\'', &'\'')?;
        if r.len() != 1 {
            return Err(parser.parse_err(&format!("expected char len is 1 not {}", r.len(),)));
        }
        Ok(Char(r.chars().next().unwrap()))
    }
}

#[inline]
pub(crate) fn number() -> impl FnMut(&mut Parser) -> Result<Value> {
    |parser: &mut Parser| {
        let n_str = parser.take_while(is_number)?;
        str_to_number(&n_str).map_err(|err_str| parser.parse_err(&err_str))
    }
}

#[inline]
pub(crate) fn is_number(pos: usize, c: &char) -> core::result::Result<bool, &str> {
    if c.is_whitespace() {
        Ok(false)
    } else if pos == 0 && !(c.is_numeric() || c.eq(&'-')) {
        Err("expected numeric character or a minus")
    } else if pos != 0 && !(c.is_numeric() || c.eq(&'.')) {
        Ok(false)
    } else {
        Ok(true)
    }
}

#[inline]
pub(crate) fn with_as() -> impl FnMut(&mut Parser) -> Result<String> {
    |parser: &mut Parser| {
        if parser.take(KW_AS) {
            let _ = parser.take_while(is_ws);
            parser.take_while(is_not_ws)
        } else {
            Ok(String::new())
        }
    }
}

/// ignore whitespace: iws
/// __name__ -> name__ -> f() -> name
pub(crate) fn iws<R>(
    parser: &mut Parser,
    mut f: impl FnMut(&mut Parser) -> Result<R>,
) -> Result<R> {
    parser.take_while(is_ws)?;
    let r = f(parser)?;
    parser.take_while(is_ws)?;
    Ok(r)
}

// map convert the given str in an type per FromStr trait
// "true" -> true
pub(crate) fn map<'a, V: 'a>(input: &'a str) -> impl FnMut(&mut Parser) -> Result<V> + 'a
where
    V: FromStr,
{
    move |parser: &mut Parser| {
        if parser.take(input) {
            V::from_str(input)
                .map_err(|_| parser.parse_err(&format!("unexpected type for value: '{}'", input)))
        } else {
            Err(parser.parse_err(&format!("expected input: '{}' not found", input)))
        }
    }
}

#[inline]
pub(crate) fn is_ws(_pos: usize, c: &char) -> core::result::Result<bool, &str> {
    Ok(c.is_whitespace())
}

#[inline]
pub(crate) fn is_not_ws(_pos: usize, c: &char) -> core::result::Result<bool, &str> {
    Ok(!c.is_whitespace())
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::error::{Location, ParseError};
    use test_case::test_case;

    #[test]
    fn starts_with_valid_op() {
        let p = Parser::new("");
        assert_eq!(Some("==".into()), p.starts_with_valid_op("=="));
        assert_eq!(Some("=".into()), p.starts_with_valid_op("="));
        assert_eq!(Some("=".into()), p.starts_with_valid_op("=7"));
        assert_eq!(None, p.starts_with_valid_op("foo"));
    }

    #[test_case("true", "true" => true; "_true")]
    #[test_case("false xyz", "false" => false ; "_false")]
    fn map_bool(input: &str, take_input: &str) -> bool {
        let mut p = Parser::new(input);
        map::<bool>(take_input)(&mut p).unwrap()
    }

    #[test_case(" true", "true", true; "_true")]
    #[test_case("  false", "false", false ; "_false")]
    fn map_bool_iws(input: &str, take_input: &str, expect: bool) {
        let mut p = Parser::new(input);
        assert_eq!(expect, iws(&mut p, map::<bool>(take_input)).unwrap());
    }

    #[test]
    fn map_f32() {
        let mut p = Parser::new("4.0");
        assert_eq!(4.0, map::<f32>("4.0")(&mut p).unwrap());
    }

    #[test]
    fn map_i32() {
        let mut p = Parser::new("42 ");
        assert_eq!(42, map::<i32>("42")(&mut p).unwrap());
    }

    #[test]
    fn map_str() {
        let mut p = Parser::new("Hello world");
        assert_eq!(
            String::from("Hello"),
            map::<String>("Hello")(&mut p).unwrap()
        );
    }

    #[test]
    fn map_char() {
        let mut p = Parser::new("X world");
        assert_eq!('X', map::<char>("X")(&mut p).unwrap());
    }

    #[test]
    fn map_err() {
        let mut p = Parser::new("abc");
        assert_eq!(
            ParseError {
                input: "abc".into(),
                location: Location { line: 1, column: 1 },
                err_msg: "expected input: 'true' not found".into()
            },
            map::<bool>("true")(&mut p).err().unwrap()
        );

        assert_eq!(
            "'abc' error at ln 1, col 1: expected input: 'true' not found",
            map::<bool>("true")(&mut p).err().unwrap().to_string()
        );
    }

    #[test_case("as i32", "i32")]
    #[test_case("as f64", "f64")]
    #[test_case("as usize", "usize")]
    #[test_case("as foo", "foo")]
    #[test_case("AS x", "x")]
    #[test_case("As y", "y")]
    #[test_case("aS z", "z")]
    #[test_case("foo", "")]
    #[test_case("as ", "")]
    #[test_case(" ", "" ; "space")]
    #[test_case("", "" ; "empty")]
    fn with_as_check(input: &str, expect: &str) {
        let mut p = Parser::new(input);
        assert_eq!(expect, with_as()(&mut p).unwrap());
    }

    #[test_case("0" => Int(0) ; "0")]
    #[test_case("01" => Int(1) ; "01")]
    #[test_case("002" => Int(2) ; "002")]
    #[test_case("0.1" => Float(0.1) ; "0.1")]
    #[test_case("1.34" => Float(1.34) ; "1.34")]
    #[test_case("-1.34" => Float(-1.34) ; "minus 1.34")]
    #[test_case("240" => Int(240) ; "240")]
    #[test_case("-240" => Int(-240); "minus 240")]
    #[test_case("-1.34 as f32" => Float(-1.34) ; "minus 1.34 as f32")]
    #[test_case("240 as u8" => Int(240) ; "240 as u8")]
    #[test_case("1a34" => Int(1) ; "1a34")]
    fn number_check(input: &str) -> Value {
        let mut p = Parser::new(input);
        number()(&mut p).unwrap()
    }

    #[test_case("a1.34", ParseError {input: "a1.34".into(),location: Location { line: 1, column: 1 },err_msg: "expected numeric character or a minus".into()} ; "a1.34")]
    #[test_case("2400000000", ParseError {input: "2400000000".into(),location: Location { line: 1, column: 10},err_msg: "number too large to fit in target type".into()} ; "240 as i32")]
    fn number_err(input: &str, err: ParseError) {
        let mut p = Parser::new(input);
        assert_eq!(err, number()(&mut p).err().unwrap());
    }

    #[test_case(r#""yeh""#, |v: Value| -> Value { str_to_number(&v.to_string()).unwrap() }=>Value::Text("yeh".into()) ; "no as value")]
    #[test_case(r#""123" as to_val"#, |v: Value| -> Value { str_to_number(&v.to_string()).unwrap() } => Value::Int(123) ; "text as Int32")]
    #[test_case(r#"123 AS to_val"#, |v: Value| -> Value { if let Value::Int(i) = v { Value::Int(i * 2) } else { Value::Int(0) } } => Value::Int(246) ; "double 123")]
    fn value_with_as_check(input: &str, as_val_fn: AsValueFn) -> Value {
        let mut p = Parser::new(input);
        p.add_fn("to_val", as_val_fn);
        value_with_as()(&mut p).unwrap()
    }

    #[test_case(r#""123" as u32"#, ParseError {input: r#""123" as u32"#.into(),location: Location { line: 1, column: 12},err_msg: "unknown function: 'u32' for converting the value: '123'".into()} ; "240 as i8")]
    fn value_with_as_err(input: &str, err: ParseError) {
        let mut p = Parser::new(input);
        assert_eq!(err, value_with_as()(&mut p).err().unwrap());
    }

    #[test]
    fn char_check() {
        let mut p = Parser::new("'Y'");
        assert_eq!(Char('Y'), char()(&mut p).unwrap());
    }

    #[test]
    fn char_err() {
        let mut p = Parser::new("'abc'");
        assert_eq!(
            ParseError {
                input: r#"'abc'"#.into(),
                location: Location { line: 1, column: 5 },
                err_msg: "expected char len is 1 not 3".into()
            },
            char()(&mut p).err().unwrap()
        );
    }

    #[test_case(" ", Null ; "space_NULL")]
    #[test_case("null", Null ; "null")]
    #[test_case("none", Null ; "none")]
    #[test_case("NULL", Null ; "upper null")]
    #[test_case("NONE", Null ; "upper NONE")]
    #[test_case("blub", Null ; "blub_NULL")]
    #[test_case("240", Int(240) ; "240")]
    #[test_case("true", Bool(true) ; "true_val")]
    #[test_case("false", Bool(false) ; "false_val")]
    #[test_case(r#""false""#, Text("false".into()) ; "false_string_val")]
    #[test_case(r#"'X'"#, Char('X'.into()) ; "X_char_val")]
    fn value_check(input: &str, expect: Value) {
        let mut p = Parser::new(input);
        assert_eq!(expect, value()(&mut p).unwrap());
    }

    #[test_case("foo", ParseError {input: "foo".into(),location: Location { line: 1, column: 1},err_msg: "expected input: 'false' not found".into()} ; "foo not false")]
    #[test_case(r#""bar"#, ParseError {input: r#""bar"#.into(),location: Location { line: 1, column: 4},err_msg: "missing closing character: '\"'".into()} ; "string not closing bracket")]
    #[test_case(r#"'bar'"#, ParseError {input: r#"'bar'"#.into(),location: Location { line: 1, column: 5},err_msg: "expected char len is 1 not 3".into()} ; "char to long")]
    fn value_err(input: &str, err: ParseError) {
        let mut p = Parser::new(input);
        assert_eq!(err, value()(&mut p).err().unwrap());
    }

    #[test_case(r#"[1]"# => List(vec![Int(1)]) ; "list_1")]
    #[test_case(r#"[ 1]"# => List(vec![Int(1)]) ; "list_space_1")]
    #[test_case(r#"[1 ]"# => List(vec![Int(1)]) ; "list_1_space")]
    #[test_case(r#"[ 1, 2]"# => List(vec![Int(1), Int(2)]) ; "list_1_2")]
    #[test_case(r#"[1, 2, 5]"# => List(vec![Int(1), Int(2), Int(5)]) ; "list_1_2_5")]
    #[test_case(r#"[1, 'A', true]"# => List(vec![Int(1), Char('A'), Bool(true)]) ; "list_1_A_true")]
    #[test_case(r#"["aA", "Bb"]"# => List(vec![Text("aA".into()), Text("Bb".into())]) ; "list_aA_Bb")]
    #[test_case(r#"[ ]"# => List(vec![Null]) ; "list_empty_NULL")]
    #[test_case(r#"[ , ]"# => List(vec![Null, Null]) ; "list_empty_NULL_NULL")]
    #[test_case(r#"[ 1, ]"# => List(vec![Int(1), Null]) ; "list_1_empty_NULL")]
    #[test_case(r#"[ , 1 ]"# => List(vec![Null, Int(1)]) ; "list_empty_NULL_1")]
    fn list_value_check(input: &str) -> Value {
        let mut p = Parser::new(input);
        value()(&mut p).unwrap()
    }

    #[test_case(r#"[ "# => ParseError {input: "[ ".into(),location: Location { line: 1, column: 2},err_msg: "expected comma as delimiter in List-Value".into()} ; "not closing bracket")]
    #[test_case(r#"[ 1 "# => ParseError {input: "[ 1 ".into(),location: Location { line: 1, column: 4},err_msg: "expected comma as delimiter in List-Value".into()} ; "not closing bracket with value")]
    #[test_case(r#"[ 1, "# => ParseError {input: "[ 1, ".into(),location: Location { line: 1, column: 5},err_msg: "expected comma as delimiter in List-Value".into()} ; "not closing bracket with value and comma")]
    fn list_value_err(input: &str) -> ParseError {
        let mut p = Parser::new(input);
        value()(&mut p).err().unwrap()
    }

    #[test_case("name", "name")]
    #[test_case("name1", "name1")]
    #[test_case("na_me", "na_me")]
    #[test_case("cars.name", "cars.name")]
    fn path_check(input: &str, expect: &str) {
        let mut p = Parser::new(input);
        assert_eq!(expect, path()(&mut p).unwrap());
    }

    #[test_case("1name", ParseError {input: "1name".into(),location: Location { line: 1, column: 1},err_msg: "the first letter must be an alphabeic".into()} ; "first letter")]
    #[test_case("na/me", ParseError {input: "na/me".into(),location: Location { line: 1, column: 3},err_msg: "the letter must be alphanumeric, '_' or '.'".into()} ; "slash in name")]
    fn path_err(input: &str, err: ParseError) {
        let mut p = Parser::new(input);
        assert_eq!(err, path()(&mut p).err().unwrap());
    }

    #[test_case("=", "="; "eq")]
    #[test_case(">=", ">="; "ge")]
    #[test_case("len", "len")]
    #[test_case("starts_with", "starts_with")]
    fn op_check(input: &str, expect: &str) {
        let mut p = Parser::new(input);
        assert_eq!(expect, op()(&mut p).unwrap());
    }

    #[test_case("foo", ParseError {input: "foo".into(),location: Location { line: 1, column: 1},err_msg: "'foo' is not a valid filter operation".into()} ; "foo")]
    fn op_err(input: &str, err: ParseError) {
        let mut p = Parser::new(input);
        assert_eq!(err, op()(&mut p).err().unwrap());
    }

    #[test_case("=7", Predicate{path: None, op: String::from("="), value: Int(7)}; "eq7")]
    #[test_case("= 7", Predicate{path: None, op: String::from("="), value: Int(7)}; "eq 7")]
    #[test_case("name len 3", Predicate{path: Some(String::from("name")), op: String::from("len"), value: Int(3)}; "name len 3")]
    #[test_case("name len3", Predicate{path: Some(String::from("name")), op: String::from("len"), value: Int(3)}; "name len3")]
    #[test_case("is_empty", Predicate{path: None, op: String::from("is_empty"), value: Null}; "is_empty")]
    #[test_case("name is_empty", Predicate{path: Some(String::from("name")), op: String::from("is_empty"), value: Null}; "name is_empty")]
    #[test_case("=", Predicate{path: None, op: String::from("="), value: Null}; "eq empty")]
    #[test_case("= ", Predicate{path: None, op: String::from("="), value: Null}; "eq empty space")]
    fn predicate_check(input: &str, expect: Predicate) {
        let mut p = Parser::new(input);
        assert_eq!(expect, predicate()(&mut p).unwrap());
    }

    #[test_case("age 3", ParseError {input: "age 3".into(),location: Location { line: 1, column: 4},err_msg: "'3' is not a valid filter operation".into()} ; "age 3")]
    #[test_case(r#"name = "Paul "#, ParseError {input: r#"name = "Paul "#.into(),location: Location { line: 1, column: 13},err_msg: "missing closing character: '\"'".into()} ; r#"name = "Paul "#)]
    fn predicate_err(input: &str, err: ParseError) {
        let mut p = Parser::new(input);
        assert_eq!(err, predicate()(&mut p).err().unwrap());
    }

    #[test_case("= 7", Filter::Predicate(Predicate{path: None, op: String::from("="), value: Int(7)}); "eq 7")]
    #[test_case("name len 3", Filter::Predicate(Predicate{path: Some(String::from("name")), op: String::from("len"), value: Int(3)}); "name len 3")]
    fn filter_check(input: &str, expect: Filter) {
        let mut p = Parser::new(input);
        assert_eq!(expect, filter()(&mut p).unwrap());
    }

    #[test_case("not (is_empty)", "not (is_empty NULL)")]
    #[test_case("=", "= NULL")]
    #[test_case("= true or = false", "= true or = false")]
    #[test_case("= true and != false", "= true and != false")]
    #[test_case("name = 'X' and name != 'Y'", "name = X and name != Y")]
    #[test_case("= 'W'  and !=  'Z'", "= W and != Z")]
    #[test_case("= 'X' or = 'y'  and !=  'Z'", "= X or = y and != Z")]
    #[test_case("= 'A' and = 'B'  or !=  'C'", "= A and = B or != C")]
    #[test_case("= 1 and = 2 and = 3  or !=  4", "= 1 and = 2 and = 3 or != 4")]
    #[test_case("= 1  or =   2 or =   3  and !=  4", "= 1 or = 2 or = 3 and != 4")]
    fn parse_check(input: &str, display: &str) {
        let exp = parse(input).unwrap();
        assert_eq!(display, format!("{}", exp));
    }

    #[test_case("not = 5", "not (= 5)")]
    #[test_case("not (= true)", "not (= true)")]
    #[test_case("NOt (= false)", "not (= false)")]
    #[test_case("not(= true and != false)", "not (= true and != false)")]
    #[test_case("noT(name = 'X' and name != 'Y')", "not (name = X and name != Y)")]
    #[test_case(
        "= false and not(= true or != false)",
        "= false and not (= true or != false)"
    )]
    #[test_case(
        "not(= true or != false) and = false",
        "not (= true or != false) and = false"
    )]
    fn parse_not_check(input: &str, display: &str) {
        let exp = parse(input).unwrap();
        assert_eq!(display, format!("{}", exp));
    }

    #[test_case(
        "= false and not(= true",
        ParseError {input: "= false and not(= true".into(),location: Location { line: 1, column: 22},err_msg: "missing closing character: ')'".into()} ;
        "missing closing bracket"
    )]
    #[test_case(
        "not",
        ParseError {input: "not".into(),location: Location { line: 1, column: 3},err_msg: "'' is not a valid filter operation".into()} ;
        "only not"
    )]
    #[test_case(
        "not name",
        ParseError {input: "not name".into(),location: Location { line: 1, column: 8},err_msg: "'' is not a valid filter operation".into()} ;
        "not name"
    )]
    #[test_case(
        "not name op",
        ParseError {input: "not name op".into(),location: Location { line: 1, column: 9},err_msg: "'op' is not a valid filter operation".into()} ;
        "not name op"
    )]
    fn parse_not_err(input: &str, err: ParseError) {
        assert_eq!(err, parse(input).err().unwrap());
    }

    #[test_case("(= true)", "(= true)")]
    #[test_case("(= true and != false)", "(= true and != false)")]
    #[test_case("(name = 'X' and name != 'Y')", "(name = X and name != Y)")]
    #[test_case("= false and (= true or != false)", "= false and (= true or != false)")]
    #[test_case("(= true or != false) and = false", "(= true or != false) and = false")]
    fn parse_nested_check(input: &str, display: &str) {
        let exp = parse(input).unwrap();
        assert_eq!(display, format!("{}", exp));
    }

    #[test_case(
        "= 1 and (not(= 5) or = 6",
        ParseError {input: "= 1 and (not(= 5) or = 6".into(),location: Location { line: 1, column: 24},err_msg: "missing closing character: ')'".into()} ;
        "missing last closing bracket"
    )]
    #[test_case(
        "= 1 and (not(= 5 or = 6)",
        ParseError {input: "= 1 and (not(= 5 or = 6)".into(),location: Location { line: 1, column: 24},err_msg: "missing closing character: ')'".into()} ;
        "missing closing bracket"
    )]
    fn parse_nested_err(input: &str, err: ParseError) {
        assert_eq!(err, parse(input).err().unwrap());
    }

    #[test_case("= 1 and (not (= 5) or = 6)", "= 1 and (not (= 5) or = 6)")]
    #[test_case(
        "= 0 or not (= 1 and (= 5 or = 6))",
        "= 0 or not (= 1 and (= 5 or = 6))"
    )]
    fn parse_nested_and_not_check(input: &str, display: &str) {
        let exp = parse(input).unwrap();
        assert_eq!(display, format!("{}", exp));
    }

    #[test_case("= true o", ParseError {input: "= true o".into(),location: Location { line: 1, column: 7},err_msg: "expected key word 'or' or 'and'".into()}; "eq true o")]
    #[test_case("= tru", ParseError {input: "= tru".into(),location: Location { line: 1, column: 2},err_msg: "expected input: 'true' not found".into()} ; "eq tru")]
    fn parse_err(input: &str, err: ParseError) {
        assert_eq!(err, parse(input).err().unwrap());
    }
}
