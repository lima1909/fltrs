#![allow(dead_code)] // TODO: remove this

use crate::operator::Operators;
use crate::scanner::{Result, Scanner};
use crate::token::{Exp, Filter, Predicate};
use crate::value::{CopyValue::*, Number, Value, Value::*};

use std::{
    ops::{Deref, DerefMut},
    str::FromStr,
};

pub(crate) struct Parser<'a> {
    s: Scanner<'a>,
    ops: Operators<bool>,
    exp: Option<Exp>,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Parser {
            s: Scanner::new(input),
            ops: Operators::default(),
            exp: None,
        }
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

#[allow(unused_must_use)]
pub(crate) fn parse(input: &str) -> Result<Exp> {
    let mut parser = Parser::new(input);
    let f = iws(&mut parser, filter())?;
    parser.exp = Some(Exp::new(f));

    loop {
        if parser.s.look_str("or") {
            iws(&mut parser, or())?;
        } else if parser.s.look_str("and") {
            iws(&mut parser, and())?;
        } else {
            parser.take_while(is_ws);
            if parser.is_done() {
                break;
            }
            return Err(parser.parse_err("expected 'or' or 'and', got"));
        }
    }
    Ok(parser.exp.take().unwrap())
}

pub(crate) fn or() -> impl FnMut(&mut Parser) -> Result<()> {
    |parser: &mut Parser| {
        loop {
            if !parser.take("or") {
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
            if !parser.take("and") {
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
        // if parser.look_str("not(") {
        //     Ok(not()(parser)?)
        // } else if parser.look_str("(") {
        //     Ok(nested()(parser)?)
        // } else {
        Ok(Filter::Predicate(predicate()(parser)?))
        // }
    }
}

// pub(crate) fn not() -> impl FnMut(&mut Parser) -> Result<Filter> {
//     |parser: &mut Parser| {
//         if parser.take("not") {
//             let input = parser.take_surround(&'(', &')')?;
//             Ok(Filter::Not(parse(&input)?))
//         } else {
//             Err(parser.parse_err("expeted key word 'not'"))
//         }
//     }
// }

// pub(crate) fn nested() -> impl FnMut(&mut Parser) -> Result<Filter> {
//     |parser: &mut Parser| {
//         let input = parser.take_surround(&'(', &')')?;
//         Ok(Filter::Nested(parse(&input)?))
//     }
// }

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
            value: iws(parser, value())?,
        })
    }
}

pub(crate) fn op() -> impl FnMut(&mut Parser) -> Result<String> {
    |parser: &mut Parser| {
        let op_str = parser.look_while(is_not_ws)?;
        if parser.ops.is_valid(&op_str) {
            parser.take(&op_str);
            return Ok(op_str);
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

pub(crate) fn value() -> impl FnMut(&mut Parser) -> Result<Value> {
    |parser: &mut Parser| match parser.s.look() {
        Some(c) => match c {
            '"' => Ok(Text(parser.take_surround(&'"', &'"')?)),
            '\'' => {
                let r = parser.take_surround(&'\'', &'\'')?;
                if r.len() != 1 {
                    return Err(
                        parser.parse_err(&format!("expected char len is 1 not {}", r.len(),))
                    );
                }
                Ok(CopyValue(Char(r.chars().next().unwrap())))
            }
            't' => Ok(CopyValue(Bool(map("true")(parser)?))),
            'f' => Ok(CopyValue(Bool(map("false")(parser)?))),
            '-' | '0'..='9' => Ok(CopyValue(Number(number()(parser)?))),
            _ => Err(parser.parse_err(&format!("unexpected char '{}' for a valid Value", c))),
        },
        None => Err(parser.parse_err("unexpected end")),
    }
}

#[inline]
pub(crate) fn number() -> impl FnMut(&mut Parser) -> Result<Number> {
    |parser: &mut Parser| {
        let n_str = parser.take_while(is_number)?;
        let as_type = iws(parser, with_as())?;
        if as_type.is_empty() {
            return Number::default(&n_str).map_err(|err_str| parser.parse_err(&err_str));
        }

        Number::try_from_as(&as_type, &n_str)
            .map_err(|err_str| parser.parse_err(&format!("'{as_type}' {err_str}")))
    }
}

#[inline]
pub(crate) fn is_number(pos: usize, c: &char) -> core::result::Result<bool, &str> {
    if c.is_whitespace() {
        Ok(false)
    } else if pos == 0 && !(c.is_numeric() || c.eq(&'-')) {
        Err("expected numeric character or a minus")
    } else if pos != 0 && !(c.is_numeric() || c.eq(&'.')) {
        Err("expected numeric character or a dot")
    } else {
        Ok(true)
    }
}

#[inline]
pub(crate) fn with_as() -> impl FnMut(&mut Parser) -> Result<String> {
    |parser: &mut Parser| {
        if parser.take("as") {
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

    #[test_case("true", "true", true; "_true")]
    #[test_case("false xyz", "false", false ; "_false")]
    fn map_bool(input: &str, take_input: &str, expect: bool) {
        let mut p = Parser::new(input);
        assert_eq!(expect, map::<bool>(take_input)(&mut p).unwrap());
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
    #[test_case("foo", "")]
    #[test_case("as ", "")]
    #[test_case(" ", "" ; "space")]
    #[test_case("", "" ; "empty")]
    fn with_as_check(input: &str, expect: &str) {
        let mut p = Parser::new(input);
        assert_eq!(expect, with_as()(&mut p).unwrap());
    }

    #[test_case("0", Number::I32(0) ; "0")]
    #[test_case("01", Number::I32(1) ; "01")]
    #[test_case("002", Number::I32(2) ; "002")]
    #[test_case("0.1", Number::F64(0.1) ; "0.1")]
    #[test_case("1.34", Number::F64(1.34) ; "1.34")]
    #[test_case("-1.34", Number::F64(-1.34) ; "minus 1.34")]
    #[test_case("240", Number::I32(240) ; "240")]
    #[test_case("-240", Number::I32(-240); "minus 240")]
    #[test_case("-1.34 as f32", Number::F32(-1.34) ; "minus 1.34 as f32")]
    #[test_case("240 as u8", Number::U8(240) ; "240 as u8")]
    fn number_check(input: &str, expect: Number) {
        let mut p = Parser::new(input);
        assert_eq!(expect, number()(&mut p).unwrap());
    }

    #[test_case("a1.34", ParseError {input: "a1.34".into(),location: Location { line: 1, column: 1 },err_msg: "expected numeric character or a minus".into()} ; "a1.34")]
    #[test_case("1a34", ParseError {input: "1a34".into(),location: Location { line: 1, column: 2 },err_msg: "expected numeric character or a dot".into()} ; "1a34")]
    #[test_case("1.34 as foo", ParseError {input: "1.34 as foo".into(),location: Location { line: 1, column: 11 },err_msg: "'foo' is not an valid 'as type' for a number".into()} ; "1.34 as foo")]
    #[test_case("240 as i8", ParseError {input: "240 as i8".into(),location: Location { line: 1, column: 9},err_msg: "'i8' number too large to fit in target type".into()} ; "240 as i8")]
    fn number_err(input: &str, err: ParseError) {
        let mut p = Parser::new(input);
        assert_eq!(err, number()(&mut p).err().unwrap());
    }

    #[test_case("240 as u8", CopyValue(Number(Number::U8(240))) ; "240 as u8")]
    #[test_case("true", CopyValue(Bool(true)) ; "true_val")]
    #[test_case("false", CopyValue(Bool(false)) ; "false_val")]
    #[test_case(r#""false""#, Text("false".into()) ; "false_string_val")]
    #[test_case(r#"'X'"#, CopyValue(Char('X'.into())) ; "X_char_val")]
    fn value_check(input: &str, expect: Value) {
        let mut p = Parser::new(input);
        assert_eq!(expect, value()(&mut p).unwrap());
    }

    #[test_case("foo", ParseError {input: "foo".into(),location: Location { line: 1, column: 1},err_msg: "expected input: 'false' not found".into()} ; "foo not false")]
    #[test_case("bar", ParseError {input: "bar".into(),location: Location { line: 1, column: 1},err_msg: "unexpected char 'b' for a valid Value".into()} ; "bar not bool")]
    #[test_case(r#""bar"#, ParseError {input: r#""bar"#.into(),location: Location { line: 1, column: 4},err_msg: "missing closing character: '\"'".into()} ; "string not closing bracket")]
    #[test_case(r#"'bar'"#, ParseError {input: r#"'bar'"#.into(),location: Location { line: 1, column: 5},err_msg: "expected char len is 1 not 3".into()} ; "char to long")]
    fn value_err(input: &str, err: ParseError) {
        let mut p = Parser::new(input);
        assert_eq!(err, value()(&mut p).err().unwrap());
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

    #[test_case("= 7", Predicate{path: None, op: String::from("="), value: CopyValue(Number(Number::I32(7)))}; "eq 7")]
    #[test_case("name len 3", Predicate{path: Some(String::from("name")), op: String::from("len"), value: CopyValue(Number(Number::I32(3)))}; "name len 3")]
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

    #[test_case("= 7", Filter::Predicate(Predicate{path: None, op: String::from("="), value: CopyValue(Number(Number::I32(7)))}); "eq 7")]
    #[test_case("name len 3", Filter::Predicate(Predicate{path: Some(String::from("name")), op: String::from("len"), value: CopyValue(Number(Number::I32(3)))}); "name len 3")]
    fn filter_check(input: &str, expect: Filter) {
        let mut p = Parser::new(input);
        assert_eq!(expect, filter()(&mut p).unwrap());
    }

    #[test_case("= true or = false", "= true or = false")]
    #[test_case("= true and != false", "= true and != false")]
    #[test_case("name = 'X' and name != 'Y'", "name = X and name != Y")]
    #[test_case("= 'W'  and !=  'Z'", "= W and != Z")]
    #[test_case("= 'X' or = 'y'  and !=  'Z'", "= X or = y and != Z")]
    #[test_case("= 'A' and = 'B'  or !=  'C'", "= A and = B or != C")]
    #[test_case("= 1 and = 2 and = 3  or !=  4", "= 1 and = 2 and = 3 or != 4")]
    #[test_case("= 1  or =   2 or =   3  and !=  4", "= 1 or = 2 or = 3 and != 4")]
    fn parse_check(input: &str, display: &str) {
        assert_eq!(display, format!("{}", parse(input).unwrap()));
    }
}
