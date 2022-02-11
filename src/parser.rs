#![allow(dead_code)] // TODO: remove this

// use crate::operator::Operators;
use crate::scanner::{Result, Scanner};

use std::{
    ops::{Deref, DerefMut},
    str::FromStr,
};

pub(crate) struct Parser<'a> {
    s: Scanner<'a>,
    // ops: Operators<>,
    // ors: Option<Ors>,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Parser {
            s: Scanner::new(input),
            // ops: vec!["=", "!=", "<", ">"],
            // ors: None,
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

/// ignore whitespace: iws
/// __name__ -> name__ -> f() -> name
pub(crate) fn iws<R>(
    parser: &mut Parser,
    mut f: impl FnMut(&mut Parser) -> Result<R>,
) -> Result<R> {
    parser.take_while_ws()?;
    let r = f(parser)?;
    parser.take_while_ws()?;
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
            Err(parser.parse_err(&format!("input: '{}' not found", input)))
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::error::{Location, ParseError};
    use test_case::test_case;

    #[test_case("true", "true", true; "_true")]
    #[test_case("false xyz", "false", false ; "_false")]
    fn map_check(input: &str, take_input: &str, expect: bool) {
        let mut p = Parser::new(input);
        assert_eq!(expect, map::<bool>(take_input)(&mut p).unwrap());
    }

    #[test_case(" true", "true", true; "_true")]
    #[test_case("  false", "false", false ; "_false")]
    fn map_iws_check(input: &str, take_input: &str, expect: bool) {
        let mut p = Parser::new(input);
        assert_eq!(expect, iws(&mut p, map::<bool>(take_input)).unwrap());
    }

    #[test]
    fn map_err() {
        let mut p = Parser::new("abc");
        assert_eq!(
            ParseError {
                input: "abc".into(),
                location: Location { line: 1, column: 1 },
                err_msg: "input: 'true' not found".into()
            },
            map::<bool>("true")(&mut p).err().unwrap()
        );

        assert_eq!(
            "'abc' error at ln 1, col 1: input: 'true' not found",
            map::<bool>("true")(&mut p).err().unwrap().to_string()
        );
    }
}
