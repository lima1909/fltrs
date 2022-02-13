#![allow(dead_code)] // TODO: remove this

use crate::error::{Location, ParseError};

use std::ops::ControlFlow;

/// This is the default Result for the Scanner and the Parser.
/// The return value can be an `Ok(T)` or an error `Err(ParseError)`.
pub type Result<T> = core::result::Result<T, ParseError>;

pub(crate) struct Scanner<'a> {
    input: &'a str,
    ptr: usize,
    inner: Vec<char>,
}

impl<'a> Scanner<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Self {
            input,
            ptr: 0,
            inner: input.chars().collect(),
        }
    }

    /// read the current char, without changing the pointer
    pub(crate) fn look(&self) -> Option<&char> {
        self.inner.get(self.ptr)
    }

    /// if the input is find, then get true (wihtout inc the pointer)
    pub(crate) fn look_str(&mut self, input: &str) -> bool {
        self.it().take(input.len()).copied().eq(input.chars())
    }

    /// look all chars for the given function: `f` gets true (wihtout inc the pointer)
    /// name__x -> name, (rest: __x)
    pub(crate) fn look_while(
        &mut self,
        f: fn(usize, &char) -> core::result::Result<bool, &str>,
    ) -> Result<String> {
        let mut count = 0;

        let cf = self.it().enumerate().try_for_each(|(p, c)| match f(p, c) {
            Ok(result) => {
                if !result {
                    return ControlFlow::Break(None);
                }
                count += 1;
                ControlFlow::Continue(())
            }
            Err(msg) => {
                count += 1;
                ControlFlow::Break(Some(String::from(msg)))
            }
        });

        match cf {
            ControlFlow::Break(msg) if msg.is_some() => {
                self.ptr += count;
                Err(self.parse_err(&msg.unwrap()))
            }
            _ => Ok(String::from_iter(&self.inner[self.ptr..(self.ptr + count)])),
        }
    }

    /// get an iterator from the current position
    pub(crate) fn it(&mut self) -> impl Iterator<Item = &char> {
        self.inner.iter().skip(self.ptr)
    }

    /// if the input is find, then take the len of the input (inc pointer += input.len())
    pub(crate) fn take(&mut self, input: &str) -> bool {
        if self.look_str(input) {
            self.ptr += input.len();
            return true;
        }
        false
    }

    /// take all chars for the given function: `f` gets true (inc the pointer)
    /// name__x -> name, (rest: __x)
    pub(crate) fn take_while(
        &mut self,
        f: fn(usize, &char) -> core::result::Result<bool, &str>,
    ) -> Result<String> {
        let s = self.look_while(f)?;
        self.ptr += s.len();
        Ok(s)
    }

    /// take a string between a char like quote: "abx" -> abx
    pub(crate) fn take_surround(&mut self, begin: &char, end: &char) -> Result<String> {
        match self.look() {
            Some(got) if got == begin => {
                self.ptr += 1; // bypass the begin char

                let mut count = 0;
                let mut found = false;
                let mut result = String::new();
                let mut also_begin = 0; // counter (stack) for beginning char (like '('.. '(')

                for current in self.it() {
                    if begin != end && current == begin {
                        also_begin += 1;
                    }

                    if current == end {
                        if also_begin == 0 {
                            found = true;
                            break;
                        } else if begin != end {
                            also_begin -= 1;
                        }
                    }

                    count += 1;
                    result.push(*current);
                }

                self.ptr += count;
                if !found {
                    return Err(self.parse_err(&format!("missing closing character: '{}'", end)));
                }

                if !self.is_done() {
                    self.ptr += 1; // add closing quote
                }
                Ok(result)
            }
            _ => Err(self.parse_err(&format!("expected character: '{}' not found", begin))),
        }
    }

    pub(crate) fn is_done(&self) -> bool {
        self.ptr == self.inner.len()
    }

    pub(crate) fn location(&self) -> Location {
        let mut line = 1;
        let mut column = self.ptr;

        for (l, col) in self.input[..self.ptr].lines().skip(1).enumerate() {
            column = col.chars().count();
            line = 2 + l;
        }

        // is there a better solution???
        if column == 0 {
            column = 1;
        }

        Location { line, column }
    }

    pub(crate) fn parse_err(&self, err_msg: &str) -> ParseError {
        ParseError {
            input: self.input.into(),
            location: self.location(),
            err_msg: err_msg.into(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::{is_not_ws, is_ws};
    use test_case::test_case;

    #[test_case("foo",  'f', 0  ; "look on first char: f")]
    #[test_case("foo",  'o', 2  ; "look second first char: o")]
    fn look(input: &str, c: char, p: usize) {
        let mut s = Scanner::new(input);
        s.ptr = p;
        assert_eq!(&c, s.look().unwrap());
    }

    #[test_case("", 0  ; "look with empty input")]
    #[test_case("foo", 3  ; "look after input")]
    fn look_none(input: &str, p: usize) {
        let mut s = Scanner::new(input);
        s.ptr = p;
        assert!(s.look().is_none());
    }

    #[test_case("foo", 0, "fo", true  ; "look_str on po 0")]
    #[test_case("foo",  0, "oo" ,false ; "look_str on po 0, bad str")]
    #[test_case("foo",  1, "oo" ,true ; "look_str on po 1")]
    fn look_str(input: &str, p: usize, look_str: &str, expect: bool) {
        let mut s = Scanner::new(input);
        s.ptr = p;
        assert_eq!(expect, s.look_str(look_str));
    }

    #[test_case("foo", 0, "fo", true  ; "take_str on po 0")]
    #[test_case("foo",  0, "oo" ,false ; "take_str on po 0, bad str")]
    #[test_case("foo",  1, "oo" ,true ; "take_str on po 1")]
    fn take(input: &str, p: usize, take_str: &str, expect: bool) {
        let mut s = Scanner::new(input);
        s.ptr = p;
        assert_eq!(expect, s.look_str(take_str));
    }

    #[test]
    fn location() {
        let inputs = [
            ("foo", 0, Location { line: 1, column: 1 }),
            ("foo", 2, Location { line: 1, column: 2 }),
            ("ba\nr", 1, Location { line: 1, column: 1 }),
            ("ba\nr", 4, Location { line: 2, column: 1 }),
            ("b\na\nrr", 6, Location { line: 3, column: 2 }),
        ];
        for (i, (input, pos, expect)) in inputs.iter().enumerate() {
            let mut s = Scanner::new(input);
            s.ptr = *pos;
            assert_eq!(expect, &s.location(), "Test: {} {}", i, input);
        }
    }

    #[test_case("foo ",  0 ; "remove 0")]
    #[test_case(" foo ",  1 ; "remove 1")]
    #[test_case("  foo ",  2 ; "remove 2")]
    #[test_case(" \n foo ",  3 ; "remove 3n")]
    #[test_case(" \t foo ",  3 ; "remove 3t")]
    #[test_case(" \t\n foo ",  4 ; "remove 4")]
    fn take_while_ws(input: &str, removed_chars: usize) {
        let mut s = Scanner::new(input);
        assert_eq!(removed_chars, s.take_while(is_ws).unwrap().len());
    }

    #[test_case("foo ", "foo")]
    #[test_case("foo \n", "foo")]
    #[test_case("foo \t", "foo")]
    fn look_while(input: &str, expect: &str) {
        let mut s = Scanner::new(input);
        assert_eq!(Ok(expect.into()), s.look_while(is_not_ws));
    }

    #[test_case("foo ", "foo")]
    #[test_case("foo x", "foo")]
    #[test_case("1foo ", "1foo")]
    #[test_case("fo1o ", "fo1o")]
    #[test_case(" foo ", "")]
    fn take_while_is_alphanumeric(input: &str, expect: &str) {
        let mut s = Scanner::new(input);
        assert_eq!(
            Ok(expect.into()),
            s.take_while(|_pos: usize, c: &char| Ok(c.is_alphanumeric()))
        );
    }

    #[test_case("fo2o", ParseError{input: "fo2o".into(),location: Location{ line:1, column:3},err_msg: "is not alphabetic".into()};"fo2o")]
    #[test_case("2foo", ParseError{input: "2foo".into(),location: Location{ line:1, column:1},err_msg: "is not alphabetic".into()};"2foo")]
    #[test_case("foo2", ParseError{input: "foo2".into(),location: Location{ line:1, column:4},err_msg: "is not alphabetic".into()};"foo2")]
    fn take_while_with_err(input: &str, err: ParseError) {
        let mut s = Scanner::new(input);
        assert_eq!(
            err,
            s.take_while(|_pos: usize, c: &char| {
                if c.is_alphabetic() {
                    Ok(true)
                } else {
                    Err("is not alphabetic")
                }
            })
            .err()
            .unwrap()
        );
    }

    #[test_case("fo2o", "'fo2o' error at ln 1, col 3: is not alphabetic"; "fo2o")]
    #[test_case("2foo", "'2foo' error at ln 1, col 1: is not alphabetic"; "2foo")]
    #[test_case("foo2", "'foo2' error at ln 1, col 4: is not alphabetic"; "foo2")]
    fn take_while_with_err_msg(input: &str, err_msg: &str) {
        let mut s = Scanner::new(input);
        assert_eq!(
            err_msg,
            s.take_while(|_pos: usize, c: &char| {
                if c.is_alphabetic() {
                    Ok(true)
                } else {
                    Err("is not alphabetic")
                }
            })
            .err()
            .unwrap()
            .to_string()
        );
    }

    #[test_case(r#""name""#, "name"; "name")]
    #[test_case(r#""name ""#, "name "; "name with space on the end")]
    #[test_case(r#""na me""#, "na me"; "name with space in the middle")]
    #[test_case(r#""na\nme""#, r#"na\nme"#; "name with new line")]
    fn take_surround_double_quote(input: &str, expect: &str) {
        let mut s = Scanner::new(input);
        assert_eq!(expect, s.take_surround(&'"', &'"').unwrap());
    }

    #[test_case(r#"(name (5))"#, "name (5)"; "simple")]
    #[test_case(r#"(name (5) )"#, "name (5) "; "with space")]
    #[test_case(r#"((7) name (5) )'"#, "(7) name (5) "; "nested")]
    fn take_surround_bracket(input: &str, expect: &str) {
        let mut s = Scanner::new(input);
        assert_eq!(expect, s.take_surround(&'(', &')').unwrap());
    }

    #[test_case("(name (5)", ParseError{input: "(name (5)".into(), location: Location{ line:1, column:9},err_msg: "missing closing character: ')'".into()};"(name (5)")]
    #[test_case("(name ", ParseError{input: "(name ".into(), location: Location{ line:1, column:6},err_msg: "missing closing character: ')'".into()};"(name ")]
    #[test_case("age)", ParseError{input: "age)".into(), location: Location{ line:1, column:1},err_msg: "expected character: '(' not found".into()};"age)")]
    fn take_surround_err(input: &str, err: ParseError) {
        let mut s = Scanner::new(input);
        assert_eq!(err, s.take_surround(&'(', &')').err().unwrap());
    }

    #[test_case(
        "(name (5)",
        "'(name (5)' error at ln 1, col 9: missing closing character: ')'" ; "(name (5)")]
    #[test_case(
        "age)",
        "'age)' error at ln 1, col 1: expected character: '(' not found"; "age)"
    )]
    fn take_surround_err_msg(input: &str, err_msg: &str) {
        let mut s = Scanner::new(input);
        assert_eq!(
            err_msg,
            s.take_surround(&'(', &')').err().unwrap().to_string()
        );
    }
}
