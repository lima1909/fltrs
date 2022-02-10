use core::fmt::{Display, Formatter, Result};

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError<'i>(pub Span<'i>, pub String);

impl<'i> Display for ParseError<'i> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}: {}", self.0, self.1)
    }
}

impl<'i> std::error::Error for ParseError<'i> {
    fn description(&self) -> &str {
        "Parse Error"
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Span<'i> {
    pub input: &'i str,
    pub location: Location,
}

impl<'i> Display for Span<'i> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "'{}' error at ln {}, col {}",
            self.input, self.location.line, self.location.column
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}
