use core::fmt::{Display, Formatter, Result};

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError<'i>(pub Span<'i>, pub String);

impl<'i> Display for ParseError<'i> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}", self.0)
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
    pub len: usize,
}

impl<'i> Display for Span<'i> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{}, {} to {}", self.input, self.location, self.len)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "at ln {}, col {}", self.line, self.column)
    }
}
