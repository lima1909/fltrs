use core::fmt::{Display, Formatter, Result};

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError {
    pub input: String,
    pub location: Location,
    pub err_msg: String,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(
            f,
            "'{}' error at ln {}, col {}: {}",
            self.input, self.location.line, self.location.column, self.err_msg
        )
    }
}

impl std::error::Error for ParseError {
    fn description(&self) -> &str {
        "Parse Error"
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

#[cfg(test)]
mod test {
    use super::*;

    fn test_impl_send_sync<T: Send + Sync + Unpin>() {}

    #[test]
    fn impl_send_sync_test() {
        test_impl_send_sync::<ParseError>();
    }
}
