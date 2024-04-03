use miette::Result;
use super::{token::tok, StrLitId, Parser};

/// Represents a single executable statement within a COBOL program.
#[derive(Debug)]
pub(crate) enum Stat<'src> {
    Display(StrLitId),
    _Placeholder(&'src str)
}

impl<'src> Parser<'src> {
    /// Parses a single statement from the current parser position.
    pub(super) fn stat(&mut self) -> Result<Stat<'src>> {
        match self.peek() {
            tok![display] => self.parse_display(),
            _ => unreachable!()
        }
    }

    /// Parses a single "DISPLAY" statement from the current parser position.
    fn parse_display(&mut self) -> Result<Stat<'src>> {
        self.consume(tok![display])?;
        let literal = self.consume_str()?;
        let lit_id = self.insert_literal(literal);
        self.consume_vec(&[tok![.], tok![eol]])?;

        Ok(Stat::Display(lit_id))
    }
}