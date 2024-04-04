use miette::Result;

use super::{token::tok, Literal, Parser};

/// Represents a single stored or literal value expressed within the AST.
#[derive(Debug)]
pub(crate) enum Value<'src> {
    Variable(&'src str),
    Literal(Literal),
}

impl<'src> Parser<'src> {
    /// Parses a single stored/literal value from the current position.
    pub(super) fn value(&mut self) -> Result<Value<'src>> {
        if self.peek() == tok![ident] {
            let tok = self.next()?;
            Ok(Value::Variable(self.text(tok)))
        } else {
            Ok(Value::Literal(self.literal()?))
        }
    }
}
