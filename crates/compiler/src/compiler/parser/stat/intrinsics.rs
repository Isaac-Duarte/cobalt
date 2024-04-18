use miette::Result;

use crate::compiler::parser::{parser_bail, token::tok, Parser, Value};

/// Represents a single call to a COBOL intrinsic from within a compile unit.
#[derive(Debug)]
pub(crate) struct IntrinsicCall<'src> {
    /// The name of the intrinsic to call.
    pub name: &'src str,

    /// The arguments passed to the intrinsic.
    pub args: Vec<Value<'src>>,
}

impl<'src> Parser<'src> {
    /// Parses a single intrinsic call from the current position.
    pub(super) fn intrinsic_call(&mut self) -> Result<IntrinsicCall<'src>> {
        let name_tok = self.consume(tok![ident])?;
        let name = self.text(name_tok);

        // Parse out a set of arguments.
        let mut args: Vec<Value<'src>> = Vec::new();
        if self.peek() == tok![open_par] {
            self.next()?;
            while self.peek() != tok![close_par] {
                // Consume argument value.
                args.push(self.value()?);

                // Consume comma if there's a following argument.
                if self.peek() == tok![,] {
                    self.next()?;

                    // Don't permit the pattern `FUNC(arg,)`.
                    if self.peek() == tok![close_par] {
                        let err_tok = self.next()?;
                        parser_bail!(
                            self,
                            "Expected argument, instead found token '{}'",
                            err_tok.0
                        );
                    }
                }
            }
            self.consume(tok![close_par])?;
        }

        Ok(IntrinsicCall { name, args })
    }
}
