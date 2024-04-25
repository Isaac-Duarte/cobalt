use crate::compiler::parser::{parser_bail, token::tok, Parser, Value};

use super::{Cond, Stat};

use miette::Result;


/// Available variants for a single "PERFORM" instruction.
#[derive(Debug)]
pub(crate) enum PerformType<'src> {
    Single(&'src str),
    Thru(&'src str, &'src str),
    Until {
        target: &'src str,
        cond: Cond<'src>,
        test_cond_before: bool,
    },
    Times(&'src str, Value<'src>),
    // Varying()
}

impl<'src> Parser<'src> {
    /// Parses a single "PERFORM" statement from the current position.
    pub(super) fn parse_perform(&mut self) -> Result<Stat<'src>> {
        self.consume(tok![perform])?;
        let first_para_tok = self.consume(tok![ident])?;
        let first_para_txt = self.text(first_para_tok);
        let perform = match self.peek() {
            // PERFORM X
            tok![.] => {
                self.consume_vec(&[tok![.], tok![eol]])?;
                PerformType::Single(first_para_txt)
            }

            // PERFORM X THRU Y
            tok![thru] => {
                self.next()?;
                let end_para_tok = self.consume(tok![ident])?;
                self.consume_vec(&[tok![.], tok![eol]])?;
                PerformType::Thru(first_para_txt, self.text(end_para_tok))
            }

            // PERFORM X UNTIL Y=Z
            tok![test_before] | tok![test_after] | tok![until] => {
                // Determine which side the check is on.
                let test_cond_before = match self.peek() {
                    tok![test_after] => {
                        self.next()?;
                        false
                    }
                    tok![test_before] => {
                        self.next()?;
                        true
                    }
                    _ => true,
                };

                self.consume(tok![until])?;
                let cond = self.parse_cond()?;
                self.consume_vec(&[tok![.], tok![eol]])?;
                PerformType::Until {
                    target: first_para_txt,
                    cond,
                    test_cond_before,
                }
            }

            // PERFORM X Y TIMES
            tok![ident] | tok![int_lit] => {
                let val = self.value()?;
                self.consume_vec(&[tok![times], tok![.], tok![eol]])?;
                PerformType::Times(first_para_txt, val)
            }

            tok => {
                self.next()?;
                parser_bail!(self, "Unknown token in PERFORM statement '{}'.", tok);
            }
        };

        Ok(Stat::Perform(perform))
    }
}

/// Available variants of the "EXIT" instruction.
#[derive(Debug)]
pub(crate) enum ExitType {
    Paragraph,
    // Section,
}

impl<'src> Parser<'src> {
    /// Parses a single "EXIT" statement from the current position.
    pub(super) fn parse_exit(&mut self) -> Result<Stat<'src>> {
        self.consume(tok![exit])?;
        let exit_type = match self.peek() {
            tok![paragraph] => {
                ExitType::Paragraph
            },
            tok => {
                parser_bail!(self, "Expected one of: PARAGRAPH for exit statement, instead found {}.", tok);
            }
        };
        self.next()?;
        self.consume_vec(&[tok![.], tok![eol]])?;
        Ok(Stat::Exit(exit_type))
    }
}