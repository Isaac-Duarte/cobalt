use super::{
    parser_bail, token::tok, Parser, Spanned, Value
};
use miette::Result;

pub(crate) use cond::*;
pub(crate) use math::*;

mod cond;
mod math;

/// Represents a single executable statement within a COBOL program.
#[derive(Debug)]
pub(crate) enum Stat<'src> {
    Display(Vec<Value<'src>>),
    Move(MoveData<'src>),
    Add(BasicMathOpData<'src>),
    Subtract(BasicMathOpData<'src>),
    Multiply(BasicMathOpData<'src>),
    Divide(DivideData<'src>),
    If(IfData<'src>),
    Perform(PerformType<'src>),
}

impl<'src> Parser<'src> {
    /// Parses a single statement from the current parser position.
    pub(super) fn stat(&mut self) -> Result<Spanned<Stat<'src>>> {
        let start_idx = self.peek_idx();
        let stat = match self.peek() {
            tok![display] => self.parse_display()?,
            tok![move] => self.parse_move()?,
            tok![add] => self.parse_add()?,
            tok![subtract] => self.parse_subtract()?,
            tok![multiply] => self.parse_multiply()?,
            tok![divide] => self.parse_divide()?,
            tok![if] => self.parse_if()?,
            tok![perform] => self.parse_perform()?,

            // Unknown token.
            tok @ _ => {
                self.next()?;
                parser_bail!(
                    self,
                    "Invalid token '{}' encountered, expected beginning of statement.",
                    tok
                );
            }
        };

        Ok((stat, (start_idx, self.cur_idx()).into()))
    }

    /// Parses a single "DISPLAY" statement from the current parser position.
    fn parse_display(&mut self) -> Result<Stat<'src>> {
        self.consume(tok![display])?;

        // Consume all values to be displayed (there must be at least one).
        let mut to_display: Vec<Value<'src>> = Vec::new();
        loop {
            to_display.push(self.value()?);
            if !Value::is_value(self.peek()) {
                break;
            }
        }
        self.consume_vec(&[tok![.], tok![eol]])?;

        Ok(Stat::Display(to_display))
    }
}

/// Data for a single "MOVE" instruction, from one source to one destination.
#[derive(Debug)]
pub(crate) struct MoveData<'src> {
    pub source: Value<'src>,
    pub dest: &'src str,
}

impl<'src> Parser<'src> {
    /// Parses a single "MOVE" statement from the current position.
    fn parse_move(&mut self) -> Result<Stat<'src>> {
        self.consume(tok![move])?;
        let source = self.value()?;
        self.consume(tok![to])?;
        let dest_tok = self.consume(tok![ident])?;
        let dest = &self.text(dest_tok);
        self.consume_vec(&[tok![.], tok![eol]])?;

        Ok(Stat::Move(MoveData { source, dest }))
    }
}

/// Available variants for a single "PERFORM" instruction.
#[derive(Debug)]
pub(crate) enum PerformType<'src> {
    Single(&'src str),
    Thru(&'src str, &'src str),
    Until {
        target: &'src str,
        cond: Cond<'src>,
        test_cond_before: bool
    },
    Times(&'src str, Value<'src>),
    // Varying()
}

impl<'src> Parser<'src> {
    /// Parses a single "PERFORM" statement from the current position.
    fn parse_perform(&mut self) -> Result<Stat<'src>> {
        self.consume(tok![perform])?;
        let first_para_tok = self.consume(tok![ident])?;
        let first_para_txt = self.text(first_para_tok);
        let perform = match self.peek() {
            // PERFORM X
            tok![.] => {
                self.consume_vec(&[tok![.], tok![eol]])?;
                PerformType::Single(first_para_txt)
            },

            // PERFORM X THRU Y
            tok![thru] => {
                self.next()?;
                let end_para_tok = self.consume(tok![ident])?;
                self.consume_vec(&[tok![.], tok![eol]])?;
                PerformType::Thru(first_para_txt, self.text(end_para_tok))
            },

            // PERFORM X UNTIL Y=Z
            tok![test_before] | tok![test_after] | tok![until] => {
                // Determine which side the check is on.
                let test_cond_before = match self.peek() {
                    tok![test_after] => {
                        self.next()?;
                        false
                    },
                    tok![test_before] => {
                        self.next()?;
                        true
                    },
                    _ => true,
                };

                self.consume(tok![until])?;
                let cond = self.parse_cond()?;
                self.consume_vec(&[tok![.], tok![eol]])?;
                PerformType::Until { target: first_para_txt, cond, test_cond_before }
            },

            // PERFORM X Y TIMES
            tok![ident] | tok![int_lit] => {
                let val = self.value()?;
                self.consume_vec(&[tok![times], tok![.], tok![eol]])?;
                PerformType::Times(first_para_txt, val)
            },

            tok @ _ => {
                self.next()?;
                parser_bail!(self, "Unknown token in PERFORM statement '{}'.", tok);
            }
        };

        Ok(Stat::Perform(perform))
    }
}