use super::{parser_bail, token::tok, Literal, Parser, Spanned, Value};
use miette::Result;

pub(crate) use cond::*;
pub(crate) use control::*;
pub(crate) use intrinsics::*;
pub(crate) use math::*;

mod cond;
mod control;
mod intrinsics;
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
    Accept(&'src str),
    Goto(&'src str),
    Exit(ExitType),
}

impl<'src> Parser<'src> {
    /// Parses a single statement from the current parser position.
    /// Takes whether this statement should end with a "." in addition to a newline.
    pub(super) fn stat(&mut self, parse_dot: bool) -> Result<Spanned<Stat<'src>>> {
        let start_idx = self.peek_idx();

        // Parse the statement body.
        let stat = match self.peek() {
            tok![display] => self.parse_display()?,
            tok![move] => self.parse_move()?,
            tok![add] => self.parse_add()?,
            tok![subtract] => self.parse_subtract()?,
            tok![multiply] => self.parse_multiply()?,
            tok![divide] => self.parse_divide()?,
            tok![if] => self.parse_if()?,
            tok![perform] => self.parse_perform()?,
            tok![accept] => self.parse_accept()?,
            tok![goto] => self.parse_goto()?,
            tok![exit] => self.parse_exit()?,

            // Unknown token.
            tok => {
                self.next()?;
                parser_bail!(
                    self,
                    "Invalid token '{}' encountered, expected beginning of statement.",
                    tok
                );
            }
        };

        // Parse the dot (optionally) and newline out.
        if parse_dot {
            self.consume(tok![.])?;
        }
        self.consume(tok![eol])?;

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

        Ok(Stat::Display(to_display))
    }

    /// Parses a single "ACCEPT" statement from the current position.
    fn parse_accept(&mut self) -> Result<Stat<'src>> {
        self.consume(tok![accept])?;
        let ident_tok = self.consume(tok![ident])?;
        Ok(Stat::Accept(self.text(ident_tok)))
    }
}

/// Data for a single "MOVE" instruction, from one source to one destination.
#[derive(Debug)]
pub(crate) struct MoveData<'src> {
    pub source: MoveSource<'src>,
    pub dest: MoveRef<'src>,
}

/// All available sources for a MOVE instruction.
#[derive(Debug)]
pub(crate) enum MoveSource<'src> {
    MoveRef(MoveRef<'src>),
    Literal(Literal),
    Intrinsic(IntrinsicCall<'src>),
}

/// Represents a single spanned source or destination for a referenced MOVE instruction.
/// Used for allowing substring accesses and moves for PIC X(N) variables.
#[derive(Debug)]
pub(crate) struct MoveRef<'src> {
    /// The underyling variable for this move reference.
    pub sym: &'src str,

    /// The span of this variable that is targeted, if specified.
    /// This is only valid on variables of type PIC X(N), and is verified at code generation.
    pub span: Option<MoveSpan<'src>>,
}

/// A single target span within a move reference variable.
#[derive(Debug)]
pub(crate) struct MoveSpan<'src> {
    pub start_idx: Value<'src>,
    pub len: Option<Value<'src>>,
}

impl<'src> Parser<'src> {
    /// Parses a single "MOVE" statement from the current position.
    fn parse_move(&mut self) -> Result<Stat<'src>> {
        self.consume(tok![move])?;

        // Parse the source.
        let source = if self.peek() == tok![function] {
            self.next()?;
            MoveSource::Intrinsic(self.intrinsic_call()?)
        } else {
            let value = self.value()?;
            match value {
                Value::Literal(lit) => MoveSource::Literal(lit),
                Value::Variable(sym) => {
                    // There may be a span specified, check.
                    let span = (self.peek() == tok![open_par])
                        .then(|| self.parse_span())
                        .transpose()?;
                    MoveSource::MoveRef(MoveRef { sym, span })
                }
            }
        };

        // Parse the destination.
        self.consume(tok![to])?;
        let dest_tok = self.consume(tok![ident])?;
        let sym = &self.text(dest_tok);
        let span = (self.peek() == tok![open_par])
            .then(|| self.parse_span())
            .transpose()?;
        let dest = MoveRef { sym, span };

        Ok(Stat::Move(MoveData { source, dest }))
    }

    /// Parses a single [`MoveSpan`] from the current position.
    fn parse_span(&mut self) -> Result<MoveSpan<'src>> {
        // Consume (S, E).
        self.consume(tok![open_par])?;
        let start_idx = self.value()?;
        self.consume(tok![:])?;
        let len = (self.peek() != tok![close_par])
            .then(|| self.value())
            .transpose()?;
        self.consume(tok![close_par])?;

        Ok(MoveSpan { start_idx, len })
    }
}
