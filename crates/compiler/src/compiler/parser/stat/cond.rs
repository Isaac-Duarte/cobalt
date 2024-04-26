use miette::Result;

use crate::compiler::parser::{parser_bail_spanned, token::tok, Parser, Spanned, Value};

use super::Stat;

/// Data required for a single "IF" conditional.
#[derive(Debug)]
pub(crate) struct IfData<'src> {
    pub condition: Cond<'src>,
    pub if_stats: Option<Vec<Spanned<Stat<'src>>>>,
    pub else_stats: Option<Vec<Spanned<Stat<'src>>>>,
}

/// A single generic condition within Cobalt.
#[derive(Debug)]
pub(crate) enum Cond<'src> {
    Eq(Value<'src>, Value<'src>),
    Ge(Value<'src>, Value<'src>),
    Le(Value<'src>, Value<'src>),
    Gt(Value<'src>, Value<'src>),
    Lt(Value<'src>, Value<'src>),
    And(Box<Cond<'src>>, Box<Cond<'src>>),
    Or(Box<Cond<'src>>, Box<Cond<'src>>),
    Not(Box<Cond<'src>>),
}

impl<'src> Parser<'src> {
    /// Parses a single "IF" statement from the current position.
    pub(super) fn parse_if(&mut self) -> Result<Stat<'src>> {
        self.consume(tok![if])?;
        let mut condition = self.parse_cond()?;
        self.consume_vec(&[tok![then], tok![eol]])?;

        // Iterate & parse out "IF" statement block.
        let mut if_stats: Vec<Spanned<Stat<'src>>> = Vec::new();
        while self.peek() != tok![end_if] && self.peek() != tok![else] {
            if_stats.push(self.stat(false)?);
        }
        let mut if_stats = (!if_stats.is_empty()).then_some(if_stats);

        // If there's an "ELSE" statement block, parse that out.
        let mut else_stats = if self.peek() == tok![else] {
            self.consume_vec(&[tok![else], tok![eol]])?;
            let mut stats: Vec<Spanned<Stat<'src>>> = Vec::new();
            while self.peek() != tok![end_if] {
                stats.push(self.stat(false)?);
            }
            Some(stats)
        } else {
            None
        };

        // If we only have an "ELSE" block, we can optimise into a simple inverted condition.
        if if_stats.is_none() && else_stats.as_ref().is_some_and(|s| !s.is_empty()) {
            if_stats = else_stats;
            else_stats = None;
            condition = Cond::Not(Box::new(condition));
        }

        self.consume(tok![end_if])?;

        Ok(Stat::If(IfData {
            if_stats,
            else_stats,
            condition,
        }))
    }

    /// Parses a single condition from the current position.
    /// todo: Implement remaining conditional types.
    pub(super) fn parse_cond(&mut self) -> Result<Cond<'src>> {
        // If there's a preceding "NOT", we NOT a following condition.
        if self.peek() == tok![not] {
            self.next()?;
            return Ok(Cond::Not(Box::new(self.parse_cond()?)));
        }

        // Parse out an initial value, operator & second value.
        let first_op = self.value()?;
        let operator = self.next()?;
        let second_op = self.value()?;

        let mut cond = match operator.0 {
            tok![=] => Cond::Eq(first_op, second_op),
            tok![<] => Cond::Lt(first_op, second_op),
            tok![>] => Cond::Gt(first_op, second_op),
            tok![<=] => Cond::Le(first_op, second_op),
            tok![>=] => Cond::Ge(first_op, second_op),
            tok => {
                parser_bail_spanned!(
                    self,
                    operator.1,
                    "Unknown operator '{}' used in conditional.",
                    tok
                );
            }
        };

        // If there's a following condition, recursively parse that.
        if self.peek() == tok![and] || self.peek() == tok![or] {
            cond = match self.next()?.0 {
                tok![and] => Cond::And(Box::new(cond), Box::new(self.parse_cond()?)),
                tok![or] => Cond::Or(Box::new(cond), Box::new(self.parse_cond()?)),
                _ => unreachable!(),
            }
        }

        Ok(cond)
    }
}
