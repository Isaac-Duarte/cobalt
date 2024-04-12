use crate::compiler::parser::parser_bail_spanned;

use super::{
    parser_bail, token::{tok, Token}, Parser, Spanned, Value
};
use miette::Result;

/// Represents a single executable statement within a COBOL program.
#[derive(Debug)]
pub(crate) enum Stat<'src> {
    Display(Vec<Value<'src>>),
    Move(MoveData<'src>),
    Add(BasicMathOpData<'src>),
    Subtract(BasicMathOpData<'src>),
    Multiply(BasicMathOpData<'src>),
    Divide(DivideData<'src>),
    If(IfData<'src>)
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

/// Data for a single "ADD", "SUB" or "MUL" instruction, from (possibly) multiple sources to a set of destinations.
#[derive(Debug)]
pub(crate) struct BasicMathOpData<'src> {
    /// The sources of the arithmetic instruction.
    pub sources: Vec<Value<'src>>,

    /// The destinations of the arithmetic instruction.
    pub dests: Vec<&'src str>,

    /// Whether to overwrite the destination value, instead of including it with the sources.
    pub overwrite_dests: bool,
}

impl<'src> Parser<'src> {
    /// Parses a single "ADD" statement from the current position.
    fn parse_add(&mut self) -> Result<Stat<'src>> {
        self.consume(tok![add])?;
        let op_data = self.parse_math_op(tok![to])?;
        self.consume_vec(&[tok![.], tok![eol]])?;

        Ok(Stat::Add(op_data))
    }

    /// Parses a single "SUBTRACT" statement from the current position.
    fn parse_subtract(&mut self) -> Result<Stat<'src>> {
        self.consume(tok![subtract])?;
        let op_data = self.parse_math_op(tok![from])?;
        self.consume_vec(&[tok![.], tok![eol]])?;

        Ok(Stat::Subtract(op_data))
    }

    /// Parses a single "MULTIPLY" statement from the current position.
    fn parse_multiply(&mut self) -> Result<Stat<'src>> {
        self.consume(tok![multiply])?;
        let op_data = self.parse_math_op(tok![by])?;
        self.consume_vec(&[tok![.], tok![eol]])?;

        Ok(Stat::Multiply(op_data))
    }

    /// Parses data for a single mathematical operation (ADD, SUB, MUL) from the current location.
    /// Takes a separator which denotes the end of the source arguments and beginning of destination arguments.
    fn parse_math_op(&mut self, sep: Token) -> Result<BasicMathOpData<'src>> {
        let mut sources: Vec<Value<'src>> = Vec::new();
        let mut dests: Vec<&'src str> = Vec::new();

        // Grab all source values (there must be at least one).
        loop {
            sources.push(self.value()?);
            if !Value::is_value(self.peek()) {
                break;
            }
        }
        self.consume(sep)?;

        // Get a single destination value.
        // We have to get one first, since there might be a "GIVING" clause, which would
        // actually make this "destination" a source value for the add... thanks COBOL.
        let first_dest_tok = self.consume(tok![ident])?;
        let first_dest = self.text(first_dest_tok);

        // If there is a "GIVING" clause, get that here.
        let mut overwrite_dests = false;
        if self.peek() == tok![giving] {
            self.next()?;
            overwrite_dests = true;

            // The first "destination" is actually a source.
            sources.push(Value::Variable(first_dest));

            // Fetch the single *actual* destination.
            let dest_tok = self.consume(tok![ident])?;
            dests.push(self.text(dest_tok));
        } else {
            // No "GIVING" clause, simply iterate destinations until the end.
            dests.push(first_dest);
            while self.peek() == tok![ident] {
                let dest_tok = self.consume(tok![ident])?;
                dests.push(self.text(dest_tok));
            }
        }

        Ok(BasicMathOpData {
            sources,
            dests,
            overwrite_dests,
        })
    }
}

/// Operand data for a single DIVIDE instruction.
#[derive(Debug)]
pub(crate) struct DivideData<'src> {
    /// The variable being divided.
    pub dividend: &'src str,

    /// The variable used as the divisor.
    pub divisor: &'src str,

    /// The variable that output is placed in.
    /// May be the same as the divisor.
    pub out_var: &'src str,
}

impl<'src> Parser<'src> {
    /// Parses a single "DIVIDE" statement from the current position.
    fn parse_divide(&mut self) -> Result<Stat<'src>> {
        self.consume(tok![divide])?;
        let first_tok = self.consume(tok![ident])?;

        // Determine whether we're dividing the first variable by the second, or
        // the second variable by the first.
        let left_to_right = if self.peek() == tok![by] {
            self.next()?;
            true
        } else if self.peek() == tok![into] {
            self.next()?;
            false
        } else {
            let err_tok = self.next()?.0;
            parser_bail!(
                self,
                "Expected either 'INTO' or 'BY', instead found '{err_tok}'."
            );
        };

        let second_tok = self.consume(tok![ident])?;

        // If we're using "BY", there must be a "GIVING" clause.
        let out_var = if self.peek() == tok![giving] {
            self.next()?;
            let output_tok = self.consume(tok![ident])?;
            self.text(output_tok)
        } else if !left_to_right {
            self.text(second_tok)
        } else {
            parser_bail!(
                self,
                "DIVIDE statements utilising a 'BY' clause must also include a 'GIVING' clause."
            );
        };
        self.consume_vec(&[tok![.], tok![eol]])?;

        // Determine which token is the dividend, and which is the divisor.
        let (dividend_tok, divisor_tok) = if left_to_right {
            (first_tok, second_tok)
        } else {
            (second_tok, first_tok)
        };

        Ok(Stat::Divide(DivideData {
            dividend: self.text(dividend_tok),
            divisor: self.text(divisor_tok),
            out_var,
        }))
    }
}

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
    fn parse_if(&mut self) -> Result<Stat<'src>> {
        self.consume(tok![if])?;
        let mut condition = self.parse_cond()?;
        self.consume_vec(&[tok![then], tok![eol]])?;

        // Iterate & parse out "IF" statement block.
        let mut if_stats: Vec<Spanned<Stat<'src>>> = Vec::new();
        while self.peek() != tok![end] && self.peek() != tok![else] {
            if_stats.push(self.stat()?);
        }
        let mut if_stats = (if_stats.len() > 0).then(|| if_stats);

        // If there's an "ELSE" statement block, parse that out.
        let mut else_stats = if self.peek() == tok![else] {
            self.consume_vec(&[tok![else], tok![eol]])?;
            let mut stats: Vec<Spanned<Stat<'src>>> = Vec::new();
            while self.peek() != tok![end] {
                stats.push(self.stat()?);
            }
            Some(stats)
        } else {
            None
        };

        // If we only have an "ELSE" block, we can optimise into a simple inverted condition.
        if if_stats.is_none() && else_stats.as_ref().is_some_and(|s| s.len() > 0) {
            if_stats = else_stats;
            else_stats = None;
            condition = Cond::Not(Box::new(condition));   
        }

        self.consume_vec(&[tok![end], tok![if], tok![.], tok![eol]])?;

        Ok(Stat::If(IfData {
            if_stats,
            else_stats,
            condition
        }))
    }

    /// Parses a single condition from the current position.
    /// todo: Implement remaining conditional types.
    fn parse_cond(&mut self) -> Result<Cond<'src>> {
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
            tok @ _ => {
                parser_bail_spanned!(self, operator.1, "Unknown operator '{}' used in conditional.", tok);
            }
        };

        // If there's a following condition, recursively parse that.
        if self.peek() == tok![and] || self.peek() == tok![or] {
            cond = match self.next()?.0 {
                tok![and] => Cond::And(Box::new(cond), Box::new(self.parse_cond()?)),
                tok![or] => Cond::Or(Box::new(cond), Box::new(self.parse_cond()?)),
                _ => unreachable!()
            }
        }

        Ok(cond)
    }
}