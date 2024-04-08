use super::{
    parser_bail,
    token::{tok, Token},
    Parser, Value,
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
}

impl<'src> Parser<'src> {
    /// Parses a single statement from the current parser position.
    pub(super) fn stat(&mut self) -> Result<Stat<'src>> {
        match self.peek() {
            tok![display] => self.parse_display(),
            tok![move] => self.parse_move(),
            tok![add] => self.parse_add(),
            tok![subtract] => self.parse_subtract(),
            tok![multiply] => self.parse_multiply(),
            tok![divide] => self.parse_divide(),

            // Unknown token.
            tok @ _ => {
                self.next()?;
                parser_bail!(
                    self,
                    "Invalid token '{}' encountered, expected beginning of statement.",
                    tok
                );
            }
        }
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
