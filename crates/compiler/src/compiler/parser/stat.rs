use super::{parser_bail, token::tok, Parser, Value};
use miette::Result;

/// Represents a single executable statement within a COBOL program.
#[derive(Debug)]
pub(crate) enum Stat<'src> {
    Display(Vec<Value<'src>>),
    Move(MoveData<'src>),
    Add(AddData<'src>),
}

impl<'src> Parser<'src> {
    /// Parses a single statement from the current parser position.
    pub(super) fn stat(&mut self) -> Result<Stat<'src>> {
        match self.peek() {
            tok![display] => self.parse_display(),
            tok![move] => self.parse_move(),
            tok![add] => self.parse_add(),

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

/// Data for a single "ADD" instruction, from (possibly) multiple sources to a set of destinations.
#[derive(Debug)]
pub(crate) struct AddData<'src> {
    /// The sources of the add instruction.
    pub sources: Vec<Value<'src>>,

    /// The destinations of the add instruction.
    pub dests: Vec<&'src str>,

    /// Whether to overwrite the destination value, instead of adding the sources to it.
    pub overwrite_dests: bool,
}

impl<'src> Parser<'src> {
    /// Parses a single "ADD" statement from the current position.
    fn parse_add(&mut self) -> Result<Stat<'src>> {
        self.consume(tok![add])?;
        let mut sources: Vec<Value<'src>> = Vec::new();
        let mut dests: Vec<&'src str> = Vec::new();

        // Grab all source values (there must be at least one).
        loop {
            sources.push(self.value()?);
            if !Value::is_value(self.peek()) {
                break;
            }
        }
        self.consume(tok![to])?;

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

        // Parse end of the statement, all done.
        self.consume_vec(&[tok![.], tok![eol]])?;
        Ok(Stat::Add(AddData {
            sources,
            dests,
            overwrite_dests,
        }))
    }
}
