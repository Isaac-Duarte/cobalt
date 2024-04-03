use miette::Result;

use super::{parser_bail, token::{tok, Token}, Parser, ParserErrorContext};

/// ID for a single string literal within the AST.
pub type StrLitId = usize;

/// A single generic literal within a COBOL AST.
#[derive(Debug)]
pub(crate) enum Literal {
    String(StrLitId),
    Int(i64),
    Float(f64)   
}

impl<'src> Parser<'src> {
    /// Parses a single literal from the current position.
    pub(super) fn literal(&mut self) -> Result<Literal> {
        match self.peek() {
            Token::StringLiteral => {
                let text = self.consume_str()?;
                let id = self.insert_literal(text);
                Ok(Literal::String(id))
            },
            Token::IntLiteral => {
                let int = self.consume_int()?;
                Ok(Literal::Int(int))
            },
            Token::FloatLiteral => {
                let float = self.consume_float()?;
                Ok(Literal::Float(float))
            },
            _ => {
                let next = self.next()?;
                parser_bail!(self, "Expected a literal, instead found '{}'.", next.0);
            }
        }
    }

    /// Expects the next token to be a string literal, consumes it, and returns a parsed version
    /// of the string contained within. If the token is not a string, returns a parser error.
    pub fn consume_str(&mut self) -> Result<String> {
        let lit_tok = self.consume(tok![str_literal])?;
        let txt = self.text(lit_tok);

        // Remove the trailing/leading quote marks.
        let mut chars = txt.chars();
        chars.next();
        chars.next_back();
        let txt = chars.as_str();

        // Replace escape codes with their relevant real characters.
        let mut txt = txt.replace("\\n", "\n");
        txt = txt.replace("\\r", "\r");
        txt = txt.replace("\\t", "\t");
        Ok(txt)
    }

    /// Expects the next token to be an integer literal, consumes it, and returns a
    /// parsed version of the integer contained within. Must fit within an [`i64`], otherwise
    /// returns an error. Similarly errors on an invalid integer value.
    pub fn consume_int(&mut self) -> Result<i64> {
        let lit_tok = self.consume(tok![int_lit])?;
        let txt = self.text(lit_tok);
        txt.parse::<i64>()
            .ctx(self, format!("Failed to parse integer literal: {}", txt))
    }

    /// Expects the next token to be an float literal, consumes it, and returns a
    /// parsed version of the float contained within. Must fit within a [`f64`], otherwise
    /// returns an error. Similarly errors on an invalid float value.
    pub fn consume_float(&mut self) -> Result<f64> {
        let lit_tok = self.consume(tok![float_lit])?;
        let txt = self.text(lit_tok);
        txt.parse::<f64>()
            .ctx(self, format!("Failed to parse float literal: {}", txt))
    }
}