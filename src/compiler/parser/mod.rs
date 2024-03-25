/**
 * Structures and utilities for parsing a COBOL AST.
 */
use std::iter::Peekable;

use miette::{Result, NamedSource, SourceSpan};
use self::token::{tok, Lexer, Token};
use crate::compiler::parser::err::GenericParseError;

mod token;
mod ast;
mod divs;
mod err;
mod stat;

//Macro for exiting with a given parser error message.
macro_rules! parser_bail {
    ($parser:ident, $msg:tt) => {{
        return Err(GenericParseError::new($parser, format!($msg)))?
    }};

    ($parser:ident, $msg:tt, $($arg:tt)*) => {{
        return Err(GenericParseError::new($parser, format!($msg, $($arg)*)))?
    }};
}
pub(crate) use parser_bail;

/// Exports.
pub use ast::Ast;
pub(crate) use stat::Stat;

/// Simple span types for use in AST items throughout the parser.
pub type Span = SourceSpan;
pub type Spanned<T> = (T, Span);

/// Represents a single compile unit parser for Cobalt.
pub(crate) struct Parser<'src> {
    input: &'src str,
    cu_name: &'src str,
    tokens: Peekable<Lexer<'src>>,
    cur: Option<Spanned<Token>>
}

impl<'src> Parser<'src> {
    /// Creates a new parser for a given compile unit.
    pub fn new(cu_name: &'src str, input: &'src str) -> Parser<'src> {
        Self {
            input,
            cu_name,
            tokens: Lexer::new(input).peekable(),
            cur: None
        }
    }

    //Returns the text for the provided token's span.
    pub fn text(&self, token: Spanned<Token>) -> &'src str {
        &self.input[token.1.offset()..(token.1.offset() + token.1.len())]
    }

    /// Returns the current parser token, if any.
    pub fn cur(&self) -> Option<Spanned<Token>> {
        self.cur
    }

    //Returns the current token index of the parser (end of the previous token's span).
    pub fn cur_idx(&self) -> usize {
        match self.cur { 
            Some(tok) => tok.1.offset() + tok.1.len(),
            None => 0
        }
    }

    /// Returns the next peeked token, without moving the parser's cursor.
    /// If there is no next token, returns EOF.
    pub fn peek(&mut self) -> Token {
        self.tokens.peek().map(|tok| tok.0).unwrap_or(tok![eof])
    }

    //Returns the index of the start of the next token (if present).
    //Otherwise, returns the end index of the current token.
    pub fn peek_idx(&mut self) -> usize {
        match self.tokens.peek() {
            Some(tok) => tok.1.offset(),
            None => self.cur_idx()
        }
    }

    /// Returns the next available token, if there is one.
    /// If there is no next token, returns a parser error.
    pub fn next(&mut self) -> Result<Spanned<Token>> {
        let next = self.tokens.next();
        match next {
            Some(tok) => {
                self.cur = Some(tok);
                Ok(tok)
            }
            None => parser_bail!(self, "Expected a following token, instead found EOF."),
        }
    }

    /// Steps forward one token, expecting the next token to be of a certain type.
    /// Returns a parser error on fail.
    pub fn consume(&mut self, expected: Token) -> Result<Spanned<Token>> {
        match self.next() {
            Ok(tok) => {
                if tok.0 != expected {
                    parser_bail!(
                        self,
                        "Expected token '{}', instead found token '{}'.",
                        expected,
                        tok.0
                    );
                }
                Ok(tok)
            }
            Err(_) => parser_bail!(
                self,
                "Expected token {:?}, but no token (EOF) was found.",
                expected
            ),
        }
    }

    /// Steps forward multiple tokens, expecting the next tokens (in order) to be those of a
    /// certain type. Returns a parser error on fail.
    pub fn consume_vec(&mut self, expected: &[Token]) -> Result<()> {
        for tok in expected {
            self.consume(*tok)?;
        }
        Ok(())
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

    //Returns the source code being parsed as a NamedSource.
    pub fn get_named_source(&self) -> NamedSource<String> {
        NamedSource::new(self.cu_name.to_string(), self.input.to_string())
    }
}