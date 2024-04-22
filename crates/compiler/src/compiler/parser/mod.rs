/**
 * Structures and utilities for parsing a COBOL AST.
 */
use std::iter::Peekable;

use self::token::{tok, Lexer, Token};
use crate::compiler::parser::err::GenericParseError;
use miette::{NamedSource, Result, SourceSpan};

mod ast;
mod data;
mod divs;
mod err;
mod expr;
mod lits;
mod stat;
mod token;

/// Macro for exiting with a given parser error message.
macro_rules! parser_bail {
    ($parser:expr, $msg:tt) => {{
        return Err($crate::compiler::parser::GenericParseError::new($parser, format!($msg)))?
    }};

    ($parser:expr, $msg:tt, $($arg:tt)*) => {{
        return Err($crate::compiler::parser::GenericParseError::new($parser, format!($msg, $($arg)*)))?
    }};
}
pub(crate) use parser_bail;

/// Macro for exiting with a given error message and span.
macro_rules! parser_bail_spanned {
    ($parser:expr, $span:expr, $msg:tt) => {{
        return Err($crate::compiler::parser::GenericParseError::with_span($parser, $span, format!($msg)))?
    }};

    ($parser:expr, $span:expr, $msg:tt, $($arg:tt)*) => {{
        return Err($crate::compiler::parser::GenericParseError::with_span($parser, $span, format!($msg, $($arg)*)))?
    }};
}
pub(crate) use parser_bail_spanned;

/// Exports.
pub use ast::Ast;
pub(crate) use data::*;
pub(crate) use divs::*;
pub(crate) use expr::*;
pub(crate) use lits::*;
pub(crate) use stat::*;

/// Simple span types for use in AST items throughout the parser.
pub type Span = SourceSpan;
pub type Spanned<T> = (T, Span);

/// Represents a single compile unit parser for Cobalt.
pub(crate) struct Parser<'src> {
    /// The full input source for the compile unit.
    input: &'src str,

    /// The name of the compile unit (usually the file name).
    cu_name: &'src str,

    /// Lexer iterator for tokens produced.
    tokens: Peekable<Lexer<'src>>,

    /// The current token the parser is pointed at.
    cur: Option<Spanned<Token>>,

    /// Store of string literals created by this parser.
    /// This is required as we need some access to a global list of string
    /// literals in order to determine the strings to store in `.rodata` later on.
    str_lits: StrLitStore,
}

impl<'src> Parser<'src> {
    /// Creates a new parser for a given compile unit.
    pub fn new(cu_name: &'src str, input: &'src str) -> Parser<'src> {
        Self {
            input,
            cu_name,
            tokens: Lexer::new(input).peekable(),
            cur: None,
            str_lits: StrLitStore::new(),
        }
    }

    /// Performs a full parse of the compile unit, returning an AST.
    pub fn parse(self) -> Result<Ast<'src>> {
        self.ast()
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
            None => 0,
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
            None => self.cur_idx(),
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

    //Returns the source code being parsed as a NamedSource.
    pub fn get_named_source(&self) -> NamedSource<String> {
        NamedSource::new(self.cu_name, self.input.to_string())
    }
}

//Trait for adding context to arbitrary objects to return as a result.
pub trait ParserErrorContext<T> {
    fn ctx(self, parser: &Parser, msg: String) -> Result<T>;
}

//Extension for allowing addition of parser context to Result objects for return.
impl<T, E: std::error::Error> ParserErrorContext<T> for Result<T, E> {
    fn ctx(self, parser: &Parser, msg: String) -> Result<T> {
        match self {
            Err(_) => Err(GenericParseError::new(parser, msg))?,
            Ok(res) => Ok(res),
        }
    }
}

//Extension for allowing conversion of Options into Results, with parser context information.
impl<T> ParserErrorContext<T> for Option<T> {
    fn ctx(self, parser: &Parser, msg: String) -> Result<T> {
        match self {
            Some(val) => Ok(val),
            None => Err(GenericParseError::new(parser, msg))?,
        }
    }
}
