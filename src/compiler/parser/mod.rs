/**
 * Structures and utilities for parsing a COBOL AST.
 */
use std::iter::Peekable;

use self::token::{tok, Lexer, Token};
use crate::compiler::parser::err::GenericParseError;
use bimap::BiMap;
use miette::{NamedSource, Result, SourceSpan};

mod ast;
mod data;
mod divs;
mod err;
mod stat;
mod token;

//Macro for exiting with a given parser error message.
macro_rules! parser_bail {
    ($parser:expr, $msg:tt) => {{
        return Err(GenericParseError::new($parser, format!($msg)))?
    }};

    ($parser:expr, $msg:tt, $($arg:tt)*) => {{
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
pub type LiteralId = usize;

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

    /// Map of literal IDs to string literals created by this parser.
    /// This is required as we need some access to a global list of string
    /// literals in order to determine the strings to store in `.data` later on.
    literal_map: BiMap<LiteralId, String>,
}

impl<'src> Parser<'src> {
    /// Creates a new parser for a given compile unit.
    pub fn new(cu_name: &'src str, input: &'src str) -> Parser<'src> {
        Self {
            input,
            cu_name,
            tokens: Lexer::new(input).peekable(),
            cur: None,
            literal_map: BiMap::new(),
        }
    }

    /// Performs a full parse of the compile unit, returning an AST and literal map.
    pub fn parse(mut self) -> Result<(Ast<'src>, BiMap<LiteralId, String>)> {
        let ast = self.ast()?;
        let literals = self.literal_map;
        Ok((ast, literals))
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

    /// Expects the next token to be a non-padded integer literal, consumes it, and returns a
    /// parsed version of the integer contained within. Must fit within a [`usize`], otherwise
    /// returns an error. Similarly errors on an invalid integer value.
    pub fn consume_int(&mut self) -> Result<usize> {
        let lit_tok = self.consume(tok![int_lit])?;
        let txt = self.text(lit_tok);
        txt.parse::<usize>()
            .ctx(self, format!("Failed to parse integer literal: {}", txt))
    }

    /// Inserts the given string literal into the literal table.
    fn insert_literal(&mut self, val: String) -> LiteralId {
        if self.literal_map.contains_right(&val) {
            return *self.literal_map.get_by_right(&val).unwrap();
        }
        let id = self.literal_map.len();
        self.literal_map.insert(id, val);
        id
    }

    //Returns the source code being parsed as a NamedSource.
    pub fn get_named_source(&self) -> NamedSource<String> {
        NamedSource::new(self.cu_name.to_string(), self.input.to_string())
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