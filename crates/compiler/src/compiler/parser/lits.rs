use std::{fmt::Display, hash::Hash};

use bimap::BiMap;
use miette::Result;

use super::{
    parser_bail,
    token::{tok, Token},
    Parser, ParserErrorContext,
};

/// ID for a single string literal within the AST.
pub type StrLitId = usize;

/// Store for string literals referenced throughout the AST.
/// Used to centralise string literal data for easy access during code generation.
#[derive(Debug)]
pub(crate) struct StrLitStore {
    /// Map of all string literals which are stored in the output program's `.rodata`.
    lit_map: BiMap<StrLitId, String>,

    /// Map of all string literals which are present in the AST, but require no storage
    /// in the output program's `.rodata` section.
    transient_map: BiMap<StrLitId, String>,

    /// The current ID counter.
    /// This must be globally unique across both arrays, to avoid ID collisions.
    cur_id: StrLitId,
}

impl StrLitStore {
    /// Creates a new string literal store.
    pub fn new() -> Self {
        Self {
            lit_map: BiMap::new(),
            transient_map: BiMap::new(),
            cur_id: 0,
        }
    }

    /// Returns a reference to the list of stored string literals to be output to the
    /// program's `.rodata` section.
    pub fn stored_lits(&self) -> &BiMap<StrLitId, String> {
        &self.lit_map
    }

    /// Inserts a new stored literal into the string literal store, returning an ID.
    /// If the literal is already present, returns the existing ID.
    pub fn insert(&mut self, val: String) -> StrLitId {
        if self.lit_map.contains_right(&val) {
            return *self.lit_map.get_by_right(&val).unwrap();
        }
        let id = self.cur_id;
        self.lit_map.insert(id, val);
        self.cur_id += 1;
        id
    }

    /// Inserts a new transient (AST-only) literal into the string literal store, returning
    /// an ID. If the literal is already present, returns the existing ID.
    pub fn insert_transient(&mut self, val: String) -> StrLitId {
        if self.transient_map.contains_right(&val) {
            return *self.transient_map.get_by_right(&val).unwrap();
        }
        let id = self.cur_id;
        self.transient_map.insert(id, val);
        self.cur_id += 1;
        id
    }

    /// Returns the stored string literal associated with the given ID.
    pub fn get(&self, lit_id: StrLitId) -> Option<&String> {
        match self.lit_map.get_by_left(&lit_id) {
            Some(x) => Some(x),
            None => self.transient_map.get_by_left(&lit_id),
        }
    }
}

/// A single generic literal within a COBOL AST.
#[derive(Debug, Clone, Copy)]
pub(crate) enum Literal {
    String(StrLitId),
    Int(i64),
    Float(f64),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(sid) => f.write_fmt(format_args!("str_{}", sid)),
            Self::Int(i) => f.write_fmt(format_args!("{}", i)),
            Self::Float(fl) => f.write_fmt(format_args!("{}", fl)),
        }
    }
}

impl Hash for Literal {
    /// Hashes the literal. Required for HashMap, etc.
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Literal::String(sid) => {
                state.write_u8(1);
                sid.hash(state);
            }
            Literal::Int(i) => {
                state.write_u8(2);
                i.hash(state);
            }
            Literal::Float(f) => {
                state.write_u8(3);
                // Yes, this is bad and jank.
                // It doesn't really matter for us though, since we only use this hash for caching purposes,
                // so all we're really leaking is another (very similar) entry in cache.
                for b in f.to_ne_bytes() {
                    state.write_u8(b);
                }
            }
        }
    }
}

impl Eq for Literal {}

impl PartialEq for Literal {
    /// Returns whether this literal matches the other provided literal.
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl Literal {
    /// Extracts a text version of this literal, for display to users.
    pub fn text(&self, str_lits: &StrLitStore) -> String {
        match self {
            Self::String(sid) => str_lits.get(*sid).unwrap().clone(),
            Self::Int(i) => i.to_string(),
            Self::Float(f) => f.to_string(),
        }
    }
}

impl<'src> Parser<'src> {
    /// Parses a single literal from the current position.
    /// Assumes all string literals found are stored, not transient (AST-only).
    pub(super) fn literal(&mut self) -> Result<Literal> {
        match self.peek() {
            Token::StringLiteral => {
                let text = self.consume_str()?;
                let id = self.str_lits.insert(text);
                Ok(Literal::String(id))
            }
            Token::IntLiteral => {
                let int = self.consume_int()?;
                Ok(Literal::Int(int))
            }
            Token::FloatLiteral => {
                let float = self.consume_float()?;
                Ok(Literal::Float(float))
            }
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
