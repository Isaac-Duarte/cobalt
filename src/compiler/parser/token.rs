use super::Spanned;
use logos::Logos;
use std::fmt::Display;

// A macro for shortening references to all the token types.
#[rustfmt::skip]
macro_rules! tok {
    [ident_div] => { $crate::compiler::parser::Token::IdentDiv };
    [proc_div] => { $crate::compiler::parser::Token::ProcDiv };
    [program_id] => { $crate::compiler::parser::Token::ProgramId };
    [stop_run] => { $crate::compiler::parser::Token::StopRun };
    [display] => { $crate::compiler::parser::Token::Display };
    [.] => { $crate::compiler::parser::Token::CtrlDot };
    [str_literal] => { $crate::compiler::parser::Token::StringLiteral };
    [ident] => { $crate::compiler::parser::Token::Identifier };
    [eol] => { $crate::compiler::parser::Token::EOL };
    [eof] => { $crate::compiler::parser::Token::EOF };
}
pub(crate) use tok;

/// A lexer for COBOL tokens.
pub(crate) struct Lexer<'src> {
    input: &'src str,
    generated: logos::SpannedIter<'src, Token>,
    eof_reached: bool
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        Self {
            input,
            generated: Token::lexer(input).spanned(),
            eof_reached: false
        }
    }
}

/// Allows for iterating over the lexer's tokens.
impl<'src> Iterator for Lexer<'src> {
    type Item = Spanned<Token>;

    /// Returns the next token as a spanned object.
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.generated.next() {
                Some((kind, span)) => {
                    let kind = kind.unwrap_or(Token::Invalid);

                    //If we've found either type of comment, we skip to the next token.
                    if kind == Token::SingleLineComment {
                        continue;
                    }

                    //Not a comment, we can safely return the token.
                    return Some((kind, span.into()));
                }

                // No more tokens, EOF already reached, exit.
                None if self.eof_reached => return None,

                // End of tokens reached, return EOF token.
                None => {
                    self.eof_reached = true;
                    let eof_span = self.input.len()..self.input.len();
                    return Some((tok![eof], eof_span.into()));
                }
            }
        }
    }
}

/// Represents a single COBOL token.
#[derive(Logos, Debug, Copy, Clone, PartialEq)]
pub(crate) enum Token {
    #[token("IDENTIFICATION DIVISION")]
    IdentDiv,
    #[token("PROCEDURE DIVISION")]
    ProcDiv,
    #[token("PROGRAM-ID")]
    ProgramId,
    #[token("STOP RUN")]
    StopRun,
    #[token("DISPLAY")]
    Display,
    #[token(".")]
    CtrlDot,
    #[regex(r#""((\[.])|[^\"])*""#)]
    #[regex(r#"'((\[.])|[^\'])*'"#)]
    StringLiteral,
    #[regex(r#"([A-Z0-9]+(-)?)*[A-Z0-9]+"#)]
    Identifier,
    #[regex(r"//[^\n]*")]
    SingleLineComment,
    #[regex(r"((\r)?\n)+")]
    EOL,
    #[regex(r"[ \t\f]+", logos::skip)]
    Ignored,
    Invalid,
    EOF,
}

/// Display formatting for all token types.
impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::IdentDiv => write!(f, "IDENTIFICATION DIVISION"),
            Token::ProcDiv => write!(f, "PROCEDURE DIVISION"),
            Token::ProgramId => write!(f, "PROGRAM-ID"),
            Token::StopRun => write!(f, "STOP RUN"),
            Token::Display => write!(f, "DISPLAY"),
            Token::CtrlDot => write!(f, "."),
            Token::StringLiteral => write!(f, "string-literal"),
            Token::Identifier => write!(f, "identifier"),
            Token::EOL => write!(f, "EOL"),
            Token::EOF => write!(f, "EOF"),
            Token::Invalid => write!(f, "(unknown)"),
            Token::SingleLineComment | Token::Ignored => unreachable!()
        }
    }
}