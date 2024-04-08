use super::Spanned;
use logos::Logos;
use std::fmt::Display;

// A macro for shortening references to all the token types.
#[rustfmt::skip]
macro_rules! tok {
    [ident_div] => { $crate::compiler::parser::Token::IdentDiv };
    [data_div] => { $crate::compiler::parser::Token::DataDiv };
    [proc_div] => { $crate::compiler::parser::Token::ProcDiv };
    [ws_section] => { $crate::compiler::parser::Token::WsSection };
    [pic] => { $crate::compiler::parser::Token::Pic };
    [value] => { $crate::compiler::parser::Token::Value };
    [comp] => { $crate::compiler::parser::Token::Comp };
    [program_id] => { $crate::compiler::parser::Token::ProgramId };
    [stop_run] => { $crate::compiler::parser::Token::StopRun };
    [display] => { $crate::compiler::parser::Token::Display };
    [add] => { $crate::compiler::parser::Token::Add };
    [subtract] => { $crate::compiler::parser::Token::Subtract };
    [multiply] => { $crate::compiler::parser::Token::Multiply };
    [divide] => { $crate::compiler::parser::Token::Divide };
    [move] => { $crate::compiler::parser::Token::Move };
    [to] => { $crate::compiler::parser::Token::To };
    [from] => { $crate::compiler::parser::Token::From };
    [by] => { $crate::compiler::parser::Token::By };
    [into] => { $crate::compiler::parser::Token::Into };
    [giving] => { $crate::compiler::parser::Token::Giving };
    [.] => { $crate::compiler::parser::Token::CtrlDot };
    [float_lit] => { $crate::compiler::parser::Token::FloatLiteral };
    [int_lit] => { $crate::compiler::parser::Token::IntLiteral };
    [str_literal] => { $crate::compiler::parser::Token::StringLiteral };
    [pic_clause] => { $crate::compiler::parser::Token::PicClause };
    [ident] => { $crate::compiler::parser::Token::Identifier };
    [eol] => { $crate::compiler::parser::Token::EOL };
    [eof] => { $crate::compiler::parser::Token::EOF };
}
pub(crate) use tok;

/// A lexer for COBOL tokens.
pub(crate) struct Lexer<'src> {
    input: &'src str,
    generated: logos::SpannedIter<'src, Token>,
    eof_reached: bool,
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        Self {
            input,
            generated: Token::lexer(input).spanned(),
            eof_reached: false,
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
    #[token("DATA DIVISION")]
    DataDiv,
    #[token("PROCEDURE DIVISION")]
    ProcDiv,
    #[token("WORKING-STORAGE SECTION")]
    WsSection,
    #[token("PIC")]
    Pic,
    #[token("VALUE")]
    Value,
    #[token("COMP")]
    Comp,
    #[token("PROGRAM-ID")]
    ProgramId,
    #[token("STOP RUN")]
    StopRun,
    #[token("DISPLAY")]
    Display,
    #[token("ADD")]
    Add,
    #[token("SUBTRACT")]
    Subtract,
    #[token("MULTIPLY")]
    Multiply,
    #[token("DIVIDE")]
    Divide,
    #[token("MOVE")]
    Move,
    #[token("TO", priority = 5)]
    To,
    #[token("FROM")]
    From,
    #[token("BY", priority = 5)]
    By,
    #[token("INTO")]
    Into,
    #[token("GIVING")]
    Giving,
    #[token(".")]
    CtrlDot,
    #[regex(r#"(-)?[0-9]+\.[0-9]+"#)]
    FloatLiteral,
    #[regex(r#"(-)?[0-9]+"#, priority = 5)]
    IntLiteral,
    #[regex(r#""((\[.])|[^\"])*""#)]
    #[regex(r#"'((\[.])|[^\'])*'"#)]
    StringLiteral,
    #[regex(r#"([9AXVSP](\([0-9]+\))?)+"#, priority = 3)]
    PicClause,
    // This is technically incorrect for now, but Logos doesn't like the correct regex of:
    // [A-Z0-9][A-Z0-9-]*[A-Z0-9]|[A-Z0-9]+
    #[regex(r#"[A-Z0-9][A-Z0-9-]+"#)]
    Identifier,
    #[regex(r"//[^\n]*[\n]*")]
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
            Token::DataDiv => write!(f, "DATA DIVISION"),
            Token::ProcDiv => write!(f, "PROCEDURE DIVISION"),
            Token::WsSection => write!(f, "WORKING-STORAGE SECTION"),
            Token::Pic => write!(f, "PIC"),
            Token::Value => write!(f, "VALUE"),
            Token::Comp => write!(f, "COMP"),
            Token::ProgramId => write!(f, "PROGRAM-ID"),
            Token::StopRun => write!(f, "STOP RUN"),
            Token::Display => write!(f, "DISPLAY"),
            Token::Add => write!(f, "ADD"),
            Token::Subtract => write!(f, "SUBTRACT"),
            Token::Multiply => write!(f, "MULTIPLY"),
            Token::Divide => write!(f, "DIVIDE"),
            Token::Move => write!(f, "MOVE"),
            Token::To => write!(f, "TO"),
            Token::From => write!(f, "FROM"),
            Token::By => write!(f, "BY"),
            Token::Into => write!(f, "INTO"),
            Token::Giving => write!(f, "GIVING"),
            Token::CtrlDot => write!(f, "."),
            Token::FloatLiteral => write!(f, "float-literal"),
            Token::IntLiteral => write!(f, "int-literal"),
            Token::StringLiteral => write!(f, "string-literal"),
            Token::PicClause => write!(f, "pic-clause"),
            Token::Identifier => write!(f, "identifier"),
            Token::EOL => write!(f, "EOL"),
            Token::EOF => write!(f, "EOF"),
            Token::Invalid => write!(f, "(unknown)"),
            Token::SingleLineComment | Token::Ignored => unreachable!(),
        }
    }
}
