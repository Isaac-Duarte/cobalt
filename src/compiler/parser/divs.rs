use chumsky::prelude::*;

use super::{stat::{stat, Stat}, token::Token, ParserInput, Span, Spanned};

/// The identification division of a single COBOL program.
#[derive(Debug)]
pub(crate) struct IdentDiv<'src> {
    /// The ID slug of the program.
    pub program_id: &'src str
}

/// The procedure division of a single COBOL program.
#[derive(Debug)]
pub(crate) struct ProcDiv<'src> {
    /// Statements within the procedure division.
    pub stats: Vec<Spanned<Stat<'src>>>
}

/// Parser for the identity division within a COBOL program.
pub(super) fn ident_div<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    IdentDiv<'src>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    let program_id = just(Token::ProgramId)
        .then_ignore(just(Token::Ctrl('.')))
        .then(any().filter(|t| matches!(t, Token::Ident(_))))
        .then_ignore(just(Token::Ctrl('.')))
        .map(|(_, txt)| IdentDiv { program_id: txt.unwrap_ident() });

    let ident_div = just([Token::IdentificationDiv, Token::Ctrl('.')])
        .then(program_id)
        .map(|(_, pid)| pid);

    ident_div
}

/// Parser for a procedure division within a COBOL program.
pub(super) fn proc_div<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    ProcDiv<'src>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    let proc_div = just([Token::ProcedureDiv, Token::Ctrl('.')])
        .then(stat().repeated().collect::<Vec<_>>())
        .then_ignore(just([Token::StopRun, Token::Ctrl('.')]))
        .map(|(_, c)| ProcDiv { stats: c });

    proc_div
}