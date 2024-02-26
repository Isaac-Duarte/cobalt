use super::Span;
use chumsky::prelude::*;

/// Represents a single token within the Cobalt parser.
#[derive(Clone, Debug, PartialEq)]
pub(super) enum Token<'src> {
    // Generic structures.
    Ident(&'src str),
    Str(&'src str),
    Ctrl(char),

    // Reserved keywords.
    IdentificationDiv, // IDENTIFICATION DIVISION
    ProcedureDiv,      // PROCEDURE DIVISION
    ProgramId,         // PROGRAM-ID
    Display,           // DISPLAY
    StopRun,           // STOP RUN
}

impl<'src> Token<'src> {
    pub fn unwrap_str(&self) -> &'src str {
        if let Token::Str(txt) = self {
            txt
        } else {
            panic!("attempted to unwrap {:#?} as Token::Str.", self);
        }
    }

    pub fn unwrap_ident(&self) -> &'src str {
        if let Token::Ident(txt) = self {
            txt
        } else {
            panic!("attempted to unwrap {:#?} as Token::Ident.", self);
        }
    }
}

/// Returns a lexer which accepts a string of COBOL tokens.
pub(super) fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>, extra::Err<Rich<'src, char, Span>>> {
    // Reserved words.
    let ident_div = just("IDENTIFICATION DIVISION").to(Token::IdentificationDiv);
    let procedure_div = just("PROCEDURE DIVISION").to(Token::ProcedureDiv);
    let stop_run = just("STOP RUN").to(Token::StopRun);

    // Identifiers.
    // This is technically incorrect, but fine for now, since we need LL(k) lookahead for correctness...
    let ident = any().filter(char::is_ascii_alphanumeric)
        .then(any().filter(|c| char::is_ascii_alphanumeric(c) || *c == '-').repeated())
        .to_slice()
        .map(|ident: &str| match ident {
            "PROGRAM-ID" => Token::ProgramId,
            "DISPLAY" => Token::Display,
            _ => Token::Ident(ident)
        });

    // String literals.
    let single_quote_str = just("'")
        .ignore_then(none_of("'").repeated())
        .then_ignore(just("'"))
        .to_slice();
    let dbl_quote_str = just('"')
        .ignore_then(none_of('"').repeated())
        .then_ignore(just('"'))
        .to_slice();
    let string = single_quote_str
        .or(dbl_quote_str)
        .map(Token::Str);

    // Control characters, endln.
    let ctrl = one_of(".").map(Token::Ctrl);

    // Create combined parser for all tokens.
    let token = choice((
        ident_div,
        procedure_div,
        stop_run,
        ident,
        string,
        ctrl
    )).padded();

    // Collect all tokens.
    token
        .map_with(|tok, e| (tok, e.span()))
        .repeated()
        .collect()
        .then_ignore(end())
}
