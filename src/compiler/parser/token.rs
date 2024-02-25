use chumsky::prelude::*;
use super::Span;

/// Represents a single token within the Cobalt parser.
#[derive(Clone, Debug, PartialEq)]
pub(super) enum Token<'src> {
    // Generic structures.
    Str(&'src str),
    Ctrl(char),

    // Reserved keywords.
    IdentificationDiv, // IDENTIFICATION DIVISION
    ProcedureDiv, // PROCEDURE DIVISION
    ProgramId, // PROGRAM-ID
    Display, // DISPLAY
    StopRun // STOP RUN
}

/// Returns a lexer which accepts a string of COBOL tokens.
pub(super) fn lexer<'src>() -> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>, extra::Err<Rich<'src, char, Span>>> {
    // Reserved words.
    let test = just("test")
        .to_slice()
        .map(Token::Str);
    let test2 = just("test2")
        .to_slice()
        .map(Token::Str);

    let token = test.or(test2);
    token.map_with(|tok, e| (tok, e.span())).repeated().collect()
}