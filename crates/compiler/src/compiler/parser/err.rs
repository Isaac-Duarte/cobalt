/*
Definitions of all possible errors resulting from parser processing.
*/
use super::Parser;
use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

/// Represents a generic parser error stemming from a single token.
#[derive(Error, Debug, Diagnostic)]
#[error("{msg}")]
#[diagnostic(code(cobalt::parse_error))]
pub struct GenericParseError {
    /// The source the error stems from.
    #[source_code]
    src: NamedSource<String>,

    /// The parsing error message.
    msg: String,

    /// The span at which the error occurs.
    #[label("here")]
    span: Option<SourceSpan>,
}

impl GenericParseError {
    /// Creates a new generic parse error, given the current parser state.
    pub fn new(parser: &Parser, msg: String) -> GenericParseError {
        let span = match parser.cur() {
            Some(tok) => Some(tok.1),
            None => None,
        };
        Self {
            src: parser.get_named_source(),
            msg,
            span,
        }
    }

    /// Creates a new generic parse error with the given span.
    pub fn with_span(parser: &Parser, span: SourceSpan, msg: String) -> GenericParseError {
        Self {
            src: parser.get_named_source(),
            msg,
            span: Some(span),
        }
    }
}
