use super::{data::WorkingStorageSec, stat::Stat, token::tok, Parser, Spanned};
use miette::Result;

////////////////////
// IDENT DIVISION //
////////////////////

/// The identification division of a single COBOL program.
#[derive(Debug)]
pub(crate) struct IdentDiv<'src> {
    /// The ID slug of the program.
    pub program_id: &'src str,
}

impl<'src> Parser<'src> {
    /// Parses an identification division from COBOL tokens.
    pub(super) fn ident_div(&mut self) -> Result<IdentDiv<'src>> {
        // Parse header.
        self.consume_vec(&[tok![ident_div], tok![.], tok![eol]])?;

        // Parse program ID statement.
        self.consume_vec(&[tok![program_id], tok![.]])?;
        let prog_id_tok = self.consume(tok![ident])?;
        let program_id = self.text(prog_id_tok);
        self.consume_vec(&[tok![.], tok![eol]])?;

        Ok(IdentDiv { program_id })
    }
}

///////////////////
// PROC DIVISION //
///////////////////

/// The procedure division of a single COBOL program.
#[derive(Debug)]
pub(crate) struct ProcDiv<'src> {
    /// Paragraphs within the procedure division.
    pub paragraphs: Vec<Paragraph<'src>>,
}

/// Represents a single paragraph within a COBOL program.
#[derive(Debug)]
pub(crate) struct Paragraph<'src> {
    /// The name of this paragraph.
    /// If this is an anonymous entrypoint paragraph, the value is [`None`].
    pub name: Option<Spanned<&'src str>>,

    /// The statements contained within the paragraph.
    pub stats: Vec<Spanned<Stat<'src>>>,

    /// Whether this paragraph terminates the program.
    pub terminates: bool,
}

impl<'src> Parser<'src> {
    /// Parses a procedure division from COBOL tokens.
    pub(super) fn proc_div(&mut self) -> Result<ProcDiv<'src>> {
        // Parse header.
        self.consume_vec(&[tok![proc_div], tok![.], tok![eol]])?;

        // Parse statements until we peek the end of the file.
        let mut paragraphs: Vec<Paragraph<'src>> = Vec::new();
        while self.peek() != tok![eol] && self.peek() != tok![eof] {
            paragraphs.push(self.paragraph()?);
        }

        // Consume the EOF.
        // We optionally consume an EOL here too, since some operating systems (like Windows)
        // prefer to save files with a CRLF before the EOF marker.
        if self.peek() == tok![eol] {
            self.next()?;
        }
        self.consume(tok![eof])?;

        Ok(ProcDiv { paragraphs })
    }

    /// Parses a single paragraph from the current position.
    fn paragraph(&mut self) -> Result<Paragraph<'src>> {
        // If this paragraph has a name, parse that out.
        let name = if self.peek() == tok![ident] {
            let name_tok = self.next()?;
            self.consume_vec(&[tok![.], tok![eol]])?;
            Some((self.text(name_tok), name_tok.1))
        } else {
            None
        };

        // Keep parsing statements until we hit the end. There must be at least 1 statement per paragraph.
        let mut stats: Vec<Spanned<Stat<'src>>> = Vec::new();
        loop {
            stats.push(self.stat()?);
            if self.peek() == tok![stop_run] || self.peek() == tok![ident] || self.peek() == tok![eof] {
                break;
            }
        }

        // If there's a STOP RUN statement, consume it.
        let terminates = if self.peek() == tok![stop_run] {
            self.consume_vec(&[tok![stop_run], tok![.]])?;
            if self.peek() == tok![eol] {
                self.next()?;
            }
            true
        } else {
            false
        };

        Ok(Paragraph {
            name,
            stats,
            terminates
        })
    }
}

///////////////////
// DATA DIVISION //
///////////////////

/// The data division of a single COBOL program.
#[derive(Debug)]
pub(crate) struct DataDiv<'src> {
    /// Working storage section, where runtime-use variables are declared.
    pub ws_section: WorkingStorageSec<'src>,
}

impl<'src> Parser<'src> {
    /// Parses a procedure division from COBOL tokens.
    pub(super) fn data_div(&mut self) -> Result<DataDiv<'src>> {
        // Consume header.
        self.consume_vec(&[tok![data_div], tok![.], tok![eol]])?;

        // Working storage section.
        let ws_section = self.ws_section()?;

        Ok(DataDiv { ws_section })
    }
}
