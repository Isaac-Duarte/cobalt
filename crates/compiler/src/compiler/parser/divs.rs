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
    /// Statements within the procedure division.
    pub stats: Vec<Spanned<Stat<'src>>>,
}

impl<'src> Parser<'src> {
    /// Parses a procedure division from COBOL tokens.
    pub(super) fn proc_div(&mut self) -> Result<ProcDiv<'src>> {
        // Parse header.
        self.consume_vec(&[tok![proc_div], tok![.], tok![eol]])?;

        // Parse statements until we peek a "STOP RUN".
        let mut stats: Vec<Spanned<Stat<'src>>> = Vec::new();
        while self.peek() != tok![stop_run] {
            stats.push(self.stat()?);
        }

        // Consume the stop, EOF.
        // We optionally consume an EOL here too, since some operating systems (like Windows)
        // prefer to save files with a CRLF before the EOF marker.
        self.consume_vec(&[tok![stop_run], tok![.]])?;
        if self.peek() == tok![eol] {
            self.next()?;
        }
        self.consume(tok![eof])?;

        Ok(ProcDiv { stats })
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
