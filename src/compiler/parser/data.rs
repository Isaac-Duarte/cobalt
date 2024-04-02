use miette::Result;

use super::{err::GenericParseError, parser_bail, token::tok, Parser, ParserErrorContext};

/// Working storage section of a COBOL data division.
#[derive(Debug)]
pub(crate) struct WorkingStorageSec<'src> {
    /// Vector of all atomic (non-grouped) data items.
    pub elementary_data: Vec<ElementaryData<'src>>,
}

impl<'src> Parser<'src> {
    /// Parses a COBOL data division working storage section from the current point.
    pub(super) fn ws_section(&mut self) -> Result<WorkingStorageSec<'src>> {
        // Parse the header.
        self.consume_vec(&[tok![ws_section], tok![.], tok![eol]])?;

        // Parse a sequence of elementary data items.
        let mut elementary_data: Vec<ElementaryData<'src>> = Vec::new();
        while self.peek() == tok![int_lit] {
            elementary_data.push(self.wss_elem_data()?);
        }

        Ok(WorkingStorageSec { elementary_data })
    }
}

/// Description of a single elementary data item.
#[derive(Debug)]
pub(crate) struct ElementaryData<'src> {
    /// The name of the variable.
    pub name: &'src str,

    /// The PIC description of the variable.
    pub pic: Pic,
}

impl<'src> Parser<'src> {
    /// Parses a single elementary (atomic) data item from a COBOL working storage section.
    /// These must begin with the code "01".
    fn wss_elem_data(&mut self) -> Result<ElementaryData<'src>> {
        let code = self.next()?;
        if self.text(code) != "01" {
            parser_bail!(
                self,
                "Elementary data items must begin with the data type '01'."
            );
        }

        // Parse the name.
        let name_tok = self.consume(tok![ident])?;
        let name = self.text(name_tok);

        // Parse the PIC layout.
        self.consume(tok![pic])?;
        let pic_tok = self.consume(tok![pic_clause])?;
        let pic_parser = PicParser::new(self, self.text(pic_tok));
        let pic = pic_parser.parse()?;

        // Data elements must end with a ".", EOL.
        self.consume_vec(&[tok![.], tok![eol]])?;

        Ok(ElementaryData { name, pic })
    }
}

/// Represents a single "PIC" data layout description.
#[derive(Debug)]
pub(crate) struct Pic {
    /// The requested layout format, in chunks.
    pub layout_chunks: Vec<PicLayoutChunk>,

    /// The total length of this data layout, in bytes.
    pub byte_len: usize,
}

/// Represents a single chunk within a PIC layout.
#[derive(Debug)]
pub(crate) struct PicLayoutChunk {
    /// The length of the chunk.
    pub len: usize,

    /// The type of data to be stored here.
    pub chunk_type: PicChunkType,
}

/// Available types of layout chunks for PIC variables.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PicChunkType {
    Sign,
    ImplicitDecimalPoint,
    DecimalPoint,
    Numeric,
    Alpha,
    AlphaNumeric,
}

impl PicChunkType {
    /// Parses the relevant chunk type from a single character.
    pub fn from_char(parser: &mut Parser, c: char) -> Result<Self> {
        Ok(match c {
            '9' => PicChunkType::Numeric,
            'A' => PicChunkType::Alpha,
            'X' => PicChunkType::AlphaNumeric,
            'V' => PicChunkType::ImplicitDecimalPoint,
            'P' => PicChunkType::DecimalPoint,
            'S' => PicChunkType::Sign,
            c @ _ => parser_bail!(
                parser,
                "Invalid data type for chunk '{}'. Expected one of '9AXVSP'.",
                c
            ),
        })
    }
}

/// Mini-parser for interpreting PIC layout strings.
pub(super) struct PicParser<'src, 'prs> {
    /// The main parser.
    parser: &'prs mut Parser<'src>,

    /// The current generated layout chunks for the Pic.
    chunks: Vec<PicLayoutChunk>,

    /// The Pic string to be parsed.
    pic_str: &'src str,

    /// The current position within the Pic string.
    cur_pos: usize,
}

impl<'src, 'prs> PicParser<'src, 'prs> {
    /// Creates a new parser for the given PIC text.
    pub fn new(parser: &'prs mut Parser<'src>, txt: &'src str) -> Self {
        PicParser {
            parser,
            chunks: Vec::new(),
            pic_str: txt,
            cur_pos: 0,
        }
    }

    /// Attempts to parse the PIC text and construct a Pic.
    pub fn parse(mut self) -> Result<Pic> {
        // Parse all of the chunks individually.
        while self.cur_pos < self.pic_str.len() {
            let chunk = self.parse_chunk()?;
            self.chunks.push(chunk);
        }

        // Calculate the total byte length of the combined chunks.
        let mut byte_len = 0;
        for chunk in (&self.chunks).iter() {
            match chunk.chunk_type {
                PicChunkType::DecimalPoint
                | PicChunkType::Numeric
                | PicChunkType::Alpha
                | PicChunkType::AlphaNumeric
                | PicChunkType::Sign => {
                    byte_len += chunk.len;
                }

                // These types are implicit, and don't add any length.
                PicChunkType::ImplicitDecimalPoint => {}
            }
        }

        // Check the byte length is valid.
        if byte_len == 0 {
            parser_bail!(
                self.parser,
                "No data format chunks provided within PIC clause."
            );
        }

        // Sanity check the generated chunks.
        self.verify_chunks()?;

        Ok(Pic {
            byte_len,
            layout_chunks: self.chunks,
        })
    }

    /// Peeks the next character to parse.
    fn peek(&mut self) -> Option<char> {
        self.pic_str.chars().nth(self.cur_pos)
    }

    /// Grabs the next character to parse.
    fn next(&mut self) -> Result<char> {
        let char = match self.pic_str.chars().nth(self.cur_pos) {
            Some(char) => Ok(char),
            None => None.ctx(
                self.parser,
                "Failed to parse PIC string, found no following character.".into(),
            ),
        };
        self.cur_pos += 1;
        char
    }

    /// Parses a single PIC chunk from the current position.
    fn parse_chunk(&mut self) -> Result<PicLayoutChunk> {
        // Get the data type of this chunk.
        let next = self.next()?;
        let chunk_type = PicChunkType::from_char(&mut self.parser, next)?;

        // Parse subsequent identical sections until we reach a new type.
        // We also parse any following defined lengths, e.g. A(4), and add those.
        let mut len = 1;
        loop {
            let peek = self.peek();
            if peek.is_some_and(|c| c == '(') {
                self.next()?; // '('
                let mut num_str = String::new();
                while self.peek().is_some_and(|c| c != ')') {
                    let digit = self.next()?;
                    // Skip leading zeroes.
                    if digit == '0' && num_str.len() == 0 {
                        continue;
                    }
                    num_str.push(digit);
                }
                self.next()?; // ')'

                // Attempt to parse the length as a usize.
                let parsed_len = num_str.parse::<usize>().ctx(
                    self.parser,
                    format!("Failed to parse PIC chunk length '{}'", num_str),
                )?;

                // We always add one less here, since the character prior has already added
                // an initial length of 1.
                len += parsed_len - 1;
            } else if peek.is_some_and(|c| {
                PicChunkType::from_char(self.parser, c).is_ok_and(|ct| ct == chunk_type)
            }) {
                self.next()?;
                len += 1;
            } else {
                // The next section is either another type, or the end of the string.
                break;
            }
        }

        Ok(PicLayoutChunk { chunk_type, len })
    }

    /// Verifies that the currently generated chunks are a valid PIC configuration.
    fn verify_chunks(&mut self) -> Result<()> {
        // Check for chunks which should be unique.
        if (&self.chunks)
            .iter()
            .filter(|c| {
                c.chunk_type == PicChunkType::DecimalPoint
                    || c.chunk_type == PicChunkType::ImplicitDecimalPoint
            })
            .count()
            > 1
        {
            parser_bail!(
                self.parser,
                "Only one decimal point can be present in a PIC clause."
            );
        }

        // Check for invalid combinations of chunks.
        if (&self.chunks)
            .iter()
            .filter(|c| c.chunk_type == PicChunkType::Numeric)
            .count()
            > 0
            && (&self.chunks)
                .iter()
                .filter(|c| {
                    c.chunk_type == PicChunkType::Alpha
                        || c.chunk_type == PicChunkType::AlphaNumeric
                })
                .count()
                > 0
        {
            parser_bail!(
                self.parser,
                "Cannot mix numeric and alpha/alphanumeric terms in a PIC clause."
            );
        }

        // Check individual chunk data.
        for (idx, chunk) in (&self.chunks).iter().enumerate() {
            match chunk.chunk_type {
                PicChunkType::Sign => {
                    if idx != 0 {
                        parser_bail!(
                            self.parser,
                            "Sign data must come at the start of the PIC clause."
                        );
                    }
                    if chunk.len > 1 {
                        parser_bail!(
                            self.parser,
                            "Sign data cannot be longer than 1 character within a PIC clause."
                        );
                    }
                }
                PicChunkType::DecimalPoint | PicChunkType::ImplicitDecimalPoint => {
                    if chunk.len > 1 {
                        parser_bail!(
                            self.parser,
                            "Multiple decimal points cannot be used within a PIC clause."
                        );
                    }
                }
                // No checks for this type.
                _ => {}
            }
        }
        Ok(())
    }
}
