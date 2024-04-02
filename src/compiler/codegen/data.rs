use std::collections::HashMap;

use bimap::BiMap;
use cranelift_module::{DataDescription, DataId, Module};
use cranelift_object::ObjectModule;
use miette::Result;

use crate::compiler::parser::{Ast, DataDiv, LiteralId, Pic};

/**
 * Structures and utilities for configuring the static data layout
 * within output Cranelift objects.
 */

/// Manages static, runtime variable and file data within the code
/// generator, mapping definitions from [`crate::compiler::parser::Ast`] to
/// appropriate Cranelift definitions.
pub(super) struct DataManager {
    /// A map of all symbols for variables within the object to their Cranelift
    /// data IDs, layouts.
    sym_map: HashMap<String, (DataId, Pic)>,

    /// A map of all string literals within the program to a Cranelift data symbol.
    str_lit_map: HashMap<LiteralId, DataId>,
}

impl DataManager {
    /// Creates a new data manager.
    pub(super) fn new() -> Self {
        DataManager {
            sym_map: HashMap::new(),
            str_lit_map: HashMap::new(),
        }
    }

    /// Uploads all string literals, runtime variable data and other object data found
    /// within the AST as Cranelift data objects, registering them in the manager.
    pub(super) fn upload<'src>(
        &mut self,
        module: &mut ObjectModule,
        ast: &Ast<'src>,
    ) -> Result<()> {
        if let Some(data_div) = ast.data_div.as_ref() {
            self.upload_vars(module, data_div)?;
        }
        self.upload_str_lits(module, &ast.str_lits)?;
        Ok(())
    }

    /// Returns the Cranelift [`DataId`] associated with the given COBOL symbol.
    pub(super) fn sym_data_id<'a>(&self, sym: &'a str) -> Option<DataId> {
        self.sym_map.get(sym).map(|o| o.0)
    }

    /// Returns the [`Pic`] layout associated with the given COBOL symbol.
    pub(super) fn sym_pic<'a>(&self, sym: &'a str) -> Option<&Pic> {
        self.sym_map.get(sym).map(|o| &o.1)
    }

    /// Returns the Cranelift [`DataId`] associated with the given [`LiteralId`].
    pub(super) fn str_data_id(&self, lit_id: LiteralId) -> Option<DataId> {
        self.str_lit_map.get(&lit_id).map(|o| *o)
    }

    /// Uploads variables present in the data division to the object file, registering them
    /// in the data manager's symbol table.
    fn upload_vars<'src>(
        &mut self,
        module: &mut ObjectModule,
        data_div: &DataDiv<'src>,
    ) -> Result<()> {
        let mut desc = DataDescription::new();
        for elem_var in data_div.ws_section.elementary_data.iter() {
            // Declare symbol data within module.
            let data_id = module
                .declare_data(elem_var.name, cranelift_module::Linkage::Local, true, false)
                .map_err(|err| miette::diagnostic!("Failed to declare data for symbol '{}': {}", elem_var.name, err))?;
            desc.clear();

            // For now, no initial values, so just declare as zeroes.
            let mut init_data: Vec<u8> = Vec::new();
            for _ in 0..elem_var.pic.byte_len {
                init_data.push(0x0);
            }

            // Define the data within the object.
            desc.define(init_data.into_boxed_slice());
            module.define_data(data_id, &desc).map_err(|err| {
                miette::diagnostic!("Failed to define data for symbol '{}': {}", elem_var.name, err)
            })?;

            // Register this symbol in the symbol map.
            self.sym_map.insert(elem_var.name.into(), (data_id, elem_var.pic.clone()));
        }
        
        Ok(())
    }

    /// Uploads the set of string literals used within the program to the object file.
    /// Registers all string literals within the object manager as offsets in a data block.
    fn upload_str_lits(
        &mut self,
        module: &mut ObjectModule,
        literals: &BiMap<LiteralId, String>,
    ) -> Result<()> {
        let mut desc = DataDescription::new();
        for (lit_id, literal) in literals.iter() {
            // Declare string literals as anonymous, unwriteable & non thread-local.
            let data_id = module
                .declare_anonymous_data(false, false)
                .map_err(|err| miette::diagnostic!("Failed to declare literal data: {}", err))?;
            desc.clear();

            // Get the bytes for this literal string.
            // Since this isn't zero-terminated by default, we do that here.
            let mut literal_bytes = literal.clone().into_bytes();
            literal_bytes.push(0x0);

            // Define the data description, data within the module.
            desc.define(literal_bytes.into_boxed_slice());
            module.define_data(data_id, &desc).map_err(|err| {
                miette::diagnostic!("Failed to define literal data bytes: {}", err)
            })?;
            self.str_lit_map.insert(*lit_id, data_id);
        }

        Ok(())
    }
}
