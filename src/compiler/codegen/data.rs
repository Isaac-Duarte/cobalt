use std::collections::HashMap;

use bimap::BiMap;
use cranelift_module::{DataDescription, DataId, Module};
use cranelift_object::ObjectModule;
use miette::Result;

use crate::compiler::parser::{Ast, LiteralId};

/**
 * Structures and utilities for configuring the static data layout
 * within output Cranelift objects.
 */

/// Manages static, runtime variable and file data within the code
/// generator, mapping definitions from [`crate::compiler::parser::Ast`] to
/// appropriate Cranelift definitions.
pub(super) struct DataManager {
    /// A map of all string literals within the program to a Cranelift data symbol.
    str_lit_map: HashMap<LiteralId, DataId>,
}

impl DataManager {
    /// Creates a new data manager.
    pub(super) fn new() -> Self {
        DataManager {
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
        self.upload_str_lits(module, &ast.str_lits)?;
        Ok(())
    }

    /// Returns the Cranelift [`DataId`] associated with the given [`LiteralId`].
    pub(super) fn str_data_id(&self, lit_id: LiteralId) -> Option<DataId> {
        self.str_lit_map.get(&lit_id).map(|o| *o)
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
