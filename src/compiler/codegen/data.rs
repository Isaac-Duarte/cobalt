use std::collections::HashMap;

use bimap::BiMap;
use cranelift_module::{DataDescription, DataId, Module};
use cranelift_object::ObjectModule;
use miette::Result;

use crate::compiler::parser::LiteralId;

/**
 * Structures and utilities for configuring the static data layout
 * within output Cranelift objects.
 */

/// Uploads the set of string literals used within the program to the object file.
/// Returns a table of the relevant Cranelift DataIds for all literals.
pub(super) fn upload_literals(
    module: &mut ObjectModule,
    literals: &BiMap<LiteralId, String>,
) -> Result<HashMap<LiteralId, DataId>> {
    let mut ids: HashMap<LiteralId, DataId> = HashMap::new();
    let mut desc = DataDescription::new();
    for (lit_id, literal) in literals.iter() {
        // Declare string literals as anonymous, unwriteable & non thread-local.
        let data_id = module
            .declare_anonymous_data(false, false)
            .expect("Failed to declare literal data.");
        desc.clear();
        let literal_bytes = literal.clone().into_bytes().into_boxed_slice();
        desc.define(literal_bytes);
        module
            .define_data(data_id, &desc)
            .expect("Failed to define literal data bytes.");

        ids.insert(*lit_id, data_id);
    }
    Ok(ids)
}
