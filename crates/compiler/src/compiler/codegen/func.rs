use std::collections::HashMap;

use cranelift::codegen::ir::{FuncRef, Function};
use cranelift_module::{FuncId, Linkage, Module};
use cranelift_object::ObjectModule;
use miette::Result;

/// Manages mapping of COBOL paragraphs into their appropriate function symbols and the
/// conversion of those into Cranelift function references.
pub(super) struct FuncManager {
    /// The name of the entrypoint paragraph for this program.
    /// If the entrypoint is an anonymous paragraph, this value is [`None`].
    entrypoint: Option<String>,

    /// The [`FuncId`] of the entrypoint.
    /// If the entrypoint has yet to be defined, this value is [`None`].
    entrypoint_id: Option<FuncId>,

    /// A map of paragraph names to their relevant [`FuncId`].
    func_map: HashMap<String, FuncId>,

    /// Function-level [`FuncRef`] data for imported functions.
    /// Must be reset per-function with [`FuncManager::clear_refs()`].
    ref_map: HashMap<String, FuncRef>,
}

impl FuncManager {
    /// Creates a new function manager.
    pub fn new() -> Self {
        Self {
            entrypoint: None,
            entrypoint_id: None,
            func_map: HashMap::new(),
            ref_map: HashMap::new(),
        }
    }

    /// Creates a new function within the function manager for the given paragraph name.
    /// If no name is provided, the paragraph *must* be the entrypoint for the program.
    pub fn create_fn(
        &mut self,
        module: &mut ObjectModule,
        name: Option<&str>,
        is_entrypoint: bool,
    ) -> Result<()> {
        if name.is_none() {
            assert!(is_entrypoint);
        }
        if self.entrypoint_id.as_ref().is_some_and(|_| is_entrypoint) {
            panic!(
                "Entrypoint paragraph defined twice, first as '{:?}' then as '{:?}'.",
                self.entrypoint, name
            );
        }

        // Ensure there isn't already a function with the same name.
        if let Some(name) = name {
            if self.func_map.contains_key(name) {
                miette::bail!("Duplicate paragraphs defined with the symbol '{}'.", name);
            }
        }

        // Determine the details for defining the function.
        // If this is the entrypoint, it may not have a name, so map one.
        let symbol = name.unwrap_or("cobalt::entrypoint");
        let sig = module.make_signature();

        // Create the function within the module, register it.
        let func_id = module
            .declare_function(symbol, Linkage::Local, &sig)
            .map_err(|err| miette::diagnostic!("Failed to declare function '{symbol}': {err}"))?;

        // Save the entrypoint info, if this is the entrypoint.
        if is_entrypoint {
            self.entrypoint = name.map(|s| s.to_string());
            self.entrypoint_id = Some(func_id);
        }

        // Insert the function information into the map, if it has a name.
        // If it's anonymous, we don't care about saving a reference to it since it's the entrypoint already.
        if let Some(name) = name {
            self.func_map.insert(name.to_string(), func_id);
        }
        Ok(())
    }

    /// Returns the [`FuncId`] for the entrypoint.
    pub fn get_entrypoint_id(&self) -> Result<FuncId> {
        self.entrypoint_id.as_ref().map(|fi| *fi).ok_or(
            miette::diagnostic!(
                "Failed to fetch function information for entrypoint, not yet defined."
            )
            .into(),
        )
    }

    /// Returns the relevant [`FuncId`] for the given paragraph, if present.
    /// If not present, returns an error.
    pub fn get_id(&self, name: &str) -> Result<FuncId> {
        self.func_map.get(name).copied().ok_or(
            miette::diagnostic!("Failed to find function information for paragraph '{name}'.")
                .into(),
        )
    }

    /// Returns a [`FuncRef`] for the entrypoint. This function does not utilise cache.
    pub fn get_entrypoint_ref(
        &mut self,
        module: &mut ObjectModule,
        func: &mut Function,
    ) -> Result<FuncRef> {
        let ep_id = self.get_entrypoint_id()?;
        Ok(module.declare_func_in_func(ep_id, func))
    }

    /// Retrieves a [`FuncRef`] for the paragraph of the given name, using cache when available.
    /// If a paragraph with that name has not been registered, returns an error.
    pub fn get_ref(
        &mut self,
        module: &mut ObjectModule,
        func: &mut Function,
        name: &str,
    ) -> Result<FuncRef> {
        if self.ref_map.contains_key(name) {
            return Ok(*self.ref_map.get(name).unwrap());
        }
        let func_id = self.get_id(name)?;
        let fn_ref = module.declare_func_in_func(func_id, func);
        self.ref_map.insert(name.to_string(), fn_ref);
        Ok(fn_ref)
    }

    /// Clears existing [`FuncRef`] data from this function.
    /// Should be called at the beginning of each new function translation.
    pub fn clear_refs(&mut self) {
        self.ref_map.clear()
    }
}
