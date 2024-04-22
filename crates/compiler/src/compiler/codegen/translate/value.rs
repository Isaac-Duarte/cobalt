use std::{
    collections::HashMap,
    fmt::{Display, Write},
};

use cranelift::codegen::ir::{GlobalValue, Value};
use cranelift_module::DataId;
use miette::Result;

use crate::compiler::parser::Literal;

/// Represents a literal used only within the code generation stage.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum CodegenLiteral {
    Char(char),
}

impl Display for CodegenLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Char(c) => f.write_char(*c),
        }
    }
}

/// Collection of cached values used within a [`super::FuncTranslator`].
pub(super) struct ValueCache {
    /// Cache of global values loaded for the current function.
    gv_cache: HashMap<DataId, GlobalValue>,

    /// Cache of static pointers loaded for the current function.
    sptr_cache: HashMap<DataId, Value>,

    /// Cache of static literal values loaded for the current function.
    litv_cache: HashMap<Literal, Value>,

    /// Cache of codegen-only literal values from the code generator.
    cglit_cache: HashMap<CodegenLiteral, Value>,
}

impl ValueCache {
    /// Creates a new function value cache.
    pub fn new() -> Self {
        Self {
            gv_cache: HashMap::new(),
            sptr_cache: HashMap::new(),
            litv_cache: HashMap::new(),
            cglit_cache: HashMap::new(),
        }
    }

    /// Resets the current value cache, barring global values.
    /// Should be called on entry to a new block.
    pub fn begin_block(&mut self) {
        self.sptr_cache.clear();
        self.litv_cache.clear();
        self.cglit_cache.clear();
    }

    /// Searches the cache for a loaded [`GlobalValue`] for the given ID.
    pub fn get_gv(&self, data_id: &DataId) -> Option<GlobalValue> {
        self.gv_cache.get(data_id).copied()
    }

    /// Searches the cache for a static pointer [`Value`] for the given ID.
    pub fn get_static_ptr(&self, data_id: &DataId) -> Option<Value> {
        self.sptr_cache.get(data_id).copied()
    }

    /// Searches the cache for a loaded literal [`Value`] for the given ID.
    pub fn get_litv(&self, lit: &Literal) -> Option<Value> {
        self.litv_cache.get(lit).copied()
    }

    /// Searches the cache for a loaded [`CodegenLiteral`].
    pub fn get_cg_litv(&self, cglit: &CodegenLiteral) -> Option<Value> {
        self.cglit_cache.get(cglit).copied()
    }

    /// Inserts the given [`GlobalValue`] into cache. Throws an error if the data is already present.
    pub fn insert_gv(&mut self, data_id: &DataId, gv: GlobalValue) -> Result<()> {
        if Self::insert_item(&mut self.gv_cache, data_id, gv) {
            miette::bail!(
                "codegen: Duplicate entry for data ID '{}' in global value cache.",
                data_id
            );
        }
        Ok(())
    }

    /// Inserts the given static pointer [`Value`] into cache. Throws an error if the data is already present.
    pub fn insert_static_ptr(&mut self, data_id: &DataId, sptr: Value) -> Result<()> {
        if Self::insert_item(&mut self.sptr_cache, data_id, sptr) {
            miette::bail!(
                "codegen: Duplicate entry for static pointer ID '{}' in sptr cache.",
                data_id
            );
        }
        Ok(())
    }

    /// Inserts the given static literal [`Value`] into cache. Throws an error if the data is already present.
    pub fn insert_litv(&mut self, lit: &Literal, litv: Value) -> Result<()> {
        if Self::insert_item(&mut self.litv_cache, lit, litv) {
            miette::bail!(
                "codegen: Duplicate entry for literal '{}' in literal value cache.",
                lit
            );
        }
        Ok(())
    }

    /// Inserts the given codegen literal [`Value`] into cache. Throws an error if the data is already present.
    pub fn insert_cg_litv(&mut self, cglit: &CodegenLiteral, cglitv: Value) -> Result<()> {
        if Self::insert_item(&mut self.cglit_cache, cglit, cglitv) {
            miette::bail!(
                "codegen: Duplicate entry for codegen literal '{}' in cglitv cache.",
                cglit
            );
        }
        Ok(())
    }

    /// Inserts an entry into the given map, if not present. Returns whether the key was already present.
    fn insert_item<K: Copy + PartialEq + Eq + std::hash::Hash, V>(
        map: &mut HashMap<K, V>,
        key: &K,
        val: V,
    ) -> bool {
        if map.contains_key(key) {
            return true;
        }
        map.insert(*key, val);
        false
    }
}
