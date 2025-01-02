use cranelift::prelude::*;
use std::collections::HashMap;

pub struct BlockManager {
    block_map: HashMap<String, Block>,
}

impl BlockManager {
    pub fn new() -> Self {
        Self {
            block_map: HashMap::new(),
        }
    }

    /// Get or create a block for the given paragraph name.
    pub fn get_or_create_block(&mut self, builder: &mut FunctionBuilder, para_name: &str) -> Block {
        *self
            .block_map
            .entry(para_name.to_string())
            .or_insert_with(|| builder.create_block())
    }

    /// Get a block for the given paragraph name, if it exists.
    pub fn get_block(&self, para_name: &str) -> Option<Block> {
        self.block_map.get(para_name).copied()
    }

    /// Insert a pre-created block for a paragraph.
    pub fn insert_block(&mut self, para_name: &str, block: Block) {
        self.block_map.insert(para_name.to_string(), block);
    }
}
