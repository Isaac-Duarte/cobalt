use miette::Result;

use crate::compiler::parser::{Literal, Value};

use super::data::DataManager;

/// Codegen-related utility functions for [`Value`] structs from the parser.
impl<'src> Value<'src> {
    /// Returns whether this value is a string or not.
    pub(super) fn is_str(&self, dm: &DataManager) -> Result<bool> {
        match self {
            Value::Variable(sym) => Ok(dm.sym_pic(sym)?.is_str()),
            Value::Literal(lit) => Ok(match lit {
                Literal::String(_) => true,
                _ => false,
            }),
        }
    }

    /// Returns whether this value is a float or not.
    pub(super) fn is_float(&self, dm: &DataManager) -> Result<bool> {
        match self {
            Value::Variable(sym) => Ok(dm.sym_pic(sym)?.is_float()),
            Value::Literal(lit) => Ok(match lit {
                Literal::Float(_) => true,
                _ => false,
            }),
        }
    }
}
