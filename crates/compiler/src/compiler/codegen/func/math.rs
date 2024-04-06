use cranelift::codegen::ir::{immediates::Offset32, types, InstBuilder, MemFlags, Value};
use miette::Result;

use crate::compiler::parser::{self, AddData};

use super::FuncTranslator;

impl<'src> FuncTranslator<'src> {
    /// Generates Cranelift IR for a single "ADD" statement.
    pub(super) fn translate_add(&mut self, add_data: &AddData<'src>) -> Result<()> {
        // Verify this add instruction is sane.
        self.verify_add(add_data)?;

        // First, load all the sources.
        let mut src_vals: Vec<Value> = Vec::new();
        for src in add_data.sources.iter() {
            src_vals.push(self.load_value(src)?);
        }

        // If any of the destinations require float output, get a set of converted sources
        // to float values only.
        let float_outputs_exist = add_data
            .dests
            .iter()
            .map(|sym| self.data.sym_pic(sym).is_ok_and(|pic| pic.is_float()))
            .any(|b| b);
        let float_srcs = if float_outputs_exist {
            let mut out: Vec<Value> = Vec::new();
            for (i, val) in src_vals.iter().enumerate() {
                if !add_data
                    .sources
                    .iter()
                    .nth(i)
                    .unwrap()
                    .is_float(&self.data)?
                {
                    // Convert from [`i64`] to [`f64`].
                    let fval = self.builder.ins().fcvt_from_sint(types::F64, *val);
                    out.push(fval);
                } else {
                    // Already a float, no need to convert.
                    out.push(*val);
                }
            }
            Some(out)
        } else {
            None
        };

        // Sum all of the source values to get a total minus the destination.
        // We repeat this for float output if required.
        let summed_srcs = self.sum_vals(src_vals, false);
        let summed_float_srcs = float_srcs.map(|f_srcs| self.sum_vals(f_srcs, true));

        // For each destination, we need to generate a new set of additions.
        for dest in add_data.dests.iter() {
            // Check whether this destination results in a floating point output.
            let results_in_float = self.data.sym_pic(dest)?.is_float();

            // Fetch the appropriate sum value for this destination.
            let src_sum_val = if results_in_float {
                *summed_float_srcs.as_ref().unwrap()
            } else {
                summed_srcs
            };

            // If the destination is non-overwrite, we need to load it once and add that to
            // create our final result.
            let final_val = if !add_data.overwrite_dests {
                // Load the destination as either a float/integer.
                let dest_val = self.load_var(dest)?;

                // Perform the addition.
                if results_in_float {
                    self.builder.ins().fadd(src_sum_val, dest_val)
                } else {
                    self.builder.ins().iadd(src_sum_val, dest_val)
                }
            } else {
                // The destination is overwrite, so just use our summed source values.
                src_sum_val
            };

            // Save the resulting value in the destination.
            let dest_ptr = self.load_static_ptr(self.data.sym_data_id(dest)?)?;
            self.builder
                .ins()
                .store(MemFlags::new(), final_val, dest_ptr, Offset32::new(0));
        }

        Ok(())
    }

    /// Adds together a vector of values, returning their sum.
    /// Supports both integer and floating point addition.
    fn sum_vals(&mut self, vals: Vec<Value>, is_float: bool) -> Value {
        assert!(vals.len() > 0);

        // If there's only one input value, no need to sum anything.
        if vals.len() == 1 {
            return vals.into_iter().next().unwrap();
        }

        // Sum the remaining values.
        let (first_val, second_val) = (*vals.iter().nth(0).unwrap(), *vals.iter().nth(1).unwrap());
        let mut cur_val = if is_float {
            self.builder.ins().fadd(first_val, second_val)
        } else {
            self.builder.ins().iadd(first_val, second_val)
        };
        for src_val in vals.into_iter().skip(2) {
            if is_float {
                cur_val = self.builder.ins().fadd(cur_val, src_val);
            } else {
                cur_val = self.builder.ins().iadd(cur_val, src_val);
            }
        }
        cur_val
    }

    /// Verifies the given ADD instruction.
    fn verify_add(&self, add_data: &AddData) -> Result<()> {
        // Check there are at least one source when adding to destination sources,
        // two sources when overwriting output & at least one output.
        if (add_data.sources.len() == 0 && !add_data.overwrite_dests)
            || (add_data.sources.len() == 1 && add_data.overwrite_dests)
            || add_data.dests.len() == 0
        {
            miette::bail!("An 'ADD' operation must have at least two sources and one output.");
        }

        // Check that the source types are sane.
        let mut results_in_float = false;
        for src in add_data.sources.iter() {
            match src {
                parser::Value::Variable(sym) => {
                    let pic = self.data.sym_pic(sym)?;
                    if pic.is_str() {
                        miette::bail!("Cannot perform an 'ADD' operation on a string.");
                    }
                    if pic.is_float() {
                        results_in_float = true;
                    }
                }
                parser::Value::Literal(lit) => match lit {
                    parser::Literal::String(_) => {
                        miette::bail!("Cannot perform an 'ADD' operation on a string.")
                    }
                    parser::Literal::Int(_) => {}
                    parser::Literal::Float(_) => {
                        results_in_float = true;
                    }
                },
            }
        }

        // Check that the destination types are sane.
        for sym in add_data.dests.iter() {
            let pic = self.data.sym_pic(sym)?;
            if pic.is_str() {
                miette::bail!(
                    "Cannot save the result of an 'ADD' operation in a string-typed variable."
                );
            }
            if results_in_float && !pic.is_float() {
                miette::bail!("Cannot save the result of a floating point 'ADD' operation in an integer-typed variable.");
            }
        }

        Ok(())
    }
}
