use cranelift::codegen::ir::{immediates::Offset32, types, InstBuilder, MemFlags, Value};
use miette::Result;

use crate::compiler::parser::{self, BasicMathOpData};

use super::FuncTranslator;

/// Variations of basic mathematical operations output by the code generator.
#[derive(Clone, Copy, PartialEq, Eq)]
enum BasicMathOp {
    Add,
    Subtract,
    Multiply,
}

impl<'src> FuncTranslator<'src> {
    /// Generates Cranelift IR for a single "ADD" statement.
    pub(super) fn translate_add(&mut self, op_data: &BasicMathOpData<'src>) -> Result<()> {
        self.translate_basic_op(op_data, BasicMathOp::Add)
    }

    /// Generates Cranelift IR for a single "SUBTRACT" statement.
    pub(super) fn translate_subtract(&mut self, op_data: &BasicMathOpData<'src>) -> Result<()> {
        self.translate_basic_op(op_data, BasicMathOp::Subtract)
    }

    /// Generates Cranelift IR for a single "MULTIPLY" statement.
    pub(super) fn translate_multiply(&mut self, op_data: &BasicMathOpData<'src>) -> Result<()> {
        self.translate_basic_op(op_data, BasicMathOp::Multiply)
    }

    /// Translates a single basic math operation (ADD, SUB, MUL) into Cranelift IR.
    fn translate_basic_op(
        &mut self,
        op_data: &BasicMathOpData<'src>,
        op_type: BasicMathOp,
    ) -> Result<()> {
        println!("{:#?}", op_data);

        // Verify this instruction is sane.
        self.verify_math_op_data(op_data)?;

        // First, load all the sources.
        let mut src_vals: Vec<Value> = Vec::new();
        for src in op_data.sources.iter() {
            src_vals.push(self.load_value(src)?);
        }

        // If any of the destinations require float output, get a set of converted sources
        // to float values only.
        let float_outputs_exist = op_data
            .dests
            .iter()
            .map(|sym| self.data.sym_pic(sym).is_ok_and(|pic| pic.is_float()))
            .any(|b| b);
        let float_srcs = if float_outputs_exist {
            let mut out: Vec<Value> = Vec::new();
            for (i, val) in src_vals.iter().enumerate() {
                if !op_data
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

        // Combine all of the source values to get a total without including the destination.
        // We repeat this for float output if required.
        let combined_srcs =
            self.combine_sources_vec(src_vals, op_type, op_data.overwrite_dests, false);
        let combined_float_srcs = float_srcs
            .map(|f_srcs| self.combine_sources_vec(f_srcs, op_type, op_data.overwrite_dests, true));

        // For each destination, we need to generate a new calculation.
        for dest in op_data.dests.iter() {
            // Check whether this destination results in a floating point output.
            let results_in_float = self.data.sym_pic(dest)?.is_float();

            // Fetch the appropriate combined value for this destination.
            let src_sum_val = if results_in_float {
                *combined_float_srcs.as_ref().unwrap()
            } else {
                combined_srcs
            };

            // If the destination is non-overwrite, we need to load it once and operate on that to
            // create our final result.
            let final_val = if !op_data.overwrite_dests {
                // Load the destination as either a float/integer.
                let dest_val = self.load_var(dest)?;

                // Perform the operation.
                match op_type {
                    BasicMathOp::Add => {
                        // out = src + dest
                        if results_in_float {
                            self.builder.ins().fadd(src_sum_val, dest_val)
                        } else {
                            self.builder.ins().iadd(src_sum_val, dest_val)
                        }
                    }
                    BasicMathOp::Subtract => {
                        // out = dest - src
                        if results_in_float {
                            self.builder.ins().fsub(dest_val, src_sum_val)
                        } else {
                            self.builder.ins().isub(dest_val, src_sum_val)
                        }
                    }
                    BasicMathOp::Multiply => {
                        // out = dest * src
                        if results_in_float {
                            self.builder.ins().fmul(src_sum_val, dest_val)
                        } else {
                            self.builder.ins().imul(src_sum_val, dest_val)
                        }
                    }
                }
            } else {
                // The destination is overwrite, so just use our combined source values.
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

    /// Combines a vector of source values, returning their combined value.
    /// Supports source combination for both integer and floating point addition, subtraction and multiplication.
    fn combine_sources_vec(
        &mut self,
        vals: Vec<Value>,
        op_type: BasicMathOp,
        is_overwrite: bool,
        is_float: bool,
    ) -> Value {
        assert!(vals.len() > 0);

        // If there's only one input value, no need to combine anything.
        if vals.len() == 1 {
            return vals.into_iter().next().unwrap();
        }

        // If this is a subtract, & there are only two sources (& we're overwriting), simply emit a subtract.
        let (first_val, second_val) = (*vals.iter().nth(0).unwrap(), *vals.iter().nth(1).unwrap());
        if vals.len() == 2 && is_overwrite && op_type == BasicMathOp::Subtract {
            if is_float {
                return self.builder.ins().fsub(second_val, first_val);
            } else {
                return self.builder.ins().isub(second_val, first_val);
            }
        }

        // Combine the first two values.
        let mut cur_val = self.combine_sources(first_val, second_val, op_type, is_float);

        // Get an iterator & combine the remaining values.
        let mut val_iter = vals.into_iter().skip(2).peekable();
        while val_iter.peek().is_some() {
            let src_val = val_iter.next().unwrap();

            // We subtract the final value if we're overwriting and this is a SUB instruction.
            if val_iter.peek().is_none() && is_overwrite && op_type == BasicMathOp::Subtract {
                if is_float {
                    cur_val = self.builder.ins().fsub(src_val, cur_val);
                } else {
                    cur_val = self.builder.ins().isub(src_val, cur_val);
                }
                break;
            }

            cur_val = self.combine_sources(cur_val, src_val, op_type, is_float);
        }
        cur_val
    }

    /// Combines the two given  source values for the given basic mathematical operation.
    fn combine_sources(
        &mut self,
        left: Value,
        right: Value,
        op_type: BasicMathOp,
        is_float: bool,
    ) -> Value {
        match op_type {
            // We actually perform an add for the "SUBTRACT" instruction, as the real calculation
            // for SUBTRACT is `dest - sum(sources)`.
            BasicMathOp::Add | BasicMathOp::Subtract => {
                if is_float {
                    self.builder.ins().fadd(left, right)
                } else {
                    self.builder.ins().iadd(left, right)
                }
            }
            BasicMathOp::Multiply => {
                if is_float {
                    self.builder.ins().fmul(left, right)
                } else {
                    self.builder.ins().imul(left, right)
                }
            }
        }
    }

    /// Verifies the given mathematical operation data.
    fn verify_math_op_data(&self, op_data: &BasicMathOpData<'src>) -> Result<()> {
        // Check there are at least one source when appending to destination sources,
        // two sources when overwriting output & at least one output.
        if (op_data.sources.len() == 0 && !op_data.overwrite_dests)
            || (op_data.sources.len() == 1 && op_data.overwrite_dests)
            || op_data.dests.len() == 0
        {
            miette::bail!("Arithmetic operations must have at least two sources and one output.");
        }

        // Check that the source types are sane.
        let mut results_in_float = false;
        for src in op_data.sources.iter() {
            match src {
                parser::Value::Variable(sym) => {
                    let pic = self.data.sym_pic(sym)?;
                    if pic.is_str() {
                        miette::bail!("Cannot perform an arithmetic operation on a string.");
                    }
                    if pic.is_float() {
                        results_in_float = true;
                    }
                }
                parser::Value::Literal(lit) => match lit {
                    parser::Literal::String(_) => {
                        miette::bail!("Cannot perform an arithmetic operation on a string.")
                    }
                    parser::Literal::Int(_) => {}
                    parser::Literal::Float(_) => {
                        results_in_float = true;
                    }
                },
            }
        }

        // Check that the destination types are sane.
        for sym in op_data.dests.iter() {
            let pic = self.data.sym_pic(sym)?;
            if pic.is_str() {
                miette::bail!(
                    "Cannot save the result of an arithmetic operation in a string-typed variable."
                );
            }
            if results_in_float && !pic.is_float() {
                miette::bail!("Cannot save the result of a floating point arithmetic operation in an integer-typed variable.");
            }
        }

        Ok(())
    }
}
