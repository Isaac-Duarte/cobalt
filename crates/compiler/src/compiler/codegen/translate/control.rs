use cranelift::{
    codegen::{
        entity::EntityRef,
        ir::{types, InstBuilder},
    },
    frontend::Variable,
};
use miette::Result;

use crate::compiler::parser::{self, Cond, ExitType, Literal, PerformType};

use super::FuncTranslator;

impl<'a, 'src> FuncTranslator<'a, 'src> {
    /// Translates an unconditional GOTO to a paragraph.
    /// In Cobalt's model where each paragraph is a separate function,
    /// this is implemented as a tail-call to the target paragraph followed by return.
    pub(super) fn translate_goto(&mut self, target: &'src str) -> Result<()> {
        let func_ref = self.funcs.get_ref(self.module, self.builder.func, target)?;
        self.builder.ins().call(func_ref, &[]);
        self.builder.ins().return_(&[]);
        Ok(())
    }

    /// Translates the given PERFORM statement into Cranelift IR.
    pub(super) fn translate_perform(&mut self, perform: &PerformType<'src>) -> Result<()> {
        match perform {
            PerformType::Single(target) => self.translate_perform_single(target),
            PerformType::Times(target, times) => self.translate_perform_times(target, times),
            PerformType::Thru(start, end) => self.translate_perform_thru(start, end),
            PerformType::Until {
                target,
                cond,
                test_cond_before,
            } => self.translate_perform_until(target, cond, *test_cond_before),
        }
    }

    /// Translates a single-target PERFORM statement to Cranelift IR.
    fn translate_perform_single(&mut self, target: &'src str) -> Result<()> {
        let func_ref = self.funcs.get_ref(self.module, self.builder.func, target)?;
        self.builder.ins().call(func_ref, &[]);
        Ok(())
    }

    /// Translates a single-target repeating PERFORM statement to Cranelift IR.
    fn translate_perform_times(
        &mut self,
        target: &'src str,
        times: &parser::Value<'src>,
    ) -> Result<()> {
        // Ensure the number of times provided is an integer.
        if times.is_float(self.data)? || times.is_str(self.data)? {
            miette::bail!("The number of repeats for a PERFORM statement must be expressed as an integer value.");
        }

        // Don't bother translating if the number of times is a literal & zero.
        if let parser::Value::Literal(lit) = times {
            match lit {
                Literal::Int(i) => {
                    if *i == 0 {
                        return Ok(());
                    }
                }
                _ => unreachable!(),
            }
        }

        // Evaluate the number of times to perform the block, create a variable to store this.
        let times_init_val = self.load_value(times)?;
        let counter_var = Variable::new(0);
        self.builder.declare_var(counter_var, types::I64);
        self.builder.def_var(counter_var, times_init_val);

        // Create blocks required for this loop, transition to & seal loop block.
        let loop_head_block = self.builder.create_block();
        let loop_body_block = self.builder.create_block();
        let trailing_block = self.builder.create_block();
        self.builder.ins().jump(loop_head_block, &[]);

        // Switch to header block, form the loop exit check.
        self.switch_to_block(loop_head_block);
        let counter = self.builder.use_var(counter_var);
        self.builder
            .ins()
            .brif(counter, loop_body_block, &[], trailing_block, &[]);

        // Fill the loop body block.
        self.switch_to_block(loop_body_block);
        self.builder.seal_block(loop_body_block);
        self.translate_perform_single(target)?;
        let counter_val = self.builder.use_var(counter_var);
        let new_counter_val = self.builder.ins().iadd_imm(counter_val, -1);
        self.builder.def_var(counter_var, new_counter_val);
        self.builder.ins().jump(loop_head_block, &[]);

        // Switch to the trailing block, we're done!
        self.switch_to_block(trailing_block);
        self.builder.seal_block(loop_head_block);
        self.builder.seal_block(trailing_block);

        Ok(())
    }

    /// Translates a multi-target PERFORM statement to Cranelift IR.
    fn translate_perform_thru(&mut self, start: &'src str, end: &'src str) -> Result<()> {
        let start_idx = self
            .ast
            .proc_div
            .paragraphs
            .iter()
            .enumerate()
            .filter(|(_, para)| para.name.is_some_and(|n| n.0 == start))
            .map(|x| x.0)
            .next();
        let end_idx = self
            .ast
            .proc_div
            .paragraphs
            .iter()
            .enumerate()
            .filter(|(_, para)| para.name.is_some_and(|n| n.0 == end))
            .map(|x| x.0)
            .next();

        // Verify that both were found.
        if start_idx.is_none() || end_idx.is_none() {
            miette::bail!("Start or end paragraph could not be found for PERFORM THRU.");
        }
        let (start_idx, end_idx) = (start_idx.unwrap(), end_idx.unwrap());

        // Verify that the order makes sense.
        if start_idx == end_idx {
            miette::bail!("Cannot execute a PERFORM THRU on a single paragraph.");
        }
        if start_idx > end_idx {
            miette::bail!(
                "Start paragraph cannot be later than the given end paragraph within PERFORM THRU."
            );
        }

        // Iterate & generate calls.
        for para in self
            .ast
            .proc_div
            .paragraphs
            .iter()
            .skip(start_idx)
            .take(end_idx - start_idx + 1)
        {
            if let Some((name, _)) = para.name {
                self.translate_perform_single(name)?;
            }
        }
        Ok(())
    }

    /// Translates a single-target repeating conditional PERFORM statement to Cranelift IR.
    fn translate_perform_until(
        &mut self,
        target: &'src str,
        cond: &Cond<'src>,
        test_before: bool,
    ) -> Result<()> {
        // Create the blocks required for this statement.
        let loop_test_block = self.builder.create_block();
        let loop_body_block = self.builder.create_block();
        let trailing_block = self.builder.create_block();

        // Jump to the correct block for the test location.
        let next_block = if test_before {
            loop_test_block
        } else {
            loop_body_block
        };
        self.builder.ins().jump(next_block, &[]);

        // Emit the check block.
        self.switch_to_block(loop_test_block);
        let cond_val = self.translate_cond_eval(cond)?;
        self.builder
            .ins()
            .brif(cond_val, trailing_block, &[], loop_body_block, &[]);

        // Emit the body block.
        self.switch_to_block(loop_body_block);
        self.builder.seal_block(loop_body_block);
        self.translate_perform_single(target)?;
        self.builder.ins().jump(loop_test_block, &[]);

        // Switch to the trailing block, we're done!
        self.switch_to_block(trailing_block);
        self.builder.seal_block(loop_test_block);
        self.builder.seal_block(trailing_block);
        Ok(())
    }

    /// Translates a single exit instruction to Cranelift IR.
    pub(super) fn translate_exit(&mut self, exit_type: &ExitType) -> Result<()> {
        match exit_type {
            ExitType::Paragraph => {
                self.builder.ins().return_(&[]);
            }
        }
        Ok(())
    }
}
