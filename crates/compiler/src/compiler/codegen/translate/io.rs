use crate::compiler::{codegen::intrinsics::CobaltIntrinsic, parser};
use cranelift::codegen::ir::{immediates::Offset32, InstBuilder, MemFlags};
use cranelift_module::Module;
use miette::Result;

use super::{value::CodegenLiteral, FuncTranslator};

impl<'a, 'src> FuncTranslator<'a, 'src> {
    /// Generates Cranelift IR for a single "DISPLAY" statement.
    pub(super) fn translate_display(&mut self, values: &Vec<parser::Value<'src>>) -> Result<()> {
        // Print each value in order.
        for val in values {
            // Load the value to display.
            let display_val = self.load_value(val)?;

            // Get the relevant intrinsic to output the value.
            let display_intrinsic = if val.is_str(self.data)? {
                CobaltIntrinsic::PrintStr
            } else if val.is_float(self.data)? {
                CobaltIntrinsic::PrintFloat
            } else {
                CobaltIntrinsic::PrintInt
            };

            // Call intrinsic to print the value.
            let display_func =
                self.intrinsics
                    .get_ref(self.module, self.builder.func, display_intrinsic)?;
            self.builder.ins().call(display_func, &[display_val]);
        }

        // All values displayed, now call "putchar" and insert a newline.
        let putchar = self.intrinsics.get_ref(
            self.module,
            self.builder.func,
            CobaltIntrinsic::LibcPutchar,
        )?;
        let newline = self.load_cg_lit(&CodegenLiteral::Char('\n'))?;
        self.builder.ins().call(putchar, &[newline]);

        Ok(())
    }

    /// Generates Cranelift IR for a single "ACCEPT" statement.
    pub(super) fn translate_accept(&mut self, target: &'src str) -> Result<()> {
        // Determine the type of variable to accept.
        let var_ptr = self.load_static_ptr(self.data.sym_data_id(target)?)?;
        let target_pic = self.data.sym_pic(target)?;
        if target_pic.is_str() {
            // Read a string.
            let readstr = self.intrinsics.get_ref(
                self.module,
                self.builder.func,
                CobaltIntrinsic::ReadStr,
            )?;
            let ptr_type = self.module.target_config().pointer_type();
            let buf_len = self
                .builder
                .ins()
                .iconst(ptr_type, target_pic.byte_len as i64);
            self.builder.ins().call(readstr, &[var_ptr, buf_len]);
        } else {
            // Read a value out.
            let intrinsic = if target_pic.is_float() {
                CobaltIntrinsic::ReadFloat
            } else {
                CobaltIntrinsic::ReadInt
            };
            let intrinsic_ref =
                self.intrinsics
                    .get_ref(self.module, self.builder.func, intrinsic)?;
            let call_inst = self.builder.ins().call(intrinsic_ref, &[]);
            let result = *self.builder.inst_results(call_inst).first().unwrap();

            // Save the value.
            self.builder
                .ins()
                .store(MemFlags::new(), result, var_ptr, Offset32::new(0));
        }
        Ok(())
    }
}
