use crate::compiler::{codegen::intrinsics::CobaltIntrinsic, parser};
use cranelift::codegen::ir::{types, InstBuilder};
use miette::Result;

use super::FuncTranslator;

impl<'src> FuncTranslator<'src> {
    /// Generates Cranelift IR for a single "DISPLAY" statement.
    pub(super) fn translate_display(&mut self, values: &Vec<parser::Value<'src>>) -> Result<()> {
        // Print each value in order.
        for val in values {
            // Load the value to display.
            let display_val = self.load_value(val)?;

            // Get the relevant intrinsic to output the value.
            let display_intrinsic = if val.is_str(&self.data)? {
                CobaltIntrinsic::PrintStr
            } else if val.is_float(&self.data)? {
                CobaltIntrinsic::PrintFloat
            } else {
                CobaltIntrinsic::PrintInt
            };

            // Call intrinsic to print the value.
            let display_func =
                self.intrinsics
                    .get_ref(self.module, self.builder.func, display_intrinsic)?;
            self.builder.ins().call(*display_func, &[display_val]);
        }

        // All values displayed, now call "putchar" and insert a newline.
        let putchar = self.intrinsics.get_ref(
            self.module,
            self.builder.func,
            CobaltIntrinsic::LibcPutchar,
        )?;
        let newline = self.builder.ins().iconst(types::I8, ('\n' as u8) as i64);
        self.builder.ins().call(*putchar, &[newline]);

        Ok(())
    }
}
