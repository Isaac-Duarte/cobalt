use cranelift::{
    codegen::{
        ir::{AbiParam, InstBuilder},
        settings::{self, Configurable},
    },
    frontend::{FunctionBuilder, FunctionBuilderContext},
};
use cranelift_module::{DataDescription, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use miette::Result;

use self::func::FuncTranslator;

use super::parser::{Ast, Spanned, Stat};

mod func;

/// Base code generator state.
pub struct CodeGenerator {
    /// Function builder context, re-used for all function builders within the module.
    builder_ctx: FunctionBuilderContext,

    /// Main Cranelift context. Holds the codegen state, separate from the module.
    ctx: cranelift::codegen::Context,

    /// Data context for functions.
    data_description: DataDescription,

    /// The main module.
    module: ObjectModule,
}

impl CodeGenerator {
    /// Creates a new code generator based on the given AST.
    pub fn new(ast: &Ast<'_>) -> Result<Self> {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap();
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();

        let obj_builder = ObjectBuilder::new(
            isa,
            ast.ident_div.program_id,
            cranelift_module::default_libcall_names(),
        )
        .unwrap();
        let obj_module = ObjectModule::new(obj_builder);

        Ok(Self {
            builder_ctx: FunctionBuilderContext::new(),
            ctx: obj_module.make_context(),
            data_description: DataDescription::new(),
            module: obj_module,
        })
    }

    /// Generates Cranelift IR from the given AST, consuming it.
    pub fn translate(&mut self, ast: Ast) -> Result<()> {
        self.translate_fn("main", &ast.proc_div.stats)?;
        Ok(())
    }

    /// Generates a single function from the AST, given a name & list of statements.
    fn translate_fn<'src>(&mut self, name: &str, stats: &Vec<Spanned<Stat<'src>>>) -> Result<()> {
        // Create "main" function for later linking.
        // Returns int, has no parameters.
        let int = self.module.target_config().pointer_type();
        self.ctx.func.signature.returns.push(AbiParam::new(int));

        // Declare the function in the module.
        let func_id = self
            .module
            .declare_function(
                name,
                cranelift_module::Linkage::Local,
                &self.ctx.func.signature,
            )
            .expect("Failed to declare function!");

        // Create builder, begin entry block for function.
        {
            let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_ctx);
            let entry_block = builder.create_block();
            builder.append_block_params_for_function_params(entry_block);

            // Switch to block for this function.
            // By "sealing" here, we inform Cranelift there are no predecessors to this block, as it's the
            // entry block.
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            // Create function translator, translate all statements.
            let mut trans = FuncTranslator {
                builder,
                module: &mut self.module,
            };
            trans.translate(stats)?;

            // Emit the return statement (for now, 0).
            let ret_val = trans.builder.ins().iconst(int, 0);
            trans.builder.ins().return_(&[ret_val]);

            // Finish the function.
            trans.builder.finalize();
        }

        // Define the function from the built function held in ctx.
        self.module
            .define_function(func_id, &mut self.ctx)
            .expect("Failed to define function body.");
        self.module.clear_context(&mut self.ctx);
        Ok(())
    }

    /// Converts the generated Cranelift IR to object code, emitting it.
    pub fn generate(self) {
        // Finish the module, we're all done.
        let out_obj = self.module.finish();

        // Flush the output object to file.
        let mut file = std::fs::OpenOptions::new()
            .write(true)
            .create(true)
            .open("./out.o")
            .expect("Failed to open object file to write.");
        std::io::Write::write_all(&mut file, &out_obj.emit().unwrap())
            .expect("Object file write failed.");
    }
}
