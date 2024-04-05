use std::path::PathBuf;

/**
 * Structures and utilities for converting parsed ASTs into Cranelift IR.
 */
use cranelift::{
    codegen::{
        ir::{AbiParam, InstBuilder},
        settings::{self, Configurable},
        verify_function,
    },
    frontend::{FunctionBuilder, FunctionBuilderContext},
};
use cranelift_module::Module;
use cranelift_object::{ObjectBuilder, ObjectModule};
use miette::Result;

use crate::config::BuildConfig;

use self::{data::DataManager, func::FuncTranslator, intrinsics::IntrinsicManager};

use super::parser::{Ast, Spanned, Stat};

mod data;
mod func;
mod intrinsics;
mod utils;

/// Base code generator state.
pub struct CodeGenerator<'cfg, 'src> {
    /// The global configuration for this build.
    cfg: &'cfg BuildConfig,

    /// The AST this code generator is translating.
    /// This is wrapped in an [`Option`] for ownership purposes, when the translation
    /// is complete this field becomes [`None`].
    ast: Option<Ast<'src>>,

    /// Function builder context, re-used for all function builders within the module.
    builder_ctx: FunctionBuilderContext,

    /// Main Cranelift context. Holds the codegen state, separate from the module.
    ctx: cranelift::codegen::Context,

    /// The main module.
    module: ObjectModule,

    /// Intrinsics manager for this module.
    intrinsics: IntrinsicManager,

    /// Manages object data for this module.
    data_manager: DataManager,
}

impl<'cfg, 'src> CodeGenerator<'cfg, 'src> {
    /// Creates a new code generator based on the given AST.
    pub fn new(cfg: &'cfg BuildConfig, ast: Ast<'src>) -> Result<Self> {
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
            cfg,
            ast: Some(ast),
            builder_ctx: FunctionBuilderContext::new(),
            ctx: obj_module.make_context(),
            module: obj_module,
            intrinsics: IntrinsicManager::new(),
            data_manager: DataManager::new(),
        })
    }

    /// Generates Cranelift IR from the given AST, consuming it.
    pub fn translate(&mut self) -> Result<()> {
        // Process all AST literal/variable data into Cranelift object data.
        let ast = self.ast.take().unwrap();
        self.data_manager.upload(&mut self.module, &ast)?;

        // Translate all functions.
        self.translate_fn("main", &ast, &ast.proc_div.stats)?;
        Ok(())
    }

    /// Generates a single function from the AST, given a name & list of statements.
    fn translate_fn(
        &mut self,
        name: &str,
        ast: &Ast<'src>,
        stats: &Vec<Spanned<Stat<'src>>>,
    ) -> Result<()> {
        // Create "main" function for later linking.
        // Returns int, has no parameters.
        let int = self.module.target_config().pointer_type();
        self.ctx.func.signature.returns.push(AbiParam::new(int));

        // Declare the function in the module.
        // Must be "export" for ld.
        let func_id = self
            .module
            .declare_function(
                name,
                cranelift_module::Linkage::Export,
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
                ast,
                intrinsics: &mut self.intrinsics,
                data: &mut self.data_manager,
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
            .map_err(|err| {
                miette::diagnostic!("codegen: Failed to define function body: {}", err)
            })?;

        // Verify that the function is valid.
        verify_function(&self.ctx.func, self.module.isa())
            .map_err(|err| miette::diagnostic!("codegen: Function verification failed: {}", err))?;
        println!("{}", self.ctx.func.display());

        self.module.clear_context(&mut self.ctx);
        Ok(())
    }

    /// Converts the generated Cranelift IR to object code, emitting it.
    /// Returns a path to the generated object file, if successful.
    pub fn generate(self) -> Result<PathBuf> {
        // Finish the module, we're all done.
        let out_obj = self.module.finish();

        // Determine the path to the output object file.
        let mut out_path = self.cfg.out_dir.clone();
        out_path.push(format!(
            "{}.o",
            self.cfg.input_file.file_stem().unwrap().to_str().unwrap()
        ));

        // Flush the output object to file.
        let mut file = std::fs::OpenOptions::new()
            .write(true)
            .create(true)
            .open(out_path.clone())
            .map_err(|err| {
                miette::diagnostic!("codegen: Failed to create output object file: {}", err)
            })?;
        std::io::Write::write_all(&mut file, &out_obj.emit().unwrap())
            .map_err(|err| miette::diagnostic!("codegen: Object file write failed: {}", err))?;

        Ok(out_path)
    }
}
