//! The code generator for the compiler

// If the ir is invalid that is a compiler bug and we dont want to spend time on writing error
// handling for that.
#![allow(clippy::unreachable, clippy::expect_used, clippy::unwrap_used)]

use std::collections::HashMap;

use inkwell::context::Context;
use inkwell::types::BasicType;
use inkwell::values::BasicValue;

use crate::ir;

/// The code generator
pub struct CodeGen<'ctx> {
    /// The LLVM context
    context: &'ctx Context,
    /// The LLVM module
    module: inkwell::module::Module<'ctx>,
    /// The LLVM builder
    builder: inkwell::builder::Builder<'ctx>,
    /// The function pass manager
    fpm: inkwell::passes::PassManager<inkwell::module::Module<'ctx>>,

    /// The variables in the current function
    function_vars: HashMap<ir::Identifier, (inkwell::values::PointerValue<'ctx>, ir::Type)>,
}

impl<'ctx> CodeGen<'ctx> {
    /// Create a new code generator
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("main");
        let builder = context.create_builder();

        let fpm = inkwell::passes::PassManager::create(());

        fpm.add_ipsccp_pass();
        fpm.add_new_gvn_pass();
        fpm.add_ind_var_simplify_pass();
        fpm.add_instruction_simplify_pass();
        fpm.add_instruction_combining_pass();

        fpm.add_constant_merge_pass();
        fpm.add_global_optimizer_pass();

        fpm.add_demote_memory_to_register_pass();
        fpm.add_merge_functions_pass();
        fpm.add_dead_arg_elimination_pass();
        fpm.add_function_attrs_pass();
        fpm.add_function_inlining_pass();
        fpm.add_tail_call_elimination_pass();

        fpm.add_licm_pass();
        fpm.add_cfg_simplification_pass();

        fpm.add_global_dce_pass();
        fpm.add_aggressive_dce_pass();
        fpm.add_loop_deletion_pass();

        Self {
            context,
            module,
            builder,
            fpm,
            function_vars: HashMap::new(),
        }
    }

    /// Save the module to a file
    pub fn save_to_file(&self, file: &std::path::Path) {
        self.fpm.run_on(&self.module);
        self.module.write_bitcode_to_path(file);
    }

    /// Generate the externs for libc functions used internally by the compiler
    fn generate_libc_externs(&mut self) {
        let void_type = self.context.void_type();

        let abort_signature = void_type.fn_type(&[], false);
        self.module.add_function("abort", abort_signature, None);
    }

    /// Generate the LLVM intrinsics used by the compiler
    fn generate_llvm_intrinsics(&mut self) {
        let functions = [
            ("add.sat", false),
            ("sub.sat", false),
            // Sat means that it doesnt wrap around
            ("mul.fix.sat", true),
            ("div.fix.sat", true),
            ("min", false),
            ("max", false),
        ];
        let widths = [8, 16, 32, 64];

        for sign in ["s", "u"] {
            for width in widths {
                for (name, is_fixed) in functions {
                    let function_name = format!("llvm.{sign}{name}.i{width}");

                    let int_type = self.int_type(width);
                    let mut arguments = vec![int_type.into(), int_type.into()];
                    if is_fixed {
                        arguments.push(self.int_type(32).into());
                    }

                    let function_type = int_type.fn_type(&arguments, false);

                    self.module
                        .add_function(&function_name, function_type, None);
                }
            }
        }
    }

    /// Get the LLVM integer type for a given width
    fn int_type(&self, size: u8) -> inkwell::types::IntType<'ctx> {
        match size {
            8 => self.context.i8_type(),
            16 => self.context.i16_type(),
            32 => self.context.i32_type(),
            64 => self.context.i64_type(),
            _ => unreachable!("Invalid int width"),
        }
    }

    /// Get the LLVM type for a given ir type
    fn llvm_type(&self, type_: ir::Type) -> inkwell::types::BasicTypeEnum<'ctx> {
        match type_ {
            ir::Type::Int(width) => self.int_type(width).as_basic_type_enum(),
            ir::Type::Bool => self.context.bool_type().as_basic_type_enum(),
        }
    }

    /// Generate the LLVM code for a file
    pub fn generate_file(&mut self, file: &ir::File) {
        self.generate_llvm_intrinsics();
        self.generate_libc_externs();
        for declaration in file.0.iter() {
            self.generate_declaration(declaration);
        }
    }

    /// Generate the LLVM code for a declaration
    fn generate_declaration(&mut self, decl: &ir::Declaration) {
        match decl {
            ir::Declaration::Function {
                name,
                return_type,
                body,
                vars,
            } => {
                self.function_vars.clear();

                let return_type = self.llvm_type(*return_type);
                let signature = return_type.fn_type(&[], false);
                let function = self.module.add_function(&name.str(), signature, None);

                let entry_block = self.context.append_basic_block(function, "entry");
                self.builder.position_at_end(entry_block);

                for (id, type_) in vars.iter() {
                    let ptr = self.builder.build_alloca(self.llvm_type(*type_), "Var");
                    self.function_vars.insert(*id, (ptr, *type_));
                }

                self.generate_body(body);
            }
        }
    }

    /// Generate the LLVM code for a body
    fn generate_body(&mut self, body: &ir::Body) {
        for stmt in body.0.iter() {
            self.generate_statement(stmt);
        }
    }

    /// Generate the LLVM code for a statement
    fn generate_statement(&mut self, stmt: &ir::Statement) {
        match stmt {
            ir::Statement::Nop => {}
            ir::Statement::Expression(expr) => {
                self.generate_expression(expr);
            }
            ir::Statement::Return(expr) => {
                let value = self.generate_expression(expr);
                self.builder.build_return(Some(&value));
            }
            ir::Statement::Assert(expr) => {
                self.generate_assert(expr);
            }
            ir::Statement::Assign { name, value } => {
                let (ptr, _) = *self.function_vars.get(name).unwrap();
                let expr = self.generate_expression(value);
                self.builder.build_store(ptr, expr);
            }
            ir::Statement::If {
                conditions,
                else_block,
            } => {
                self.generate_if(conditions, else_block);
            }
            ir::Statement::WhileLoop { condition, body } => {
                self.generate_while_loop(condition, body);
            }
        }
    }

    /// Generate the LLVM code for a while loop
    fn generate_while_loop(&mut self, condition: &ir::BoolExpression, body: &ir::Body) {
        let current_block = self.builder.get_insert_block().unwrap();
        let condition_block = self
            .context
            .insert_basic_block_after(current_block, "condition");
        let body_block = self
            .context
            .insert_basic_block_after(condition_block, "condition");
        let continue_block = self
            .context
            .insert_basic_block_after(body_block, "condition");

        self.builder.build_unconditional_branch(condition_block);
        self.builder.position_at_end(condition_block);
        let condition = self.generate_bool_expression(condition);
        self.builder
            .build_conditional_branch(condition, body_block, continue_block);

        self.builder.position_at_end(body_block);
        self.generate_body(body);
        self.builder.build_unconditional_branch(condition_block);
        self.builder.position_at_end(continue_block);
    }

    /// Generate the LLVM code for a if statement
    fn generate_if(
        &mut self,
        conditions: &[(ir::BoolExpression, ir::Body)],
        else_block: &Option<ir::Body>,
    ) {
        let current_block = self.builder.get_insert_block().unwrap();
        let continue_block = self
            .context
            .insert_basic_block_after(current_block, "Continue");

        let mut true_block = self.context.insert_basic_block_after(current_block, "True");
        let mut false_block = self.context.insert_basic_block_after(true_block, "False");

        let mut index = 0;
        for (condition, body) in conditions {
            let condition = self.generate_bool_expression(condition);
            self.builder
                .build_conditional_branch(condition, true_block, false_block);

            self.builder.position_at_end(true_block);
            self.generate_body(body);
            self.builder.build_unconditional_branch(continue_block);

            self.builder.position_at_end(false_block);

            index += 1;
            if index != conditions.len() {
                true_block = self.context.insert_basic_block_after(false_block, "True");
                false_block = self.context.insert_basic_block_after(true_block, "False");
            }
        }

        if let Some(else_block) = else_block {
            self.generate_body(else_block);
        }
        self.builder.build_unconditional_branch(continue_block);
        self.builder.position_at_end(continue_block);
    }

    /// Generate the LLVM code for a assert
    fn generate_assert(&mut self, expr: &ir::BoolExpression) {
        let value = self.generate_bool_expression(expr);

        let current_block = self.builder.get_insert_block().expect("No block");
        let fail_branch = self.context.insert_basic_block_after(current_block, "Fail");
        let continue_branch = self
            .context
            .insert_basic_block_after(fail_branch, "Continue");

        self.builder
            .build_conditional_branch(value, continue_branch, fail_branch);

        self.builder.position_at_end(fail_branch);
        let abort = self.module.get_function("abort").unwrap();
        self.builder.build_call(abort, &[], "Abort");
        self.builder.build_unreachable();

        self.builder.position_at_end(continue_branch);
    }

    /// Generate the LLVM code for a expression
    fn generate_expression(
        &mut self,
        expr: &ir::Expression,
    ) -> inkwell::values::BasicValueEnum<'ctx> {
        match expr {
            ir::Expression::Int(expr) => self.generate_int_expression(expr).as_basic_value_enum(),
            ir::Expression::Bool(expr) => self.generate_bool_expression(expr).as_basic_value_enum(),
        }
    }

    /// Generate the LLVM code for a int expression
    fn generate_int_expression(
        &mut self,
        expr: &ir::IntExpression,
    ) -> inkwell::values::IntValue<'ctx> {
        match expr {
            ir::IntExpression::Literal { value, width } => {
                let type_ = self.int_type(*width);
                type_.const_int(*value, false)
            }
            ir::IntExpression::Extend {
                value,
                target,
                signed,
            } => {
                let value = self.generate_int_expression(value);
                if *signed {
                    self.builder
                        .build_int_s_extend(value, self.int_type(*target), "Signed_Cast")
                } else {
                    self.builder
                        .build_int_z_extend(value, self.int_type(*target), "Unsigned_Cast")
                }
            }
            ir::IntExpression::Truncate { value, target } => {
                let value = self.generate_int_expression(value);
                self.builder
                    .build_int_truncate(value, self.int_type(*target), "Trunc")
            }
            ir::IntExpression::Neg(value) => {
                let value = self.generate_int_expression(value);
                self.builder.build_int_neg(value, "Negate value")
            }
            ir::IntExpression::Binary {
                left,
                op,
                right,
                signed,
                bounds,
                width,
            } => self.generate_int_binary(left, op, right, *signed, bounds, *width),
            ir::IntExpression::LoadVar(id) => {
                let (ptr, type_) = *self.function_vars.get(id).unwrap();
                self.builder
                    .build_load(self.llvm_type(type_), ptr, "Var")
                    .into_int_value()
            }
        }
    }

    /// Generate the LLVM code for a binary int operation
    fn generate_int_binary(
        &mut self,
        left: &ir::IntExpression,
        op: &ir::IntBinaryOp,
        right: &ir::IntExpression,
        signed: bool,
        bounds: &Option<(u64, u64)>,
        width: u8,
    ) -> inkwell::values::IntValue<'ctx> {
        let left = self.generate_int_expression(left);
        let right = self.generate_int_expression(right);

        if let ir::IntBinaryOp::Remainder = op {
            return if signed {
                self.builder.build_int_signed_rem(left, right, "Signed Rem")
            } else {
                self.builder
                    .build_int_unsigned_rem(left, right, "Signed Rem")
            };
        }

        let (name, is_fixed) = match op {
            ir::IntBinaryOp::Add => ("add.sat", false),
            ir::IntBinaryOp::Sub => ("sub.sat", false),
            ir::IntBinaryOp::Mul => ("mul.fix.sat", true),
            ir::IntBinaryOp::FloorDivision => ("div.fix.sat", true),
            ir::IntBinaryOp::Remainder => unreachable!(),
        };
        let signed = if signed { "s" } else { "u" };
        let name = format!("llvm.{signed}{name}.i{width}");
        let function = self.module.get_function(&name).unwrap();

        let mut arguments = vec![left.into(), right.into()];
        if is_fixed {
            arguments.push(self.int_type(32).const_int(0, false).into());
        }
        let mut result = self.builder.build_call(function, &arguments, "Result");

        let min = format!("llvm.{signed}min.i{width}");
        let max = format!("llvm.{signed}max.i{width}");

        let min = self.module.get_function(&min).unwrap();
        let max = self.module.get_function(&max).unwrap();

        if let Some(bounds) = bounds {
            result = self.builder.build_call(
                min,
                &[
                    result.try_as_basic_value().unwrap_left().into(),
                    self.int_type(width).const_int(bounds.1, false).into(),
                ],
                "Result",
            );
            result = self.builder.build_call(
                max,
                &[
                    result.try_as_basic_value().unwrap_left().into(),
                    self.int_type(width).const_int(bounds.0, false).into(),
                ],
                "Result",
            );
        }

        result
            .try_as_basic_value()
            .unwrap_left()
            .as_basic_value_enum()
            .into_int_value()
    }

    /// Generate the LLVM code for a bool expression
    fn generate_bool_expression(
        &mut self,
        expr: &ir::BoolExpression,
    ) -> inkwell::values::IntValue<'ctx> {
        match expr {
            ir::BoolExpression::Literal(value) => {
                self.context.bool_type().const_int(u64::from(*value), false)
            }
            ir::BoolExpression::Not(expr) => {
                let value = self.generate_bool_expression(expr);
                self.builder.build_not(value, "Bool_Not")
            }
            ir::BoolExpression::Comparison(left, chains) => {
                let mut previous = self.generate_int_expression(left);
                let mut result = self.context.bool_type().const_int(1, false);

                for (op, expr) in chains.iter() {
                    let expr = self.generate_int_expression(expr);
                    let this_result =
                        self.builder
                            .build_int_compare(*op, previous, expr, "Comp_Result");
                    result = self.builder.build_and(result, this_result, "Final_Result");
                    previous = expr;
                }

                result
            }
            ir::BoolExpression::LoadVar(id) => {
                let (ptr, type_) = *self.function_vars.get(id).unwrap();
                self.builder
                    .build_load(self.llvm_type(type_), ptr, "Var")
                    .into_int_value()
            }
        }
    }
}
