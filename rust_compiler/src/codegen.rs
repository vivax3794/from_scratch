use inkwell::context::Context;
use inkwell::types::BasicType;
use inkwell::values::BasicValue;

use crate::ir;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: inkwell::module::Module<'ctx>,
    builder: inkwell::builder::Builder<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("main");
        let builder = context.create_builder();

        Self {
            context,
            module,
            builder,
        }
    }

    pub fn save_to_file(&self, file: &std::path::Path) {
        self.module.write_bitcode_to_path(file);
    }

    fn generate_libc_externs(&mut self) {
        let void_type = self.context.void_type();

        let abort_signature = void_type.fn_type(&[], false);
        self.module.add_function("abort", abort_signature, None);
    }

    fn int_type(&self, size: u8) -> inkwell::types::IntType<'ctx> {
        match size {
            8 => self.context.i8_type(),
            16 => self.context.i16_type(),
            32 => self.context.i32_type(),
            64 => self.context.i64_type(),
            _ => panic!("Invalid int width"),
        }
    }

    fn llvm_type(&self, type_: &ir::Type) -> inkwell::types::BasicTypeEnum<'ctx> {
        match type_ {
            ir::Type::Int(width) => self.int_type(*width).as_basic_type_enum(),
            ir::Type::Bool => self.context.bool_type().as_basic_type_enum(),
        }
    }

    pub fn generate_file(&mut self, file: &ir::File) {
        self.generate_libc_externs();
        for declaration in file.0.iter() {
            self.generate_declaration(declaration);
        }
    }

    fn generate_declaration(&mut self, decl: &ir::Declaration) {
        match decl {
            ir::Declaration::Function {
                name,
                return_type,
                body,
            } => {
                let return_type = self.llvm_type(return_type);
                let signature = return_type.fn_type(&[], false);
                let function = self.module.add_function(&name.str(), signature, None);

                let entry_block = self.context.append_basic_block(function, "entry");
                self.builder.position_at_end(entry_block);
                for stmt in body.0.iter() {
                    self.generate_statement(stmt);
                }
            }
        }
    }

    fn generate_statement(&mut self, stmt: &ir::Statement) {
        match stmt {
            ir::Statement::Return(expr) => {
                let value = self.generate_expression(expr);
                self.builder.build_return(Some(&value));
            }
            ir::Statement::Assert(expr) => {
                let value = self.generate_bool_expression(expr);

                let current_block = self.builder.get_insert_block().unwrap();
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
        }
    }

    fn generate_expression(
        &mut self,
        expr: &ir::Expression,
    ) -> inkwell::values::BasicValueEnum<'ctx> {
        match expr {
            ir::Expression::Int(expr) => self.generate_int_expression(expr).as_basic_value_enum(),
            ir::Expression::Bool(expr) => self.generate_bool_expression(expr).as_basic_value_enum(),
        }
    }

    fn generate_int_expression(
        &mut self,
        expr: &ir::IntExpression,
    ) -> inkwell::values::IntValue<'ctx> {
        match expr {
            ir::IntExpression::Literal { value, width } => {
                let type_ = self.int_type(*width);
                type_.const_int(*value as u64, false)
            }
        }
    }

    fn generate_bool_expression(
        &mut self,
        expr: &ir::BoolExpression,
    ) -> inkwell::values::IntValue<'ctx> {
        match expr {
            ir::BoolExpression::Literal(value) => {
                self.context.bool_type().const_int(*value as u64, false)
            }
            ir::BoolExpression::Not(expr) => {
                let value = self.generate_bool_expression(expr);
                self.builder.build_not(value, "Bool_Not")
            }
            ir::BoolExpression::Comparison(left, chains) => {
                let mut exprs = vec![self.generate_int_expression(left)];
                let mut ops = vec![];

                for (op, expr) in chains.iter() {
                    exprs.push(self.generate_int_expression(expr));
                    ops.push(op);
                }

                let results = vec![];
                for index in 0..ops.len() {
                    let left = exprs[index];
                    let right = exprs[index + 1];

                    let op = ops[index];
                    // let op = match op {};
                    // let result = self.builder.build_int_compare();
                }
                todo!();
            }
        }
    }
}
