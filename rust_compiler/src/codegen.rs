use std::collections::HashMap;

use inkwell::context::Context;
use inkwell::types::BasicType;
use inkwell::values::BasicValue;

use crate::ir;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: inkwell::module::Module<'ctx>,
    builder: inkwell::builder::Builder<'ctx>,

    function_vars: HashMap<
        ir::Identifier,
        (inkwell::values::PointerValue<'ctx>, ir::Type),
    >,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("main");
        let builder = context.create_builder();

        Self {
            context,
            module,
            builder,
            function_vars: HashMap::new(),
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

    fn llvm_type(
        &self,
        type_: &ir::Type,
    ) -> inkwell::types::BasicTypeEnum<'ctx> {
        match type_ {
            ir::Type::Int(width) => {
                self.int_type(*width).as_basic_type_enum()
            }
            ir::Type::Bool => {
                self.context.bool_type().as_basic_type_enum()
            }
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
                vars,
            } => {
                self.function_vars.clear();

                let return_type = self.llvm_type(return_type);
                let signature = return_type.fn_type(&[], false);
                let function = self.module.add_function(
                    &name.str(),
                    signature,
                    None,
                );

                let entry_block = self
                    .context
                    .append_basic_block(function, "entry");
                self.builder.position_at_end(entry_block);

                for (id, type_) in vars.iter() {
                    let ptr = self
                        .builder
                        .build_alloca(self.llvm_type(type_), "Var");
                    self.function_vars.insert(*id, (ptr, *type_));
                }

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

                let current_block =
                    self.builder.get_insert_block().unwrap();
                let fail_branch = self
                    .context
                    .insert_basic_block_after(current_block, "Fail");
                let continue_branch =
                    self.context.insert_basic_block_after(
                        fail_branch,
                        "Continue",
                    );

                self.builder.build_conditional_branch(
                    value,
                    continue_branch,
                    fail_branch,
                );

                self.builder.position_at_end(fail_branch);
                let abort =
                    self.module.get_function("abort").unwrap();
                self.builder.build_call(abort, &[], "Abort");
                self.builder.build_unreachable();

                self.builder.position_at_end(continue_branch);
            }
            ir::Statement::Assign { name, value } => {
                let (ptr, _) = *self.function_vars.get(name).unwrap();
                let expr = self.generate_expression(value);
                self.builder.build_store(ptr, expr);
            }
        }
    }

    fn generate_expression(
        &mut self,
        expr: &ir::Expression,
    ) -> inkwell::values::BasicValueEnum<'ctx> {
        match expr {
            ir::Expression::Int(expr) => self
                .generate_int_expression(expr)
                .as_basic_value_enum(),
            ir::Expression::Bool(expr) => self
                .generate_bool_expression(expr)
                .as_basic_value_enum(),
        }
    }

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
                    self.builder.build_int_s_extend(
                        value,
                        self.int_type(*target),
                        "Signed_Cast",
                    )
                } else {
                    self.builder.build_int_z_extend(
                        value,
                        self.int_type(*target),
                        "Unsigned_Cast",
                    )
                }
            }
            ir::IntExpression::Truncate { value, target } => {
                let value = self.generate_int_expression(value);
                self.builder.build_int_truncate(
                    value,
                    self.int_type(*target),
                    "Trunc",
                )
            }
            ir::IntExpression::Neg(value) => {
                let value = self.generate_int_expression(value);
                self.builder.build_int_neg(value, "Negate value")
            }
            ir::IntExpression::Binary(left, op, right, signed) => {
                let left = self.generate_int_expression(left);
                let right = self.generate_int_expression(right);

                match (op, signed) {
                    (ir::IntBinaryOp::Add, _) => {
                        self.builder.build_int_add(left, right, "Add")
                    }
                    (ir::IntBinaryOp::Sub, _) => {
                        self.builder.build_int_sub(left, right, "Sub")
                    }
                    (ir::IntBinaryOp::Mul, _) => {
                        self.builder.build_int_mul(left, right, "Mul")
                    }
                    (ir::IntBinaryOp::FloorDivision, false) => self
                        .builder
                        .build_int_unsigned_div(left, right, "Div"),
                    (ir::IntBinaryOp::FloorDivision, true) => self
                        .builder
                        .build_int_signed_div(left, right, "Div"),
                    (ir::IntBinaryOp::Remainder, false) => self
                        .builder
                        .build_int_unsigned_rem(left, right, "Rem"),
                    (ir::IntBinaryOp::Remainder, true) => self
                        .builder
                        .build_int_signed_rem(left, right, "Rem"),
                }
            }
            ir::IntExpression::LoadVar(id) => {
                let (ptr, type_) =
                    *self.function_vars.get(id).unwrap();
                self.builder
                    .build_load(self.llvm_type(&type_), ptr, "Var")
                    .into_int_value()
            }
        }
    }

    fn generate_bool_expression(
        &mut self,
        expr: &ir::BoolExpression,
    ) -> inkwell::values::IntValue<'ctx> {
        match expr {
            ir::BoolExpression::Literal(value) => self
                .context
                .bool_type()
                .const_int(*value as u64, false),
            ir::BoolExpression::Not(expr) => {
                let value = self.generate_bool_expression(expr);
                self.builder.build_not(value, "Bool_Not")
            }
            ir::BoolExpression::Comparison(left, chains) => {
                let mut previous = self.generate_int_expression(left);
                let mut result =
                    self.context.bool_type().const_int(1, false);

                for (op, expr) in chains.iter() {
                    let expr = self.generate_int_expression(expr);
                    let this_result = self.builder.build_int_compare(
                        *op,
                        previous,
                        expr,
                        "Comp_Result",
                    );
                    result = self.builder.build_and(
                        result,
                        this_result,
                        "Final_Result",
                    );
                    previous = expr;
                }

                result
            }
            ir::BoolExpression::LoadVar(id) => {
                let (ptr, type_) =
                    *self.function_vars.get(id).unwrap();
                self.builder
                    .build_load(self.llvm_type(&type_), ptr, "Var")
                    .into_int_value()
            }
        }
    }
}
