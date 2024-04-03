//! Type resolving for expressions.

use super::{scope, types, TypeResolver};
use crate::{ast, ir, span, CompileError, Result};

impl TypeResolver {
    /// Resolve an expression.
    pub fn resolve_expression(
        &mut self,
        expr_ast: &span::Spanned<ast::Expression>,
    ) -> Result<(types::TypedExpression, types::TypeNarrows)> {
        match &expr_ast.value {
            ast::Expression::Identifier(ident) => {
                let var_data = self
                    .scope
                    .get(ident)
                    .ok_or(CompileError::VariableNotFound {
                        span: expr_ast.span.into(),
                    })?
                    .variable(expr_ast.span)?;

                Ok((
                    var_data.type_.load_var(var_data.id, expr_ast.span),
                    types::TypeNarrows::default(),
                ))
            }
            ast::Expression::Literal(lit) => resolve_literal(lit, expr_ast.span),
            ast::Expression::Prefix(op, expr) => {
                self.resolve_prefix_operator(expr, *op, expr_ast.span)
            }
            ast::Expression::Comparison(left_ast, chains) => {
                self.resolve_comparisson(left_ast, chains, expr_ast.span)
            }
            ast::Expression::Binary(left, op, right) => {
                self.resolve_binary(left, right, *op, expr_ast.span)
            }
            ast::Expression::Call {
                function,
                arguments,
            } => self.resolve_call(function, arguments, expr_ast.span),
            ast::Expression::Cast { type_, expr } => {
                let (expr, _) = self.resolve_expression(expr)?;
                let type_ = self.resolve_type(type_)?;

                let (range, expr) = expr.int(None)?;
                let target_range = type_.value.int(expr_ast.span)?;

                let expr = if range.width < target_range.width {
                    types::cast_range_to_width(range, range.width, expr.value)
                } else {
                    expr.value
                };

                let result_range = types::Range {
                    min: target_range.min,
                    max: target_range.max,
                    width: u8::max(range.width, target_range.width),
                };
                #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                let expr = ir::IntExpression::Cast {
                    target_min: target_range.min as u64,
                    target_max: target_range.max as u64,
                    signed: target_range.signed() || range.signed(),
                    width: result_range.width,
                    value: Box::new(expr),
                };

                Ok((
                    types::TypedExpression::Int(result_range, expr_ast.span.with_value(expr)),
                    types::TypeNarrows::default(),
                ))
            }
        }
    }

    /// Resolve a function call
    fn resolve_call(
        &mut self,
        function: &span::Spanned<ast::Expression>,
        arguments: &[span::Spanned<ast::Expression>],
        span: span::Span,
    ) -> Result<(types::TypedExpression, types::TypeNarrows)> {
        match &function.value {
            ast::Expression::Identifier(iden) => {
                let func = self
                    .scope
                    .get(iden)
                    .ok_or(CompileError::VariableNotFound {
                        span: function.span.into(),
                    })?
                    .function(function.span)?
                    .clone();
                Ok((
                    self.resolve_free_function(&func, arguments, span)?,
                    types::TypeNarrows::default(),
                ))
            }
            _ => {
                let (expr, _) = self.resolve_expression(function)?;
                Err(CompileError::InvalidFunctionCallTarget {
                    span: function.span.into(),
                    type_: expr.type_str(),
                })
            }
        }
    }

    /// Resolve a free standing function call
    fn resolve_free_function(
        &mut self,
        func: &scope::FunctionInfo,
        arguments: &[span::Spanned<ast::Expression>],
        span: span::Span,
    ) -> Result<types::TypedExpression> {
        if func.arguments.len() != arguments.len() {
            return Err(CompileError::FunctionArgumentCount {
                got: arguments.len(),
                expected: func.arguments.len(),
                span: span.into(),
            });
        }
        let mut args = Vec::new();
        for (arg, (_, var)) in arguments.iter().zip(func.arguments.iter()) {
            let (expr, _) = self.resolve_expression(arg)?;

            let expr = types::implicit_convert_to_type(expr, &var.type_, var.span_type)?;
            args.push(expr.generic().1.value);
        }

        let call = ir::Call::FreeStanding {
            function: func.name.clone(),
            arguments: args.into_boxed_slice(),
        };
        Ok(match func.return_type.value {
            types::Type::Boolean => {
                types::TypedExpression::Bool(span.with_value(ir::BoolExpression::Call(call)))
            }
            types::Type::Range(range) => {
                types::TypedExpression::Int(range, span.with_value(ir::IntExpression::Call(call)))
            }
        })
    }

    /// Resolve a binary operator.
    fn resolve_binary(
        &mut self,
        left: &span::Spanned<ast::Expression>,
        right: &span::Spanned<ast::Expression>,
        op: ast::BinaryOp,
        span: span::Span,
    ) -> Result<(types::TypedExpression, types::TypeNarrows)> {
        let (left_range, left) = self.resolve_expression(left)?.0.int(None)?;
        let (right_range, right) = self.resolve_expression(right)?.0.int(None)?;

        let (mut new_range, op) = match op {
            ast::BinaryOp::Add => (
                types::Range::new(
                    left_range.min + right_range.min,
                    left_range.max + right_range.max,
                    span,
                )?,
                ir::IntBinaryOp::Add,
            ),
            ast::BinaryOp::Sub => (
                types::Range::new(
                    left_range.min - right_range.max,
                    left_range.max - right_range.min,
                    span,
                )?,
                ir::IntBinaryOp::Sub,
            ),
            ast::BinaryOp::Mul => {
                let bottom = left_range.min * right_range.min;
                let top = left_range.max * right_range.max;
                (
                    types::Range::new(i128::min(top, bottom), i128::max(top, bottom), span)?,
                    ir::IntBinaryOp::Mul,
                )
            }
            ast::BinaryOp::FloorDivision => {
                // div is really just a mul
                if right_range.contains(0) {
                    return Err(CompileError::DivisionByZero {
                        span: right.span.into(),
                        range: right_range.type_str(),
                    });
                }

                let bottom = left_range.min / right_range.min;
                let top = left_range.max / right_range.max;
                (
                    types::Range::new(i128::min(top, bottom), i128::max(top, bottom), span)?,
                    ir::IntBinaryOp::FloorDivision,
                )
            }
            ast::BinaryOp::Mod => {
                // a % b = a - b * (a // b)
                if right_range.contains(0) {
                    return Err(CompileError::DivisionByZero {
                        span: right.span.into(),
                        range: right_range.type_str(),
                    });
                }

                let right_min = right_range.min.abs();
                let right_max = right_range.max.abs();

                let top = left_range.min.signum() * i128::min(left_range.max.abs(), right_max - 1);

                // Bottom is 0
                // unless the left range does not go above the right range (and doesnt include zero)
                let bottom = if left_range.max.abs() < right_min {
                    left_range.min
                } else {
                    0
                };

                let (bottom, top) = if top < bottom {
                    (top, bottom)
                } else {
                    (bottom, top)
                };

                (
                    types::Range::new(bottom, top, span)?,
                    ir::IntBinaryOp::Remainder,
                )
            }
            ast::BinaryOp::Pow => {
                return Err(CompileError::InvalidBinaryOperation {
                    op: ast::BinaryOp::Pow,
                    type_: "int".to_owned(),
                    op_span: span.into(),
                });
            }
            ast::BinaryOp::And => {
                if left_range.signed() {
                    return Err(CompileError::TypeMismatch {
                        expected: "int[0..]".to_owned(),
                        actual: left_range.type_str(),
                        span: left.span.into(),
                        reason: None,
                    });
                }
                if right_range.signed() {
                    return Err(CompileError::TypeMismatch {
                        expected: "int[0..]".to_owned(),
                        actual: right_range.type_str(),
                        span: right.span.into(),
                        reason: None,
                    });
                }

                let new_range =
                    types::Range::new(0, i128::min(left_range.max, right_range.max), span)?;

                (new_range, ir::IntBinaryOp::And)
            }
            ast::BinaryOp::Or => {
                if left_range.signed() {
                    return Err(CompileError::TypeMismatch {
                        expected: "int[0..]".to_owned(),
                        actual: left_range.type_str(),
                        span: left.span.into(),
                        reason: None,
                    });
                }
                if right_range.signed() {
                    return Err(CompileError::TypeMismatch {
                        expected: "int[0..]".to_owned(),
                        actual: right_range.type_str(),
                        span: right.span.into(),
                        reason: None,
                    });
                }
                // Find the min width that can hold both ranges
                // And this is for the result of the or, so not aligned to 8 bits, we have to use
                // custom logic.

                let left_width = left_range.max.ilog2() + 1;
                let right_width = right_range.max.ilog2() + 1;

                let new_range = types::Range::new(
                    i128::max(left_range.min, right_range.min),
                    (2i128).pow(u32::max(left_width, right_width)) - 1,
                    span,
                )?;

                (new_range, ir::IntBinaryOp::Or)
            }
        };

        let (super_type, mut exprs) = types::cast_to_common_super_type(
            vec![(left_range, left.value), (right_range, right.value)],
            new_range.width,
        );
        new_range.width = super_type.width;

        // its okay to use swap_remove here because we only have two elements
        // so after the first one is removed, the second one is at index 0 no matter what
        let left = exprs.swap_remove(0);
        let right = exprs.swap_remove(0);

        Ok((
            types::TypedExpression::Int(
                new_range,
                span.with_value(ir::IntExpression::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                    signed: super_type.signed || new_range.signed(),
                    bounds: None,
                    width: super_type.width,
                }),
            ),
            types::TypeNarrows::default(),
        ))
    }

    /// Resolve a comparisson.
    fn resolve_comparisson(
        &mut self,
        left_ast: &span::Spanned<ast::Expression>,
        chains: &[(ast::ComparissonOp, Box<span::Spanned<ast::Expression>>)],
        span: span::Span,
    ) -> Result<(types::TypedExpression, types::TypeNarrows)> {
        let left = self.resolve_expression(left_ast)?;
        let exprs: Vec<_> = [left.0]
            .into_iter()
            .chain(
                chains
                    .iter()
                    .map(|(_, expr)| Ok(self.resolve_expression(expr)?.0))
                    .collect::<Result<Vec<_>>>()?,
            )
            .collect();

        let mut index = 0;
        let mut narrow = types::TypeNarrows::default();
        while index < exprs.len() - 1 {
            let left_expr = &exprs[index];
            let right_expr = &exprs[index + 1];

            let (comparison, right_ast) = &chains[index];
            let left_ast = if index == 0 {
                left_ast
            } else {
                &chains[index - 1].1
            };

            narrow = narrow.narrow(
                *comparison,
                (left_expr.clone(), left_ast.clone().value),
                (right_expr.clone(), right_ast.clone().value),
            );

            index += 1;
        }

        let exprs = exprs
            .into_iter()
            .map(|expr| {
                let (range, expr) = expr.int(None)?;
                Ok((range, expr.value))
            })
            .collect::<Result<_>>()?;

        let (super_type, exprs) = types::cast_to_common_super_type(exprs, 0);

        let ops = chains.iter().map(|(op, _)| match (super_type.signed, op) {
            (_, ast::ComparissonOp::Eq) => inkwell::IntPredicate::EQ,
            (_, ast::ComparissonOp::Ne) => inkwell::IntPredicate::NE,
            (false, ast::ComparissonOp::Lt) => inkwell::IntPredicate::ULT,
            (false, ast::ComparissonOp::Gt) => inkwell::IntPredicate::UGT,
            (false, ast::ComparissonOp::Le) => inkwell::IntPredicate::ULE,
            (false, ast::ComparissonOp::Ge) => inkwell::IntPredicate::UGE,
            (true, ast::ComparissonOp::Lt) => inkwell::IntPredicate::SLT,
            (true, ast::ComparissonOp::Gt) => inkwell::IntPredicate::SGT,
            (true, ast::ComparissonOp::Le) => inkwell::IntPredicate::SLE,
            (true, ast::ComparissonOp::Ge) => inkwell::IntPredicate::SGE,
        });

        let mut exprs = exprs.into_iter();
        #[allow(clippy::expect_used)]
        let left = exprs.next().expect("Always has one element");
        let chains = ops.zip(exprs).collect();

        Ok((
            types::TypedExpression::Bool(
                span.with_value(ir::BoolExpression::Comparison(left, chains)),
            ),
            narrow,
        ))
    }

    /// Resolve a prefix operator.
    fn resolve_prefix_operator(
        &mut self,
        expr: &span::Spanned<ast::Expression>,
        op: ast::PrefixOp,
        span: span::Span,
    ) -> Result<(types::TypedExpression, types::TypeNarrows)> {
        let expr = self.resolve_expression(expr)?;
        match op {
            ast::PrefixOp::Not => {
                let expr = ir::BoolExpression::Not(Box::new(expr.0.bool(None)?.value));
                Ok((
                    types::TypedExpression::Bool(span.with_value(expr)),
                    types::TypeNarrows::default(),
                ))
            }
            ast::PrefixOp::Neg => {
                let (range, expr) = expr.0.int(None)?;
                let width = types::min_width_of_range(&range, true);
                let expr = types::cast_range_to_width(range, width, expr.value);

                let expr = ir::IntExpression::Neg(Box::new(expr));

                let range = types::Range {
                    min: -range.max,
                    max: -range.min,
                    width,
                };
                Ok((
                    types::TypedExpression::Int(range, span.with_value(expr)),
                    types::TypeNarrows::default(),
                ))
            }
        }
    }
}

/// Resolve a literal.
fn resolve_literal(
    literal: &ast::Literal,
    span: span::Span,
) -> Result<(types::TypedExpression, types::TypeNarrows)> {
    match literal {
        ast::Literal::Int(value) => {
            let value = *value;
            let range = types::Range::new(value, value, span)?;
            let width = range.width;
            let expr = ir::IntExpression::Literal {
                #[allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]
                value: value as u64,
                width,
            };
            Ok((
                types::TypedExpression::Int(range, span.with_value(expr)),
                types::TypeNarrows::default(),
            ))
        }
        ast::Literal::Bool(value) => Ok((
            types::TypedExpression::Bool(span.with_value(ir::BoolExpression::Literal(*value))),
            types::TypeNarrows::default(),
        )),
    }
}
