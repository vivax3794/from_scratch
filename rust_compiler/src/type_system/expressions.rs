use super::{types, TypeResolver};
use crate::{ast, ir, span, Error, Result};

impl TypeResolver {
    /// Resolve an expression.
    pub fn resolve_expression(
        &mut self,
        expr_ast: &span::Spanned<ast::Expression>,
    ) -> Result<(types::TypedExpression, types::TypeNarrows)> {
        match &expr_ast.value {
            ast::Expression::Identifier(ident) => {
                let var_data = self.scope.get(ident).ok_or(Error::VariableNotFound {
                    span: expr_ast.span.into(),
                })?;

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
        }
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
                    return Err(Error::DivisionByZero {
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
                    return Err(Error::DivisionByZero {
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
                return Err(Error::InvalidBinaryOperation {
                    op: ast::BinaryOp::Pow,
                    type_: "int".to_owned(),
                    op_span: span.into(),
                });
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
