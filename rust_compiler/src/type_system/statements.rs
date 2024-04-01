use super::{scope, types};
use crate::{ast, ir, span, Error, Result};

impl super::TypeResolver {
    /// Resolve a statement.
    pub fn resolve_statement(&mut self, stmt: &ast::Statement) -> Result<ir::Statement> {
        match stmt {
            ast::Statement::Expr(expr) => {
                let (_, expr) = self.resolve_expression(expr)?.0.generic();
                Ok(ir::Statement::Expression(expr.value))
            }
            ast::Statement::Return(expr) => self.resolve_return(expr),
            ast::Statement::Assert(expr, assert_span) => {
                let expr = self.resolve_expression(expr)?.0;
                let expr = expr.bool(*assert_span)?;

                Ok(ir::Statement::Assert(expr.value))
            }
            ast::Statement::AssertType(type_, expr) => {
                let type_ = self.resolve_type(type_)?;
                let expr = self.resolve_expression(expr)?.0;

                let span = expr.span();
                let expr_type = expr.generic_type();

                if type_.value != expr_type {
                    Err(Error::TypeMismatch {
                        expected: type_.value.type_str(),
                        actual: expr_type.type_str(),
                        span: span.into(),
                        reason: Some(type_.span.into()),
                    })
                } else {
                    Ok(ir::Statement::Nop)
                }
            }
            ast::Statement::VaribleBinding {
                name,
                type_,
                value,
                mutable,
            } => self.resolve_variable_declaration(type_, value, name, *mutable),
            ast::Statement::Assign { target, expr, op } => {
                self.resolve_assignment(target, expr, *op)
            }
            ast::Statement::If {
                condition,
                body,
                elif,
                else_block,
            } => self.resolve_if_statement(condition, body, elif, else_block),
            ast::Statement::WhileLoop { condition, body } => {
                let condition = self.resolve_expression(condition)?.0.bool(None)?.value;
                let body = self.resolve_body(body)?;

                Ok(ir::Statement::WhileLoop { condition, body })
            }
        }
    }

    /// Create a new scope with the type narrow
    fn new_scope_from_narrow(&mut self, narrows: &types::TypeNarrows) {
        self.new_scope();
        for (key, value) in narrows.0.iter() {
            #[allow(clippy::expect_used)]
            let var = *self
                .scope
                .get(key)
                .expect("Vars from narrows should always be present in the scope")
                .variable(span::Span::new(0, 0))
                .expect("Should be a variable");

            self.scope.insert(
                key.clone(),
                scope::ScopeItem::Variable(scope::Variable {
                    type_: types::Type::Range(value.0),
                    ..var
                }),
            );
        }
    }

    /// Resolve a if statement.
    fn resolve_if_statement(
        &mut self,
        condition: &span::Spanned<ast::Expression>,
        body: &ast::Body,
        elif: &[(span::Spanned<ast::Expression>, ast::Body)],
        else_block: &Option<ast::Body>,
    ) -> std::prelude::v1::Result<ir::Statement, Error> {
        let (condition, narrows) = self.resolve_expression(condition)?;
        let condition = condition.bool(None)?.value;

        self.new_scope_from_narrow(&narrows);
        let body = self.resolve_body(body)?;
        self.pop_scope();

        let elif = elif
            .iter()
            .map(|(condition, body)| {
                let (expr, narrows) = self.resolve_expression(condition)?;
                self.new_scope_from_narrow(&narrows);
                let body = self.resolve_body(body)?;
                self.pop_scope();
                Ok((expr.bool(None)?.value, body))
            })
            .collect::<Result<Vec<_>>>()?;
        let else_block = else_block
            .as_ref()
            .map(|body| self.resolve_body(body))
            .transpose()?;

        Ok(ir::Statement::If {
            conditions: [(condition, body)]
                .into_iter()
                .chain(elif)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            else_block,
        })
    }

    /// Resolve a assignment.
    fn resolve_assignment(
        &mut self,
        target: &span::Spanned<ast::Expression>,
        expr: &span::Spanned<ast::Expression>,
        op: Option<span::Spanned<ast::BinaryOp>>,
    ) -> std::prelude::v1::Result<ir::Statement, Error> {
        let ast::Expression::Identifier(name) = &target.value else {
            return Err(Error::InvalidAssignmentTarget {
                reason: "Not an identifier".to_owned(),
                span: target.span.into(),
            });
        };

        let mut var_data = *self
            .scope
            .get(name)
            .ok_or(Error::VariableNotFound {
                span: target.span.into(),
            })?
            .variable(target.span)?;

        if !var_data.mutable {
            return Err(Error::CantMutateImmutable {
                span: target.span.into(),
                decl_span: var_data.span_name.into(),
            });
        }

        // If the type is narrowed down we want to reset it to the original type
        if var_data.true_type != var_data.type_ {
            var_data.type_ = var_data.true_type;
            self.scope
                .insert(name.clone(), scope::ScopeItem::Variable(var_data));
        }

        let expr = self.resolve_expression(expr)?.0;

        if let Some(op) = op {
            self.resolve_compound_assignment(expr, target, var_data, op)
        } else {
            if !var_data.type_.is_sub(&expr.generic_type()) {
                let span = expr.span();
                return Err(Error::TypeMismatch {
                    expected: var_data.type_.type_str(),
                    actual: expr.generic_type().type_str(),
                    span: span.into(),
                    reason: Some(var_data.span_type.into()),
                });
            }
            let expr = types::implicit_convert_to_type(expr, &var_data.type_, var_data.span_type)?;
            Ok(ir::Statement::Assign {
                name: var_data.id,
                value: expr.generic().1.value,
            })
        }
    }

    /// Resolve a compound assignment.
    fn resolve_compound_assignment(
        &mut self,
        expr: types::TypedExpression,
        target: &span::Spanned<ast::Expression>,
        var_data: scope::Variable,
        op: span::Spanned<ast::BinaryOp>,
    ) -> Result<ir::Statement> {
        let (expr_type, expr) = expr.int(target.span)?;
        let type_ = var_data.type_.int(expr.span)?;

        let var_expr = ir::IntExpression::LoadVar(var_data.id);
        let (super_type, results) =
            types::cast_to_common_super_type(vec![(type_, var_expr), (expr_type, expr.value)], 0);
        let mut results = results.into_iter();
        #[allow(clippy::expect_used)]
        let var_expr = results.next().expect("Always has one element");
        #[allow(clippy::expect_used)]
        let expr = results.next().expect("Always has one element");

        Ok(ir::Statement::Assign {
            name: var_data.id,
            value: ir::Expression::Int(ir::IntExpression::Truncate {
                value: Box::new(ir::IntExpression::Binary {
                    left: Box::new(var_expr),
                    op: match op.value {
                        ast::BinaryOp::Add => ir::IntBinaryOp::Add,
                        ast::BinaryOp::Sub => ir::IntBinaryOp::Sub,
                        ast::BinaryOp::Mul => ir::IntBinaryOp::Mul,
                        ast::BinaryOp::FloorDivision => ir::IntBinaryOp::FloorDivision,
                        ast::BinaryOp::Mod => ir::IntBinaryOp::Remainder,
                        _ => {
                            return Err(Error::InvalidBinaryOperation {
                                op: op.value,
                                type_: var_data.type_.type_str(),
                                op_span: op.span.into(),
                            })
                        }
                    },
                    right: Box::new(expr),
                    signed: super_type.signed,
                    // We actually WANT to lose the sign here
                    #[allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]
                    bounds: Some((type_.min as u64, type_.max as u64)),
                    width: super_type.width,
                }),
                target: type_.width,
            }),
        })
    }

    /// Resolve a variable binding.
    fn resolve_variable_declaration(
        &mut self,
        type_: &span::Spanned<ast::Type>,
        value: &span::Spanned<ast::Expression>,
        name: &span::Spanned<ast::Ident>,
        mutable: bool,
    ) -> Result<ir::Statement> {
        let id = ir::Identifier::new();
        let type_ = self.resolve_type(type_)?;
        let value = self.resolve_expression(value)?.0;
        let (value_type, value) =
            types::implicit_convert_to_type(value, &type_.value, type_.span)?.generic();

        if !type_.value.is_sub(&value_type) {
            return Err(Error::TypeMismatch {
                expected: type_.value.type_str(),
                actual: value_type.type_str(),
                span: value.span.into(),
                reason: Some(type_.span.into()),
            });
        }

        self.function_info.vars.push((id, type_.value));
        self.scope.insert(
            name.value.clone(),
            scope::ScopeItem::Variable(scope::Variable {
                id,
                type_: type_.value,
                true_type: type_.value,
                mutable,
                span_name: name.span,
                span_type: type_.span,
            }),
        );

        Ok(ir::Statement::Assign {
            name: id,
            value: value.value,
        })
    }

    /// Resolve a return statement.
    fn resolve_return(&mut self, expr: &span::Spanned<ast::Expression>) -> Result<ir::Statement> {
        let return_type = self.function_info.return_type;
        let expr = self.resolve_expression(expr)?.0;
        let (_, expr) =
            types::implicit_convert_to_type(expr, &return_type.value, return_type.span)?.generic();
        Ok(ir::Statement::Return(expr.value))
    }
}
