//! This module is responsible for type checking the AST and converting it to the IR.

use std::collections::HashMap;

use crate::ast;
use crate::ir;
use crate::Error;
use crate::Result;
use crate::Span;
use crate::Spanned;

/// Integer type range.
#[derive(Clone, Debug, Copy)]
struct Range {
    /// The minimum value of the range.
    min: i128,
    /// The maximum value of the range.
    max: i128,
    /// The width of the range in bits.
    width: u8,
}

/// The type of an expression.
#[derive(Clone, Debug, Copy)]
enum Type {
    /// A range of integer values.
    Range(Range),
    /// A boolean value.
    Boolean,
}

impl Type {
    /// Get the range of the type.
    fn int(&self, span: Span) -> Result<Range> {
        match self {
            Type::Range(range) => Ok(*range),
            _ => Err(Error::TypeMismatch {
                expected: "int[..]".to_owned(),
                actual: self.type_str(),
                span: span.into(),
                reason: None,
            }),
        }
    }

    /// Get the type as a string.
    fn type_str(&self) -> String {
        match self {
            Type::Range(range) => range.type_str(),
            Type::Boolean => "bool".to_owned(),
        }
    }
}

/// An expression with a type.
#[derive(Debug, Clone)]
enum TypedExpression {
    /// An integer expression.
    Int(Range, Spanned<ir::IntExpression>),
    /// A boolean expression.
    Bool(Spanned<ir::BoolExpression>),
}

impl TypedExpression {
    /// Get the integer expression.
    fn int(
        self,
        reason: impl Into<Option<Span>>,
    ) -> Result<(Range, Spanned<ir::IntExpression>)> {
        match self {
            Self::Int(range, expr) => Ok((range, expr)),
            _ => Err(Error::TypeMismatch {
                expected: "int[..]".to_owned(),
                actual: self.type_str(),
                span: self.span().into(),
                reason: reason.into().map(Into::into),
            }),
        }
    }

    /// Get the boolean expression.
    fn bool(
        self,
        reason: impl Into<Option<Span>>,
    ) -> Result<Spanned<ir::BoolExpression>> {
        match self {
            Self::Bool(expr) => Ok(expr),
            _ => Err(Error::TypeMismatch {
                expected: "bool".to_owned(),
                actual: self.type_str(),
                span: self.span().into(),
                reason: reason.into().map(Into::into),
            }),
        }
    }

    /// Get type as a string.
    fn type_str(&self) -> String {
        self.type_().type_str()
    }

    /// Get the type of the expression.
    const fn type_(&self) -> Type {
        match self {
            Self::Int(range, _) => Type::Range(*range),
            Self::Bool(_) => Type::Boolean,
        }
    }

    /// Get the span of the expression.
    const fn span(&self) -> Span {
        match self {
            Self::Int(_, expr) => expr.span,
            Self::Bool(expr) => expr.span,
        }
    }

    /// Split the expression into its type and the IR expression.
    fn generic(self) -> (Type, Spanned<ir::Expression>) {
        match self {
            Self::Int(range, expr) => {
                let expr = expr
                    .span
                    .with_value(ir::Expression::Int(expr.value));
                (Type::Range(range), expr)
            }
            Self::Bool(expr) => {
                let expr = expr
                    .span
                    .with_value(ir::Expression::Bool(expr.value));
                (Type::Boolean, expr)
            }
        }
    }

    /// Get the type of the expression.
    const fn generic_type(&self) -> Type {
        match self {
            Self::Int(range, _) => Type::Range(*range),
            Self::Bool(_) => Type::Boolean,
        }
    }
}

impl Range {
    /// Create a new range.
    fn new(min: i128, max: i128, span: Span) -> Result<Self> {
        Ok(Self {
            min,
            max,
            width: Self::width(min, max, span)?,
        })
    }

    /// Check if the range contains a value.
    const fn contains(&self, value: i128) -> bool {
        self.min <= value && value <= self.max
    }

    /// Get the bit width of the range.
    #[allow(clippy::cast_lossless)]
    fn width(min: i128, max: i128, span: Span) -> Result<u8> {
        Ok(if min >= u8::MIN as i128 && max <= u8::MAX as i128 {
            8
        } else if min >= u16::MIN as i128 && max <= u16::MAX as i128 {
            16
        } else if min >= u32::MIN as i128 && max <= u32::MAX as i128 {
            32
        } else if min >= u64::MIN as i128 && max <= u64::MAX as i128 {
            64
        } else if min >= i8::MIN as i128 && max <= i8::MAX as i128 {
            8
        } else if min >= i16::MIN as i128 && max <= i16::MAX as i128 {
            16
        } else if min >= i32::MIN as i128 && max <= i32::MAX as i128 {
            32
        } else if min >= i64::MIN as i128 && max <= i64::MAX as i128 {
            64
        } else {
            return Err(Error::InvalidRange {
                start: min,
                end: max,
                span: span.into(),
            });
        })
    }

    /// Check if the range is signed.
    const fn signed(&self) -> bool {
        self.min < 0
    }

    /// String representation of the range.
    fn type_str(&self) -> String {
        format!("int[{}..{}]", self.min, self.max)
    }
}

impl Type {
    /// Get the underlying IR type.
    const fn underlying(&self) -> ir::Type {
        match self {
            Self::Range(range) => ir::Type::Int(range.width),
            Self::Boolean => ir::Type::Bool,
        }
    }

    /// Check if the type is a subrange of another type.
    fn is_sub(&self, other: &Type) -> bool {
        match (self, other) {
            (
                Self::Range(Range {
                    min: self_min,
                    max: self_max,
                    ..
                }),
                Self::Range(Range {
                    min: other_min,
                    max: other_max,
                    ..
                }),
            ) => self_min <= other_min && other_max <= self_max,
            (Self::Boolean, Self::Boolean) => true,
            _ => false,
        }
    }
}

/// A variable in the scope.
#[derive(Clone, Debug, Copy)]
struct Variable {
    /// The identifier of the variable.
    id: ir::Identifier,
    /// The type of the variable.
    type_: Type,
    /// If the variable is mutable.
    mutable: bool,
    /// The span of the variable declaration.
    span: Span,
}

/// A scope of variables.
#[derive(Default, Clone)]
struct Scope {
    /// The variables in the scope.
    vars: HashMap<ast::Ident, Variable>,
}

/// A int type narrow.
#[derive(Clone, Copy, Debug)]
struct TypeNarrow(Range);

/// A map of type narrows.
#[derive(Default)]
struct TypeNarrows(HashMap<ast::Ident, TypeNarrow>);

impl TypeNarrow {
    /// Narrow the type based on a comparison operation.
    fn narrow_type(
        &self,
        operation: ast::ComparissonOp,
        other_type: Range,
    ) -> Self {
        let (min, max) = match operation {
            ast::ComparissonOp::Eq => (
                i128::max(self.0.min, other_type.min),
                i128::min(self.0.max, other_type.max),
            ),
            ast::ComparissonOp::Ne => (self.0.min, self.0.max), // TODO:: wrong,
            ast::ComparissonOp::Lt => (
                self.0.min,
                i128::min(self.0.max, other_type.max - 1),
            ),
            ast::ComparissonOp::Le => {
                (self.0.min, i128::min(self.0.max, other_type.max))
            }
            ast::ComparissonOp::Gt => (
                i128::max(self.0.min, other_type.min - 1),
                self.0.max,
            ),
            ast::ComparissonOp::Ge => {
                (i128::max(self.0.min, other_type.min), self.0.max)
            }
        };
        let range = Range {
            min,
            max,
            width: self.0.width,
        };

        TypeNarrow(range)
    }

    /// Combine two narrows.
    const fn and(self, other: Self) -> Self {
        // TODO
        self
    }
}

impl TypeNarrows {
    /// Narrow the type of an expression.
    fn narrow(
        self,
        operation: ast::ComparissonOp,
        (left_expr, left_ast): (TypedExpression, ast::Expression),
        (right_expr, right_ast): (TypedExpression, ast::Expression),
    ) -> Self {
        let TypedExpression::Int(left_range, _) = left_expr else {
            return Self(HashMap::new());
        };
        let TypedExpression::Int(right_range, _) = right_expr else {
            return Self(HashMap::new());
        };

        let complement = match operation {
            ast::ComparissonOp::Eq => ast::ComparissonOp::Eq,
            ast::ComparissonOp::Ne => ast::ComparissonOp::Ne,
            ast::ComparissonOp::Lt => ast::ComparissonOp::Gt,
            ast::ComparissonOp::Le => ast::ComparissonOp::Ge,
            ast::ComparissonOp::Gt => ast::ComparissonOp::Lt,
            ast::ComparissonOp::Ge => ast::ComparissonOp::Le,
        };

        let left_type = TypeNarrow(left_range)
            .narrow_type(operation, right_range);
        let right_type = TypeNarrow(right_range)
            .narrow_type(complement, left_range);

        let mut narrows = HashMap::new();
        if let ast::Expression::Identifier(ident) = left_ast {
            narrows.insert(ident, left_type);
        }
        if let ast::Expression::Identifier(ident) = right_ast {
            narrows.insert(ident, right_type);
        }
        let new = Self(narrows);
        self.and(new)
    }

    /// Combine two type narrows.
    fn and(mut self, other: Self) -> Self {
        for (key, value) in other.0 {
            match self.0.try_insert(key, value) {
                Ok(_) => {}
                Err(err) => {
                    let mut entry = err.entry;
                    let value = entry.get_mut();
                    *value = value.and(err.value);
                }
            }
        }

        self
    }
}

/// The type resolver.
pub struct TypeResolver {
    /// The return type of the current function.
    return_type: Option<Type>,
    /// The span of the return type
    return_type_span: Option<Span>,
    /// The current scope.
    scope: Scope,
    /// The variables in the current function.
    function_vars: Vec<(ir::Identifier, Type)>,
}

impl TypeResolver {
    /// Create a new type resolver.
    pub fn new() -> Self {
        Self {
            return_type: None,
            return_type_span: None,
            scope: Scope::default(),
            function_vars: Vec::new(),
        }
    }

    /// Resolve a file.
    pub fn resolve_file(
        &mut self,
        file: &ast::File,
    ) -> Result<ir::File> {
        Ok(ir::File(
            file.0
                .iter()
                .map(|decl| self.resolve_declaration(decl))
                .collect::<Result<_>>()?,
        ))
    }

    /// Resolve a top level declaration.
    fn resolve_declaration(
        &mut self,
        decl: &ast::Declaration,
    ) -> Result<ir::Declaration> {
        match decl {
            ast::Declaration::Function(function) => {
                self.resolve_function(function)
            }
        }
    }

    /// Resolve a function declaration.
    fn resolve_function(
        &mut self,
        func: &ast::FunctionDeclration,
    ) -> Result<ir::Declaration> {
        match func {
            ast::FunctionDeclration::ExposedFunction {
                name,
                return_type,
                body,
            } => {
                self.function_vars.clear();
                self.scope = Scope::default();

                let return_type = self.resolve_type(return_type)?;
                self.return_type = Some(return_type.value);
                self.return_type_span = Some(return_type.span);
                let body = self.resolve_body(body)?;
                self.return_type = None;

                Ok(ir::Declaration::Function {
                    name: ir::FunctionName::Named(name.0.clone()),
                    return_type: return_type.value.underlying(),
                    body,
                    vars: self
                        .function_vars
                        .clone()
                        .into_iter()
                        .map(|(id, type_)| (id, type_.underlying()))
                        .collect(),
                })
            }
        }
    }

    /// Resolve a body, creating a new scope.
    fn resolve_body(&mut self, body: &ast::Body) -> Result<ir::Body> {
        let scope_before = self.scope.clone();
        let body = body
            .0
            .iter()
            .map(|stmt| self.resolve_statement(stmt))
            .collect::<Result<_>>()?;
        self.scope = scope_before;
        Ok(ir::Body(body))
    }

    /// Resolve a statement.
    fn resolve_statement(
        &mut self,
        stmt: &ast::Statement,
    ) -> Result<ir::Statement> {
        match stmt {
            ast::Statement::Expr(expr) => {
                let (_, expr) =
                    self.resolve_expression(expr)?.0.generic();
                Ok(ir::Statement::Expression(expr.value))
            }
            ast::Statement::Return(expr) => {
                let return_type = *self.return_type.as_ref().unwrap();
                let expr = self.resolve_expression(expr)?.0;
                let (expr_type, expr) =
                    convert_if_possible(expr, &return_type)?
                        .generic();
                if return_type.is_sub(&expr_type) {
                    Ok(ir::Statement::Return(expr.value))
                } else {
                    Err(Error::TypeMismatch {
                        expected: return_type.type_str(),
                        actual: expr_type.type_str(),
                        span: expr.span.into(),
                        // This should always be Some
                        // But it is nicer for the linter to do it with map instead of unwrap and
                        // wrapping that in a Some
                        #[allow(clippy::expect_used)]
                        reason: Some(
                            self.return_type_span
                                .expect("Should be some")
                                .into(),
                        ),
                    })
                }
            }
            ast::Statement::Assert(expr, assert_span) => {
                let expr = self.resolve_expression(expr)?.0;
                let expr = expr.bool(*assert_span)?;

                Ok(ir::Statement::Assert(expr.value))
            }
            ast::Statement::VaribleBinding {
                name,
                type_,
                value,
                mutable,
            } => {
                let id = ir::Identifier::new();
                let type_ = self.resolve_type(type_)?;
                let value = self.resolve_expression(value)?.0;
                let (value_type, value) =
                    convert_if_possible(value, &type_.value)?
                        .generic();

                if !type_.value.is_sub(&value_type) {
                    return Err(Error::TypeMismatch {
                        expected: type_.value.type_str(),
                        actual: value_type.type_str(),
                        span: value.span.into(),
                        reason: Some(type_.span.into()),
                    });
                }

                self.function_vars.push((id, type_.value));
                self.scope.vars.insert(
                    name.value.clone(),
                    Variable {
                        id,
                        type_: type_.value,
                        mutable: *mutable,
                        span: name.span,
                    },
                );

                Ok(ir::Statement::Assign {
                    name: id,
                    value: value.value,
                })
            }
            ast::Statement::Assign { target, expr, op } => {
                let ast::Expression::Identifier(name) = &target.value
                else {
                    return Err(Error::InvalidAssignmentTarget {
                        reason: "Not an identifier".to_owned(),
                        span: target.span.into(),
                    });
                };

                let var_data = *self.scope.vars.get(name).ok_or(
                    Error::VariableNotFound {
                        span: target.span.into(),
                    },
                )?;
                if !var_data.mutable {
                    return Err(Error::CantMutateImmutable {
                        span: target.span.into(),
                        decl_span: var_data.span.into(),
                    });
                }

                let expr = self.resolve_expression(expr)?.0;

                if let Some(op) = op {
                    let (expr_type, expr) = expr.int(target.span)?;
                    let type_ = var_data.type_.int(expr.span)?;

                    let var_expr =
                        ir::IntExpression::LoadVar(var_data.id);
                    let (signed, width, results) = self
                        .make_same_type(
                            vec![
                                (type_, var_expr),
                                (expr_type, expr.value),
                            ],
                            0,
                        );
                    let mut results = results.into_iter();
                    #[allow(clippy::expect_used)]
                    let var_expr = results
                        .next()
                        .expect("Always has one element");
                    #[allow(clippy::expect_used)]
                    let expr = results
                        .next()
                        .expect("Always has one element");

                    Ok(ir::Statement::Assign {
                            name: var_data.id,
                            value: ir::Expression::Int(ir::IntExpression::Truncate {
                                value: Box::new(ir::IntExpression::Binary {
                                    left: Box::new(var_expr),
                                    op: match op {
                                        ast::BinaryOp::Add => ir::IntBinaryOp::Add,
                                        ast::BinaryOp::Sub => ir::IntBinaryOp::Sub,
                                        ast::BinaryOp::Mul => ir::IntBinaryOp::Mul,
                                        ast::BinaryOp::FloorDivision => ir::IntBinaryOp::FloorDivision,
                                        ast::BinaryOp::Mod => ir::IntBinaryOp::Remainder,
                                    },
                                    right: Box::new(expr),
                                    signed,
                                    bounds: Some((type_.min as u64, type_.max as u64)),
                                    width,
                                }),
                                target: type_.width,
                            }),
                        })
                } else {
                    if !var_data.type_.is_sub(&expr.generic_type()) {
                        panic!("At least give them the same type");
                    }
                    let expr =
                        convert_if_possible(expr, &var_data.type_)?;
                    Ok(ir::Statement::Assign {
                        name: var_data.id,
                        value: expr.generic().1.value,
                    })
                }
            }
            ast::Statement::If {
                condition,
                body,
                elif,
                else_block,
            } => {
                let (condition, narrows) =
                    self.resolve_expression(condition)?;
                let condition = condition.bool(None)?.value;

                let restore_scope = self.scope.clone();
                for (key, value) in narrows.0.into_iter() {
                    self.scope.vars.get_mut(&key).unwrap().type_ =
                        Type::Range(value.0);
                }
                let body = self.resolve_body(body)?;
                self.scope = restore_scope;

                let elif = elif
                    .iter()
                    .map(|(condition, body)| {
                        Ok((
                            self.resolve_expression(condition)?
                                .0
                                .bool(None)?
                                .value,
                            self.resolve_body(body)?,
                        ))
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
            ast::Statement::WhileLoop { condition, body } => {
                let condition = self
                    .resolve_expression(condition)?
                    .0
                    .bool(None)?
                    .value;
                let body = self.resolve_body(body)?;

                Ok(ir::Statement::WhileLoop { condition, body })
            }
        }
    }

    /// Resolve an expression.
    fn resolve_expression(
        &mut self,
        expr_ast: &Spanned<ast::Expression>,
    ) -> Result<(TypedExpression, TypeNarrows)> {
        match &expr_ast.value {
            ast::Expression::Identifier(ident) => {
                let var_data = self.scope.vars.get(ident).ok_or(
                    Error::VariableNotFound {
                        span: expr_ast.span.into(),
                    },
                )?;

                Ok((
                    match var_data.type_ {
                        Type::Range(range) => TypedExpression::Int(
                            range,
                            expr_ast.span.with_value(
                                ir::IntExpression::LoadVar(
                                    var_data.id,
                                ),
                            ),
                        ),
                        Type::Boolean => TypedExpression::Bool(
                            expr_ast.span.with_value(
                                ir::BoolExpression::LoadVar(
                                    var_data.id,
                                ),
                            ),
                        ),
                    },
                    TypeNarrows::default(),
                ))
            }
            ast::Expression::Literal(lit) => match lit {
                ast::Literal::Int(value) => {
                    let value = *value;
                    let range =
                        Range::new(value, value, expr_ast.span)?;
                    let width = range.width;
                    let expr = ir::IntExpression::Literal {
                        value: if value >= 0 {
                            #[allow(
                                clippy::cast_sign_loss,
                                clippy::cast_possible_truncation
                            )]
                            {
                                value as u64
                            }
                        } else {
                            #[allow(clippy::expect_used)]
                            let signed: i64 = value.try_into().expect(
                                "The range line above should have caught this",
                            );

                            #[allow(clippy::cast_sign_loss)]
                            {
                                signed as u64
                            }
                        },
                        width,
                    };
                    Ok((
                        TypedExpression::Int(
                            range,
                            expr_ast.span.with_value(expr),
                        ),
                        TypeNarrows::default(),
                    ))
                }
                ast::Literal::Bool(value) => Ok((
                    TypedExpression::Bool(expr_ast.span.with_value(
                        ir::BoolExpression::Literal(*value),
                    )),
                    TypeNarrows::default(),
                )),
            },
            ast::Expression::Prefix(op, expr) => {
                let expr = self.resolve_expression(expr)?;
                match op {
                    ast::PrefixOp::Not => {
                        let expr = ir::BoolExpression::Not(Box::new(
                            expr.0.bool(None)?.value,
                        ));
                        Ok((
                            TypedExpression::Bool(
                                expr_ast.span.with_value(expr),
                            ),
                            TypeNarrows::default(),
                        ))
                    }
                    ast::PrefixOp::Neg => {
                        let (range, expr) = expr.0.int(None)?;
                        let width = min_width_of_range(&range, true);
                        let expr = cast_range_to_width(
                            range, width, expr.value,
                        );

                        let expr =
                            ir::IntExpression::Neg(Box::new(expr));

                        let range = Range {
                            min: -range.max,
                            max: -range.min,
                            width,
                        };
                        Ok((
                            TypedExpression::Int(
                                range,
                                expr_ast.span.with_value(expr),
                            ),
                            TypeNarrows::default(),
                        ))
                    }
                }
            }
            ast::Expression::Comparison(left_ast, chains) => {
                let left = self.resolve_expression(left_ast)?;
                let exprs: Vec<_> = [left.0]
                    .into_iter()
                    .chain(
                        chains
                            .iter()
                            .map(|(_, expr)| {
                                Ok(self.resolve_expression(expr)?.0)
                            })
                            .collect::<Result<Vec<_>>>()?,
                    )
                    .collect();

                let mut index = 0;
                let mut narrow = TypeNarrows::default();
                while index < exprs.len() - 1 {
                    let left_expr = &exprs[index];
                    let right_expr = &exprs[index + 1];

                    let left_ast = if index == 0 {
                        left_ast
                    } else {
                        &chains[index - 1].1
                    };
                    let (comparison, right_ast) = &chains[index];

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

                let (signed, _, exprs) =
                    self.make_same_type(exprs, 0);

                let ops =
                    chains.iter().map(|(op, _)| match (signed, op) {
                        (_, ast::ComparissonOp::Eq) => {
                            inkwell::IntPredicate::EQ
                        }
                        (_, ast::ComparissonOp::Ne) => {
                            inkwell::IntPredicate::NE
                        }
                        (false, ast::ComparissonOp::Lt) => {
                            inkwell::IntPredicate::ULT
                        }
                        (false, ast::ComparissonOp::Gt) => {
                            inkwell::IntPredicate::UGT
                        }
                        (false, ast::ComparissonOp::Le) => {
                            inkwell::IntPredicate::ULE
                        }
                        (false, ast::ComparissonOp::Ge) => {
                            inkwell::IntPredicate::UGE
                        }
                        (true, ast::ComparissonOp::Lt) => {
                            inkwell::IntPredicate::SLT
                        }
                        (true, ast::ComparissonOp::Gt) => {
                            inkwell::IntPredicate::SGT
                        }
                        (true, ast::ComparissonOp::Le) => {
                            inkwell::IntPredicate::SLE
                        }
                        (true, ast::ComparissonOp::Ge) => {
                            inkwell::IntPredicate::SGE
                        }
                    });

                let mut exprs = exprs.into_iter();
                #[allow(clippy::expect_used)]
                let left =
                    exprs.next().expect("Always has one element");
                let chains = ops.zip(exprs).collect();

                Ok((
                    TypedExpression::Bool(expr_ast.span.with_value(
                        ir::BoolExpression::Comparison(left, chains),
                    )),
                    narrow,
                ))
            }
            ast::Expression::Binary(left, op, right) => {
                let (left_range, left) =
                    self.resolve_expression(left)?.0.int(None)?;
                let (right_range, right) =
                    self.resolve_expression(right)?.0.int(None)?;

                let (mut new_range, op) = match op {
                    ast::BinaryOp::Add => (
                        Range::new(
                            left_range.min + right_range.min,
                            left_range.max + right_range.max,
                            expr_ast.span,
                        )?,
                        ir::IntBinaryOp::Add,
                    ),
                    ast::BinaryOp::Sub => (
                        Range::new(
                            left_range.min - right_range.max,
                            left_range.max - right_range.min,
                            expr_ast.span,
                        )?,
                        ir::IntBinaryOp::Sub,
                    ),
                    ast::BinaryOp::Mul => {
                        let bottom = left_range.min * right_range.min;
                        let top = left_range.max * right_range.max;
                        (
                            Range::new(
                                i128::min(top, bottom),
                                i128::max(top, bottom),
                                expr_ast.span,
                            )?,
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
                            Range::new(
                                i128::min(top, bottom),
                                i128::max(top, bottom),
                                expr_ast.span,
                            )?,
                            ir::IntBinaryOp::FloorDivision,
                        )
                    }
                    ast::BinaryOp::Mod => {
                        // a % b = a - b * (a / b)
                        if right_range.contains(0) {
                            panic!("Dont divide by zero you idiot");
                        }

                        let bottom = right_range.min
                            * (left_range.min / right_range.min);
                        let top = right_range.max
                            * (left_range.max / right_range.max);

                        (
                            Range::new(
                                left_range.min
                                    - i128::min(top, bottom),
                                right_range.max
                                    - i128::max(top, bottom),
                                expr_ast.span,
                            )?,
                            ir::IntBinaryOp::Remainder,
                        )
                    }
                };

                let (signed, width, exprs) = self.make_same_type(
                    vec![
                        (left_range, left.value),
                        (right_range, right.value),
                    ],
                    new_range.width,
                );
                new_range.width = width;

                let mut exprs = exprs.into_iter();
                let left = exprs.next().unwrap();
                let right = exprs.next().unwrap();

                Ok((
                    TypedExpression::Int(
                        new_range,
                        expr_ast.span.with_value(
                            ir::IntExpression::Binary {
                                left: Box::new(left),
                                op,
                                right: Box::new(right),
                                signed: signed || new_range.signed(),
                                bounds: None,
                                width,
                            },
                        ),
                    ),
                    TypeNarrows::default(),
                ))
            }
        }
    }

    /// Resolve a type.
    #[allow(clippy::cast_lossless)]
    fn resolve_type(
        &mut self,
        type_: &Spanned<ast::Type>,
    ) -> Result<Spanned<Type>> {
        let result = match &type_.value {
            ast::Type::Range(min, max) => {
                Type::Range(Range::new(*min, *max, type_.span)?)
            }
            ast::Type::Named(name) => match name.0.as_ref() {
                "u8" => Type::Range(Range::new(
                    u8::MIN as i128,
                    u8::MAX as i128,
                    type_.span,
                )?),
                "u16" => Type::Range(Range::new(
                    u16::MIN as i128,
                    u16::MAX as i128,
                    type_.span,
                )?),
                "u32" => Type::Range(Range::new(
                    u32::MIN as i128,
                    u32::MAX as i128,
                    type_.span,
                )?),
                "u64" => Type::Range(Range::new(
                    u64::MIN as i128,
                    u64::MAX as i128,
                    type_.span,
                )?),
                "i8" => Type::Range(Range::new(
                    i8::MIN as i128,
                    i8::MAX as i128,
                    type_.span,
                )?),
                "i16" => Type::Range(Range::new(
                    i16::MIN as i128,
                    i16::MAX as i128,
                    type_.span,
                )?),
                "i32" => Type::Range(Range::new(
                    i32::MIN as i128,
                    i32::MAX as i128,
                    type_.span,
                )?),
                "i64" => Type::Range(Range::new(
                    i64::MIN as i128,
                    i64::MAX as i128,
                    type_.span,
                )?),
                "bool" => Type::Boolean,
                name => panic!("Unknown name {name}"),
            },
        };
        Ok(type_.span.with_value(result))
    }

    /// Make a set of expressions the same type.
    fn make_same_type(
        &mut self,
        exprs: Vec<(Range, ir::IntExpression)>,
        min_width: u8,
    ) -> (bool, u8, Vec<ir::IntExpression>) {
        let signed = exprs.iter().any(|(range, _)| range.signed());
        let common_width = min_width.max(
            exprs
                .iter()
                .map(|(range, _)| min_width_of_range(range, signed))
                .max()
                .unwrap(),
        );

        let exprs = exprs
            .into_iter()
            .map(|(range, expr)| {
                cast_range_to_width(range, common_width, expr)
            })
            .collect();

        (signed, common_width, exprs)
    }
}
/// Get the minimum width of a range to fit a value.
fn min_width_of_range(type_: &Range, target_signed: bool) -> u8 {
    match (target_signed, type_.signed()) {
        (false, false) => type_.width,
        (true, true) => type_.width,
        (true, false) => {
            // If the value can fit in a signed value of the same type no casting is
            // needed.
            if type_.max < 2i128.pow(type_.width as u32 - 1) {
                type_.width
            } else {
                type_.width * 2
            }
        }
        (false, true) => unreachable!(
            "If one of the operands is signed, so must the target."
        ),
    }
}

/// Cast a range to a specific width.
fn cast_range_to_width(
    type_: Range,
    target_width: u8,
    expr: ir::IntExpression,
) -> ir::IntExpression {
    if type_.width == target_width {
        expr
    } else {
        ir::IntExpression::Extend {
            value: Box::new(expr),
            target: target_width,
            signed: type_.signed(),
        }
    }
}

/// Truncate an integer expression to a specific width.
fn truncate_int_maybe(
    expr: ir::IntExpression,
    range: Range,
    target: u8,
) -> ir::IntExpression {
    if range.width == target {
        expr
    } else if range.width > target {
        ir::IntExpression::Truncate {
            value: Box::new(expr),
            target,
        }
    } else {
        cast_range_to_width(range, target, expr)
    }
}

/// Convert an expression to a specific type if possible.
fn convert_if_possible(
    expr: TypedExpression,
    target_type: &Type,
) -> Result<TypedExpression> {
    match expr {
        TypedExpression::Int(range, expr) => {
            Ok(TypedExpression::Int(
                {
                    let mut range = range;
                    range.width = target_type.int(expr.span)?.width;
                    range
                },
                expr.span.with_value(truncate_int_maybe(
                    expr.value,
                    range,
                    target_type.int(expr.span)?.width,
                )),
            ))
        }
        TypedExpression::Bool(expr) => {
            Ok(TypedExpression::Bool(expr))
        }
    }
}
