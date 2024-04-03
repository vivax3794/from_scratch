//! Types for the type checker.
use std::collections::HashMap;

use crate::{ast, ir, span, CompileError, Result};

/// Integer type range.
#[derive(Clone, Debug, Copy, Eq)]
pub struct Range {
    /// The minimum value of the range.
    pub min: i128,
    /// The maximum value of the range.
    pub max: i128,
    /// The width of the range in bits.
    pub width: u8,
}

impl PartialEq for Range {
    fn eq(&self, other: &Self) -> bool {
        self.min == other.min && self.max == other.max
    }
}

impl Range {
    /// Create a new range.
    pub fn new(min: i128, max: i128, span: span::Span) -> Result<Self> {
        if min > max {
            return Err(CompileError::InvalidRange {
                start: min,
                end: max,
                span: span.into(),
            });
        }

        Ok(Self {
            min,
            max,
            width: Self::width(min, max, span)?,
        })
    }

    /// Check if the range contains a value.
    pub const fn contains(&self, value: i128) -> bool {
        self.min <= value && value <= self.max
    }

    /// Get the bit width of the range.
    #[allow(clippy::cast_lossless)]
    fn width(min: i128, max: i128, span: span::Span) -> Result<u8> {
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
            return Err(CompileError::TooLargeRange {
                start: min,
                end: max,
                span: span.into(),
            });
        })
    }

    /// Check if the range is signed.
    pub const fn signed(&self) -> bool {
        self.min < 0
    }

    /// String representation of the range.
    pub fn type_str(&self) -> String {
        format!("int[{}..{}]", self.min, self.max)
    }
}

/// The type of an expression.
#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum Type {
    /// A range of integer values.
    Range(Range),
    /// A boolean value.
    Boolean,
}

impl Type {
    /// Get the range of the type.
    pub fn int(&self, span: span::Span) -> Result<Range> {
        if let Type::Range(range) = self {
            Ok(*range)
        } else {
            Err(CompileError::TypeMismatch {
                expected: "int[..]".to_owned(),
                actual: self.type_str(),
                span: span.into(),
                reason: None,
            })
        }
    }

    /// Get the type as a string.
    pub fn type_str(&self) -> String {
        match self {
            Type::Range(range) => range.type_str(),
            Type::Boolean => "bool".to_owned(),
        }
    }

    /// Get the underlying IR type.
    pub const fn underlying(&self) -> ir::Type {
        match self {
            Self::Range(range) => ir::Type::Int(range.width),
            Self::Boolean => ir::Type::Bool,
        }
    }

    /// Check if the type is a subrange of another type.
    pub fn is_sub(&self, other: &Type) -> bool {
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

    /// Load a variable of a specific type.
    pub const fn load_var(&self, var: ir::Identifier, span: span::Span) -> TypedExpression {
        match self {
            Self::Range(range) => {
                TypedExpression::Int(*range, span.with_value(ir::IntExpression::LoadVar(var)))
            }
            Self::Boolean => {
                TypedExpression::Bool(span.with_value(ir::BoolExpression::LoadVar(var)))
            }
        }
    }
}

/// An expression with a type.
#[derive(Clone)]
pub enum TypedExpression {
    /// An integer expression.
    Int(Range, span::Spanned<ir::IntExpression>),
    /// A boolean expression.
    Bool(span::Spanned<ir::BoolExpression>),
}

impl TypedExpression {
    /// Get the integer expression.
    pub fn int(
        self,
        reason: impl Into<Option<span::Span>>,
    ) -> Result<(Range, span::Spanned<ir::IntExpression>)> {
        if let Self::Int(range, expr) = self {
            Ok((range, expr))
        } else {
            Err(CompileError::TypeMismatch {
                expected: "int[..]".to_owned(),
                actual: self.type_str(),
                span: self.span().into(),
                reason: reason.into().map(Into::into),
            })
        }
    }

    /// Get the boolean expression.
    pub fn bool(
        self,
        reason: impl Into<Option<span::Span>>,
    ) -> Result<span::Spanned<ir::BoolExpression>> {
        if let Self::Bool(expr) = self {
            Ok(expr)
        } else {
            Err(CompileError::TypeMismatch {
                expected: "bool".to_owned(),
                actual: self.type_str(),
                span: self.span().into(),
                reason: reason.into().map(Into::into),
            })
        }
    }

    /// Get type as a string.
    pub fn type_str(&self) -> String {
        self.type_().type_str()
    }

    /// Get the type of the expression.
    pub const fn type_(&self) -> Type {
        match self {
            Self::Int(range, _) => Type::Range(*range),
            Self::Bool(_) => Type::Boolean,
        }
    }

    /// Get the span of the expression.
    pub const fn span(&self) -> span::Span {
        match self {
            Self::Int(_, expr) => expr.span,
            Self::Bool(expr) => expr.span,
        }
    }

    /// Split the expression into its type and the IR expression.
    pub fn generic(self) -> (Type, span::Spanned<ir::Expression>) {
        match self {
            Self::Int(range, expr) => {
                let expr = expr.span.with_value(ir::Expression::Int(expr.value));
                (Type::Range(range), expr)
            }
            Self::Bool(expr) => {
                let expr = expr.span.with_value(ir::Expression::Bool(expr.value));
                (Type::Boolean, expr)
            }
        }
    }

    /// Get the type of the expression.
    pub const fn generic_type(&self) -> Type {
        match self {
            Self::Int(range, _) => Type::Range(*range),
            Self::Bool(_) => Type::Boolean,
        }
    }
}

/// A int type narrow.
#[derive(Clone, Copy, Debug)]
pub struct TypeNarrow(pub Range);

/// A map of type narrows.
#[derive(Default)]
pub struct TypeNarrows {
    /// Positive narrows.
    pub positive: HashMap<ast::Ident, TypeNarrow>,
    /// Negative narrows. i.e else branch.
    pub negative: HashMap<ast::Ident, TypeNarrow>,
}

/// A result of a narrow.
enum NarrowResult {
    /// The narrow was successful.
    Narrowed(Range),
    /// The narrow was not possible.
    /// meaning no narrow should be generated. (but no error should be generated either)
    NotPossible,
}

impl TypeNarrow {
    /// Narrow the type based on a comparison operation.
    fn narrow_type(&self, operation: ast::ComparissonOp, other_type: Range) -> NarrowResult {
        let (min, max) = match operation {
            ast::ComparissonOp::Eq => (
                i128::max(self.0.min, other_type.min),
                i128::min(self.0.max, other_type.max),
            ),
            ast::ComparissonOp::Ne => return NarrowResult::NotPossible,
            ast::ComparissonOp::Lt => (self.0.min, i128::min(self.0.max, other_type.max - 1)),
            ast::ComparissonOp::Le => (self.0.min, i128::min(self.0.max, other_type.max)),
            ast::ComparissonOp::Gt => (i128::max(self.0.min, other_type.min + 1), self.0.max),
            ast::ComparissonOp::Ge => (i128::max(self.0.min, other_type.min), self.0.max),
        };

        if min > max {
            return NarrowResult::NotPossible;
        }

        let range = Range {
            min,
            max,
            width: self.0.width,
        };

        NarrowResult::Narrowed(range)
    }

    /// Combine two narrows.
    fn and(self, other: Self) -> Self {
        Self(Range {
            min: i128::max(self.0.min, other.0.min),
            max: i128::min(self.0.max, other.0.max),
            width: self.0.width,
        })
    }
}

/// Returns the reverse of a comparison operation.
const fn comparison_reverse(op: ast::ComparissonOp) -> ast::ComparissonOp {
    match op {
        ast::ComparissonOp::Eq => ast::ComparissonOp::Ne,
        ast::ComparissonOp::Ne => ast::ComparissonOp::Eq,
        ast::ComparissonOp::Lt => ast::ComparissonOp::Ge,
        ast::ComparissonOp::Le => ast::ComparissonOp::Gt,
        ast::ComparissonOp::Gt => ast::ComparissonOp::Le,
        ast::ComparissonOp::Ge => ast::ComparissonOp::Lt,
    }
}

impl TypeNarrows {
    /// Narrow the type of an expression.
    pub fn narrow(
        self,
        operation: ast::ComparissonOp,
        (left_expr, left_ast): (TypedExpression, ast::Expression),
        (right_expr, right_ast): (TypedExpression, ast::Expression),
    ) -> Self {
        let TypedExpression::Int(left_range, _) = left_expr else {
            return Self::default();
        };
        let TypedExpression::Int(right_range, _) = right_expr else {
            return Self::default();
        };

        let complement = match operation {
            ast::ComparissonOp::Eq => ast::ComparissonOp::Eq,
            ast::ComparissonOp::Ne => ast::ComparissonOp::Ne,
            ast::ComparissonOp::Lt => ast::ComparissonOp::Gt,
            ast::ComparissonOp::Le => ast::ComparissonOp::Ge,
            ast::ComparissonOp::Gt => ast::ComparissonOp::Lt,
            ast::ComparissonOp::Ge => ast::ComparissonOp::Le,
        };

        let left_type_pos = TypeNarrow(left_range).narrow_type(operation, right_range);
        let right_type_pos = TypeNarrow(right_range).narrow_type(complement, left_range);
        let left_type_neg =
            TypeNarrow(left_range).narrow_type(comparison_reverse(operation), right_range);
        let right_type_neg =
            TypeNarrow(right_range).narrow_type(comparison_reverse(complement), left_range);

        let mut positive = HashMap::new();
        let mut negative = HashMap::new();
        if let ast::Expression::Identifier(ident) = left_ast {
            if let NarrowResult::Narrowed(range) = left_type_pos {
                positive.insert(ident.clone(), TypeNarrow(range));
            }
            if let NarrowResult::Narrowed(range) = left_type_neg {
                negative.insert(ident, TypeNarrow(range));
            }
        }
        if let ast::Expression::Identifier(ident) = right_ast {
            if let NarrowResult::Narrowed(range) = right_type_pos {
                positive.insert(ident.clone(), TypeNarrow(range));
            }
            if let NarrowResult::Narrowed(range) = right_type_neg {
                negative.insert(ident, TypeNarrow(range));
            }
        }
        self.and(TypeNarrows { positive, negative })
    }

    /// Combine two type narrows.
    pub fn and(mut self, other: Self) -> Self {
        for (key, value) in other.positive {
            match self.positive.try_insert(key, value) {
                Ok(_) => {}
                Err(err) => {
                    let mut entry = err.entry;
                    let value = entry.get_mut();
                    *value = value.and(err.value);
                }
            }
        }
        for (key, value) in other.negative {
            match self.negative.try_insert(key, value) {
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

/// Get the minimum width of a range to fit a value.
pub fn min_width_of_range(type_: &Range, target_signed: bool) -> u8 {
    match (target_signed, type_.signed()) {
        (false, false) | (true, true) => type_.width,
        (true, false) => {
            // If the value can fit in a signed value of the same type no casting is
            // needed.
            #[allow(clippy::cast_lossless)]
            if type_.max < 2i128.pow(type_.width as u32 - 1) {
                type_.width
            } else {
                type_.width * 2
            }
        }
        // It is a logic error if one of the operands is signed and the target is not.
        // TODO: Can we enforce this invariant?
        #[allow(clippy::unreachable)]
        (false, true) => unreachable!("If one of the operands is signed, so must the target."),
    }
}

/// Cast a range to a specific width.
pub fn cast_range_to_width(
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
pub fn cast_expr_to_target_width(
    expr: ir::IntExpression,
    range: Range,
    target: u8,
) -> ir::IntExpression {
    match range.width.cmp(&target) {
        std::cmp::Ordering::Equal => expr,
        std::cmp::Ordering::Greater => ir::IntExpression::Truncate {
            value: Box::new(expr),
            target,
        },
        std::cmp::Ordering::Less => cast_range_to_width(range, target, expr),
    }
}

/// Convert an expression to a specific type.
/// Should only be called in valid cases.
///
/// If called in invalid cases then the resulting IR might cause undefined behavior.
pub fn implicit_convert_to_type(
    expr: TypedExpression,
    target_type: &Type,
    reason: impl Into<Option<span::Span>>,
) -> Result<TypedExpression> {
    match target_type {
        Type::Range(target_range) => {
            if !target_type.is_sub(&expr.type_()) {
                return Err(CompileError::TypeMismatch {
                    expected: target_type.type_str(),
                    actual: expr.type_str(),
                    span: expr.span().into(),
                    reason: reason.into().map(Into::into),
                });
            }

            let (range, expr) = expr.int(reason)?;
            let target_width = target_range.width;
            Ok(TypedExpression::Int(
                {
                    let mut range = range;
                    range.width = target_width;
                    range
                },
                expr.span
                    .with_value(cast_expr_to_target_width(expr.value, range, target_width)),
            ))
        }
        Type::Boolean => {
            let expr = expr.bool(reason)?;
            Ok(TypedExpression::Bool(expr))
        }
    }
}

/// Return value of `cast_to_common_super_type`.
pub struct SuperTypeInfo {
    /// If the type is signed.
    pub signed: bool,
    /// The width of the type.
    pub width: u8,
}

/// Make a set of expressions the same super type.
pub fn cast_to_common_super_type(
    exprs: Vec<(Range, ir::IntExpression)>,
    min_width: u8,
) -> (SuperTypeInfo, Vec<ir::IntExpression>) {
    let signed = exprs.iter().any(|(range, _)| range.signed());
    let common_width = min_width.max(
        #[allow(clippy::expect_used)]
        exprs
            .iter()
            .map(|(range, _)| min_width_of_range(range, signed))
            .max()
            .expect("This should only be called with at least one expression."),
    );

    let exprs = exprs
        .into_iter()
        .map(|(range, expr)| cast_range_to_width(range, common_width, expr))
        .collect();

    (
        SuperTypeInfo {
            signed,
            width: common_width,
        },
        exprs,
    )
}

impl super::TypeResolver {
    /// Resolve a type.
    #[allow(clippy::cast_lossless)]
    #[allow(clippy::unused_self)] // TODO: remove when no longer needed
    pub fn resolve_type(
        &mut self, // We will be looking up types in the scope later
        type_: &span::Spanned<ast::Type>,
    ) -> Result<span::Spanned<Type>> {
        let result = match &type_.value {
            ast::Type::Range(min, max) => Type::Range(Range::new(*min, *max, type_.span)?),
            ast::Type::Named(name) => match name.0.as_ref() {
                "u8" => Type::Range(Range::new(u8::MIN as i128, u8::MAX as i128, type_.span)?),
                "u16" => Type::Range(Range::new(u16::MIN as i128, u16::MAX as i128, type_.span)?),
                "u32" => Type::Range(Range::new(u32::MIN as i128, u32::MAX as i128, type_.span)?),
                "u64" => Type::Range(Range::new(u64::MIN as i128, u64::MAX as i128, type_.span)?),
                "i8" => Type::Range(Range::new(i8::MIN as i128, i8::MAX as i128, type_.span)?),
                "i16" => Type::Range(Range::new(i16::MIN as i128, i16::MAX as i128, type_.span)?),
                "i32" => Type::Range(Range::new(i32::MIN as i128, i32::MAX as i128, type_.span)?),
                "i64" => Type::Range(Range::new(i64::MIN as i128, i64::MAX as i128, type_.span)?),
                "bool" => Type::Boolean,
                _ => {
                    return Err(CompileError::VariableNotFound {
                        span: type_.span.into(),
                    });
                }
            },
        };
        Ok(type_.span.with_value(result))
    }
}
