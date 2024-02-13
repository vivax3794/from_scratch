use std::collections::HashMap;

use crate::ast;
use crate::ir;

#[derive(Clone, Debug, Copy)]
struct Range {
    min: i128,
    max: i128,
    width: u8,
}

#[derive(Clone, Debug, Copy)]
enum Type {
    Range(Range),
    Boolean,
}

impl Type {
    fn int(&self) -> Range {
        match self {
            Type::Range(range) => *range,
            _ => panic!("hmmm"),
        }
    }
}

// #[derive(Debug)]
// struct TypedExpression {
//     expr: ir::Expression,
//     type_: Type,
// }
#[derive(Debug)]
enum TypedExpression {
    Int(Range, ir::IntExpression),
    Bool(ir::BoolExpression),
}

impl TypedExpression {
    fn int(self) -> (Range, ir::IntExpression) {
        match self {
            Self::Int(range, expr) => (range, expr),
            _ => panic!("Not a int"),
        }
    }

    fn bool(self) -> ir::BoolExpression {
        match self {
            Self::Bool(expr) => expr,
            _ => panic!("Expected bool"),
        }
    }

    fn generic(self) -> (Type, ir::Expression) {
        match self {
            Self::Int(range, expr) => (Type::Range(range), ir::Expression::Int(expr)),
            Self::Bool(expr) => (Type::Boolean, ir::Expression::Bool(expr)),
        }
    }

    fn generic_type(&self) -> Type {
        match self {
            Self::Int(range, _) => Type::Range(*range),
            Self::Bool(_) => Type::Boolean,
        }
    }
}

impl Range {
    fn new(min: i128, max: i128) -> Self {
        Self {
            min,
            max,
            width: Self::width(min, max),
        }
    }

    fn contains(&self, value: i128) -> bool {
        self.min <= value && value <= self.max
    }

    fn width(min: i128, max: i128) -> u8 {
        if min >= u8::MIN as i128 && max <= u8::MAX as i128 {
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
            panic!("Invalid range")
        }
    }

    fn signed(&self) -> bool {
        self.min < 0
    }
}

impl Type {
    fn underlying(&self) -> ir::Type {
        match self {
            Self::Range(range) => ir::Type::Int(range.width),
            Self::Boolean => ir::Type::Bool,
        }
    }

    fn is_sub(&self, other: &Type) -> bool {
        match (self, other) {
            (
                Self::Range(Range {
                    min: smin,
                    max: smax,
                    ..
                }),
                Self::Range(Range {
                    min: omin,
                    max: omax,
                    ..
                }),
            ) => smin <= omin && omax <= smax,
            (Self::Boolean, Self::Boolean) => true,
            _ => false,
        }
    }
}

#[derive(Default, Clone)]
struct Scope {
    vars: HashMap<ast::Ident, (ir::Identifier, Type, bool)>,
}

struct TypeNarrow(Range);

#[derive(Default)]
struct TypeNarrows(HashMap<ast::Ident, TypeNarrow>);

impl TypeNarrow {
    fn narrow_type(&self, operation: ast::Comparisson, other_type: Range) -> Self {
        let (min, max) = match operation {
            ast::Comparisson::Eq => (
                i128::max(self.0.min, other_type.min),
                i128::min(self.0.max, other_type.max),
            ),
            ast::Comparisson::Ne => todo!("Need union type"),
            ast::Comparisson::Lt => (self.0.min, i128::min(self.0.max, other_type.max - 1)),
            ast::Comparisson::Le => (self.0.min, i128::min(self.0.max, other_type.max)),
            ast::Comparisson::Gt => (i128::max(self.0.min, other_type.min - 1), self.0.max),
            ast::Comparisson::Ge => (i128::max(self.0.min, other_type.min), self.0.max),
        };
        let range = Range {
            min,
            max,
            width: self.0.width,
        };

        TypeNarrow(range)
    }

    fn and(self, other: Self) -> Self {
        todo!("Combine narrows");
    }
}

impl TypeNarrows {
    fn narrow(
        self,
        operation: ast::Comparisson,
        (left_expr, left_ast): (TypedExpression, ast::Expression),
        (right_expr, right_ast): (TypedExpression, ast::Expression),
    ) -> Self {
        let TypedExpression::Int(left_range, left_expr) = left_expr else {
            return Self(HashMap::new());
        };
        let TypedExpression::Int(right_range, right_expr) = right_expr else {
            return Self(HashMap::new());
        };

        let complement = match operation {
            ast::Comparisson::Eq => ast::Comparisson::Eq,
            ast::Comparisson::Ne => ast::Comparisson::Ne,
            ast::Comparisson::Lt => ast::Comparisson::Gt,
            ast::Comparisson::Le => ast::Comparisson::Ge,
            ast::Comparisson::Gt => ast::Comparisson::Lt,
            ast::Comparisson::Ge => ast::Comparisson::Le,
        };

        let left_type = TypeNarrow(left_range).narrow_type(operation, right_range);
        let right_type = TypeNarrow(right_range).narrow_type(complement, left_range);

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

    fn and(self, other: Self) -> Self {
        return self;
    }
}

pub struct TypeResolver {
    return_type: Option<Type>,
    scope: Scope,

    function_vars: Vec<(ir::Identifier, Type)>,
}

impl TypeResolver {
    pub fn new() -> Self {
        Self {
            return_type: None,
            scope: Scope::default(),
            function_vars: Vec::new(),
        }
    }

    pub fn resolve_file(&mut self, file: &ast::File) -> ir::File {
        ir::File(
            file.0
                .iter()
                .map(|decl| self.resolve_declaration(decl))
                .collect(),
        )
    }

    fn resolve_declaration(&mut self, decl: &ast::Declaration) -> ir::Declaration {
        match decl {
            ast::Declaration::Function(function) => self.resolve_function(function),
        }
    }

    fn resolve_function(&mut self, func: &ast::FunctionDeclration) -> ir::Declaration {
        match func {
            ast::FunctionDeclration::ExposedFunction {
                name,
                return_type,
                body,
            } => {
                self.function_vars.clear();
                self.scope = Scope::default();

                let return_type = self.resolve_type(return_type);
                self.return_type = Some(return_type);
                let body = self.resolve_body(body);
                self.return_type = None;

                ir::Declaration::Function {
                    name: ir::FunctionName::Named(name.0.clone()),
                    return_type: return_type.underlying(),
                    body,
                    vars: self
                        .function_vars
                        .clone()
                        .into_iter()
                        .map(|(id, type_)| (id, type_.underlying()))
                        .collect(),
                }
            }
        }
    }

    fn resolve_body(&mut self, body: &ast::Body) -> ir::Body {
        let scope_before = self.scope.clone();
        let body = body
            .0
            .iter()
            .map(|stmt| self.resolve_statement(stmt))
            .collect();
        self.scope = scope_before;
        ir::Body(body)
    }

    fn resolve_statement(&mut self, stmt: &ast::Statement) -> ir::Statement {
        match stmt {
            ast::Statement::Return(expr) => {
                let return_type = *self.return_type.as_ref().unwrap();
                let expr = self.resolve_expression(expr).0;
                let (expr_type, expr) = convert_if_possible(expr, &return_type).generic();
                if !return_type.is_sub(&expr_type) {
                    panic!("Incompatible types, {:?} and {:?}", return_type, expr_type);
                }

                ir::Statement::Return(expr)
            }
            ast::Statement::Assert(expr) => {
                let expr = self.resolve_expression(expr).0;
                let expr = expr.bool();

                ir::Statement::Assert(expr)
            }
            ast::Statement::VaribleBinding {
                name,
                type_,
                value,
                mutable,
            } => {
                let id = ir::Identifier::new();
                let type_ = self.resolve_type(type_);
                let value = self.resolve_expression(value).0;
                let (value_type, value) = convert_if_possible(value, &type_).generic();

                if !type_.is_sub(&value_type) {
                    panic!("Type mismatch")
                }

                self.function_vars.push((id, type_));
                self.scope.vars.insert(name.clone(), (id, type_, *mutable));

                ir::Statement::Assign { name: id, value }
            }
            ast::Statement::Assign { name, expr, op } => {
                let (id, type_, mutable) = *self.scope.vars.get(name).unwrap();
                if !mutable {
                    panic!("Cant mutate this value");
                }

                let expr = self.resolve_expression(expr).0;

                if let Some(op) = op {
                    let (expr_type, expr) = expr.int();
                    let type_ = type_.int();

                    let var = ir::IntExpression::LoadVar(id);
                    let (signed, width, results) =
                        self.make_same_type(vec![(type_, var), (expr_type, expr)], 0);
                    let mut results = results.into_iter();
                    let var = results.next().unwrap();
                    let expr = results.next().unwrap();

                    ir::Statement::Assign {
                        name: id,
                        value: ir::Expression::Int(ir::IntExpression::Truncate {
                            value: Box::new(ir::IntExpression::Binary {
                                left: Box::new(var),
                                op: match op {
                                    ast::BinaryOp::Add => ir::IntBinaryOp::Add,
                                    ast::BinaryOp::Sub => ir::IntBinaryOp::Sub,
                                    ast::BinaryOp::Mul => ir::IntBinaryOp::Mul,
                                    ast::BinaryOp::FloorDivision => ir::IntBinaryOp::FloorDivision,
                                    ast::BinaryOp::Mod => ir::IntBinaryOp::Remainder,
                                    _ => panic!("Invalid operator"),
                                },
                                right: Box::new(expr),
                                signed,
                                bounds: Some((type_.min as u64, type_.max as u64)),
                                width,
                            }),
                            target: type_.width,
                        }),
                    }
                } else {
                    if !type_.is_sub(&expr.generic_type()) {
                        panic!("At least give them the same type");
                    }
                    let expr = convert_if_possible(expr, &type_);
                    ir::Statement::Assign {
                        name: id,
                        value: expr.generic().1,
                    }
                }
            }
            ast::Statement::If {
                condition,
                body,
                elif,
                else_block,
            } => {
                let condition = self.resolve_expression(condition).0.bool();
                let body = self.resolve_body(body);
                let elif = elif
                    .iter()
                    .map(|(condition, body)| {
                        (
                            self.resolve_expression(condition).0.bool(),
                            self.resolve_body(body),
                        )
                    })
                    .collect::<Vec<_>>();
                let else_block = else_block.as_ref().map(|body| self.resolve_body(body));

                ir::Statement::If {
                    conditions: [(condition, body)]
                        .into_iter()
                        .chain(elif)
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                    else_block,
                }
            }
            ast::Statement::WhileLoop { condition, body } => {
                let condition = self.resolve_expression(condition).0.bool();
                let body = self.resolve_body(body);

                ir::Statement::WhileLoop { condition, body }
            }
        }
    }

    fn resolve_expression(&mut self, expr: &ast::Expression) -> (TypedExpression, TypeNarrows) {
        match expr {
            ast::Expression::Identifier(ident) => {
                let (id, type_, _) = self.scope.vars.get(ident).unwrap();

                (
                    match type_ {
                        Type::Range(range) => {
                            TypedExpression::Int(*range, ir::IntExpression::LoadVar(*id))
                        }
                        Type::Boolean => TypedExpression::Bool(ir::BoolExpression::LoadVar(*id)),
                    },
                    TypeNarrows::default(),
                )
            }
            ast::Expression::Literal(lit) => match lit {
                ast::Literal::Int(value) => {
                    let value = *value;
                    let range = Range::new(value, value);
                    let width = range.width;
                    let expr = ir::IntExpression::Literal {
                        value: if value >= 0 {
                            value as u64
                        } else {
                            let signed: i64 = value.try_into().unwrap();
                            signed as u64
                        },
                        width,
                    };
                    (TypedExpression::Int(range, expr), TypeNarrows::default())
                }
                ast::Literal::Bool(value) => (
                    TypedExpression::Bool(ir::BoolExpression::Literal(*value)),
                    TypeNarrows::default(),
                ),
            },
            ast::Expression::Prefix(op, expr) => {
                let expr = self.resolve_expression(expr);
                match op {
                    ast::PrefixOp::Not => {
                        let expr = ir::BoolExpression::Not(Box::new(expr.0.bool()));
                        (TypedExpression::Bool(expr), TypeNarrows::default())
                    }
                    ast::PrefixOp::Neg => {
                        let (range, expr) = expr.0.int();
                        let width = min_width_of_range(&range, true);
                        let expr = cast_range_to_width(range, width, expr);

                        let expr = ir::IntExpression::Neg(Box::new(expr));

                        let range = Range {
                            min: -range.max,
                            max: -range.min,
                            width,
                        };
                        (TypedExpression::Int(range, expr), TypeNarrows::default())
                    }
                }
            }
            ast::Expression::Comparison(left, chains) => {
                let left = self.resolve_expression(left);
                let exprs = [left.0.int()]
                    .into_iter()
                    .chain(
                        chains
                            .iter()
                            .map(|(_, expr)| self.resolve_expression(expr).0.int()),
                    )
                    .collect();
                let (signed, width, exprs) = self.make_same_type(exprs, 0);

                let ops = chains.iter().map(|(op, _)| match (signed, op) {
                    (_, ast::Comparisson::Eq) => inkwell::IntPredicate::EQ,
                    (_, ast::Comparisson::Ne) => inkwell::IntPredicate::NE,
                    (false, ast::Comparisson::Lt) => inkwell::IntPredicate::ULT,
                    (false, ast::Comparisson::Gt) => inkwell::IntPredicate::UGT,
                    (false, ast::Comparisson::Le) => inkwell::IntPredicate::ULE,
                    (false, ast::Comparisson::Ge) => inkwell::IntPredicate::UGE,
                    (true, ast::Comparisson::Lt) => inkwell::IntPredicate::SLT,
                    (true, ast::Comparisson::Gt) => inkwell::IntPredicate::SGT,
                    (true, ast::Comparisson::Le) => inkwell::IntPredicate::SLE,
                    (true, ast::Comparisson::Ge) => inkwell::IntPredicate::SGE,
                });

                let mut exprs = exprs.into_iter();
                let left = exprs.next().unwrap();
                let chains = ops.zip(exprs).collect();

                (
                    TypedExpression::Bool(ir::BoolExpression::Comparison(left, chains)),
                    TypeNarrows::default(),
                )
            }
            ast::Expression::Binary(left, op, right) => {
                let (left_range, left) = self.resolve_expression(left).0.int();
                let (right_range, right) = self.resolve_expression(right).0.int();

                let (mut new_range, op) = match op {
                    ast::BinaryOp::Add => (
                        Range::new(
                            left_range.min + right_range.min,
                            left_range.max + right_range.max,
                        ),
                        ir::IntBinaryOp::Add,
                    ),
                    ast::BinaryOp::Sub => (
                        Range::new(
                            left_range.min - right_range.max,
                            left_range.max - right_range.min,
                        ),
                        ir::IntBinaryOp::Sub,
                    ),
                    ast::BinaryOp::Mul => {
                        let bottom = left_range.min * right_range.min;
                        let top = left_range.max * right_range.max;
                        (
                            Range::new(i128::min(top, bottom), i128::max(top, bottom)),
                            ir::IntBinaryOp::Mul,
                        )
                    }
                    ast::BinaryOp::FloorDivision => {
                        // div is really just a mul
                        if right_range.contains(0) {
                            panic!("Dont divide by zero you idiot");
                        }

                        let bottom = left_range.min / right_range.min;
                        let top = left_range.max / right_range.max;
                        (
                            Range::new(i128::min(top, bottom), i128::max(top, bottom)),
                            ir::IntBinaryOp::FloorDivision,
                        )
                    }
                    ast::BinaryOp::Mod => {
                        // a % b = a - b * (a / b)
                        if right_range.contains(0) {
                            panic!("Dont divide by zero you idiot");
                        }

                        let bottom = right_range.min * (left_range.min / right_range.min);
                        let top = right_range.max * (left_range.max / right_range.max);

                        (
                            Range::new(
                                left_range.min - i128::min(top, bottom),
                                right_range.max - i128::max(top, bottom),
                            ),
                            ir::IntBinaryOp::Remainder,
                        )
                    }
                };

                let (signed, width, exprs) = self.make_same_type(
                    vec![(left_range, left), (right_range, right)],
                    new_range.width,
                );
                new_range.width = width;

                let mut exprs = exprs.into_iter();
                let left = exprs.next().unwrap();
                let right = exprs.next().unwrap();

                (
                    TypedExpression::Int(
                        new_range,
                        ir::IntExpression::Binary {
                            left: Box::new(left),
                            op,
                            right: Box::new(right),
                            signed: signed || new_range.signed(),
                            bounds: None,
                            width,
                        },
                    ),
                    TypeNarrows::default(),
                )
            }
        }
    }

    fn resolve_type(&mut self, type_: &ast::Type) -> Type {
        match type_ {
            ast::Type::Range(min, max) => Type::Range(Range::new(*min, *max)),
            ast::Type::Named(name) => match name.0.as_ref() {
                "u8" => Type::Range(Range::new(u8::MIN as i128, u8::MAX as i128)),
                "u16" => Type::Range(Range::new(u16::MIN as i128, u16::MAX as i128)),
                "u32" => Type::Range(Range::new(u32::MIN as i128, u32::MAX as i128)),
                "u64" => Type::Range(Range::new(u64::MIN as i128, u64::MAX as i128)),
                "u128" => Type::Range(Range::new(u128::MIN as i128, u128::MAX as i128)),
                "i8" => Type::Range(Range::new(i8::MIN as i128, i8::MAX as i128)),
                "i16" => Type::Range(Range::new(i16::MIN as i128, i16::MAX as i128)),
                "i32" => Type::Range(Range::new(i32::MIN as i128, i32::MAX as i128)),
                "i64" => Type::Range(Range::new(i64::MIN as i128, i64::MAX as i128)),
                "bool" => Type::Boolean,
                name => panic!("Unknown name {name}"),
            },
        }
    }

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
            .map(|(range, expr)| cast_range_to_width(range, common_width, expr))
            .collect();

        (signed, common_width, exprs)
    }
}
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
        (false, true) => unreachable!("If one of the operands is signed, so must the target."),
    }
}

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

fn truncate_int_maybe(expr: ir::IntExpression, range: Range, target: u8) -> ir::IntExpression {
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

fn convert_if_possible(expr: TypedExpression, target_type: &Type) -> TypedExpression {
    match expr {
        TypedExpression::Int(range, expr) => TypedExpression::Int(
            {
                let mut range = range;
                range.width = target_type.int().width;
                range
            },
            truncate_int_maybe(expr, range, target_type.int().width),
        ),
        TypedExpression::Bool(expr) => TypedExpression::Bool(expr),
    }
}
