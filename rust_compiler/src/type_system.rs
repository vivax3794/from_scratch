use crate::ast;
use crate::ir;

#[derive(Clone, Debug, Copy)]
struct Range {
    min: i128,
    max: i128,
    width: u8,
}

#[derive(Clone, Debug)]
enum Type {
    Range(Range),
    Boolean,
}

#[derive(Debug)]
struct TypedExpression {
    expr: ir::Expression,
    type_: Type,
}

impl TypedExpression {
    fn int(self) -> (Range, ir::IntExpression) {
        match (self.type_, self.expr) {
            (Type::Range(range), ir::Expression::Int(expr)) => {
                (range, expr)
            }
            _ => panic!("Not a int"),
        }
    }

    fn int_value(self) -> ir::IntExpression {
        match self.expr {
            ir::Expression::Int(expr) => expr,
            _ => panic!("Expected int"),
        }
    }
    fn bool(self) -> ir::BoolExpression {
        match self.expr {
            ir::Expression::Bool(expr) => expr,
            _ => panic!("Expected bool"),
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

    fn width(min: i128, max: i128) -> u8 {
        match () {
            _ if min >= u8::MIN as i128 && max <= u8::MAX as i128 => {
                8
            }
            _ if min >= u16::MIN as i128
                && max <= u16::MAX as i128 =>
            {
                16
            }
            _ if min >= u32::MIN as i128
                && max <= u32::MAX as i128 =>
            {
                32
            }
            _ if min >= u64::MIN as i128
                && max <= u64::MAX as i128 =>
            {
                64
            }
            _ if min >= u128::MIN as i128
                && max <= u128::MAX as i128 =>
            {
                128
            }
            _ if min >= i8::MIN as i128 && max <= i8::MAX as i128 => {
                8
            }
            _ if min >= i16::MIN as i128
                && max <= i16::MAX as i128 =>
            {
                16
            }
            _ if min >= i32::MIN as i128
                && max <= i32::MAX as i128 =>
            {
                32
            }
            _ if min >= i64::MIN as i128
                && max <= i64::MAX as i128 =>
            {
                64
            }
            _ => panic!("Invalid range"),
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

pub struct TypeResolver {
    return_type: Option<Type>,
}

impl TypeResolver {
    pub fn new() -> Self {
        Self { return_type: None }
    }

    pub fn resolve_file(&mut self, file: &ast::File) -> ir::File {
        ir::File(
            file.0
                .iter()
                .map(|decl| self.resolve_declaration(decl))
                .collect(),
        )
    }

    fn resolve_declaration(
        &mut self,
        decl: &ast::Declaration,
    ) -> ir::Declaration {
        match decl {
            ast::Declaration::Function(function) => {
                self.resolve_function(function)
            }
        }
    }

    fn resolve_function(
        &mut self,
        func: &ast::FunctionDeclration,
    ) -> ir::Declaration {
        match func {
            ast::FunctionDeclration::ExposedFunction {
                name,
                return_type,
                body,
            } => {
                let return_type = self.resolve_type(return_type);
                self.return_type = Some(return_type.clone());
                let body = body
                    .0
                    .iter()
                    .map(|stmt| self.resolve_statement(stmt))
                    .collect();
                self.return_type = None;

                ir::Declaration::Function {
                    name: ir::FunctionName::Named(name.0.clone()),
                    return_type: return_type.underlying(),
                    body: ir::Body(body),
                }
            }
        }
    }

    fn resolve_statement(
        &mut self,
        stmt: &ast::Statement,
    ) -> ir::Statement {
        match stmt {
            ast::Statement::Return(expr) => {
                let return_type =
                    self.return_type.as_ref().unwrap().clone();
                let expr = self.resolve_expression(expr);
                if !return_type.is_sub(&expr.type_) {
                    panic!(
                        "Incompatible types, {:?} and {:?}",
                        return_type, expr.type_
                    );
                }

                ir::Statement::Return(expr.expr)
            }
            ast::Statement::Assert(expr) => {
                let expr = self.resolve_expression(expr);
                let expr = expr.bool();

                ir::Statement::Assert(expr)
            }
        }
    }

    fn resolve_expression(
        &mut self,
        expr: &ast::Expression,
    ) -> TypedExpression {
        match expr {
            ast::Expression::Literal(lit) => match lit {
                ast::Literal::Int(value) => {
                    let value = *value;
                    let range = Range::new(value, value);
                    let width = range.width;
                    let type_ = Type::Range(range);
                    let expr = ir::Expression::Int(
                        ir::IntExpression::Literal {
                            value: if value >= 0 {
                                value as u64
                            } else {
                                let signed: i64 =
                                    value.try_into().unwrap();
                                signed as u64
                            },
                            width,
                        },
                    );
                    TypedExpression { expr, type_ }
                }
                ast::Literal::Bool(value) => {
                    let type_ = Type::Boolean;
                    let expr = ir::Expression::Bool(
                        ir::BoolExpression::Literal(*value),
                    );
                    TypedExpression { expr, type_ }
                }
            },
            ast::Expression::Prefix(op, expr) => {
                let expr = self.resolve_expression(expr);
                match op {
                    ast::PrefixOp::Not => {
                        let expr = ir::BoolExpression::Not(Box::new(
                            expr.bool(),
                        ));
                        TypedExpression {
                            expr: ir::Expression::Bool(expr),
                            type_: Type::Boolean,
                        }
                    }
                    ast::PrefixOp::Neg => {
                        let (range, expr) = expr.int();
                        let width = min_width_of_range(&range, true);
                        let expr =
                            cast_range_to_width(range, width, expr);

                        let range = Range {
                            min: -range.max,
                            max: -range.min,
                            width,
                        };
                        TypedExpression {
                            type_: Type::Range(range),
                            expr: ir::Expression::Int(expr),
                        }
                    }
                }
            }
            ast::Expression::Comparison(left, chains) => {
                let left = self.resolve_expression(left);
                let exprs = [left.int()]
                    .into_iter()
                    .chain(chains.iter().map(|(_, expr)| {
                        self.resolve_expression(expr).int()
                    }))
                    .collect();
                let (signed, width, exprs) =
                    self.make_same_type(exprs, 0);

                let ops = chains.into_iter().map(|(op, _)| {
                    match (signed, op) {
                        (_, ast::Comparisson::Eq) => {
                            inkwell::IntPredicate::EQ
                        }
                        (_, ast::Comparisson::Ne) => {
                            inkwell::IntPredicate::NE
                        }
                        (false, ast::Comparisson::Lt) => {
                            inkwell::IntPredicate::ULT
                        }
                        (false, ast::Comparisson::Gt) => {
                            inkwell::IntPredicate::UGT
                        }
                        (false, ast::Comparisson::Le) => {
                            inkwell::IntPredicate::ULE
                        }
                        (false, ast::Comparisson::Ge) => {
                            inkwell::IntPredicate::UGE
                        }
                        (true, ast::Comparisson::Lt) => {
                            inkwell::IntPredicate::SLT
                        }
                        (true, ast::Comparisson::Gt) => {
                            inkwell::IntPredicate::SGT
                        }
                        (true, ast::Comparisson::Le) => {
                            inkwell::IntPredicate::SLE
                        }
                        (true, ast::Comparisson::Ge) => {
                            inkwell::IntPredicate::SGE
                        }
                    }
                });

                let mut exprs = exprs.into_iter();
                let left = exprs.next().unwrap();
                let chains = ops.zip(exprs).collect();

                TypedExpression {
                    type_: Type::Boolean,
                    expr: ir::Expression::Bool(
                        ir::BoolExpression::Comparison(left, chains),
                    ),
                }
            }
        }
    }

    fn resolve_type(&mut self, type_: &ast::Type) -> Type {
        match type_ {
            ast::Type::Named(name) => match name.0.as_ref() {
                "u8" => Type::Range(Range::new(
                    u8::MIN as i128,
                    u8::MAX as i128,
                )),
                "u16" => Type::Range(Range::new(
                    u16::MIN as i128,
                    u16::MAX as i128,
                )),
                "u32" => Type::Range(Range::new(
                    u32::MIN as i128,
                    u32::MAX as i128,
                )),
                "u64" => Type::Range(Range::new(
                    u64::MIN as i128,
                    u64::MAX as i128,
                )),
                "u128" => Type::Range(Range::new(
                    u128::MIN as i128,
                    u128::MAX as i128,
                )),
                "i8" => Type::Range(Range::new(
                    i8::MIN as i128,
                    i8::MAX as i128,
                )),
                "i16" => Type::Range(Range::new(
                    i16::MIN as i128,
                    i16::MAX as i128,
                )),
                "i32" => Type::Range(Range::new(
                    i32::MIN as i128,
                    i32::MAX as i128,
                )),
                "i64" => Type::Range(Range::new(
                    i64::MIN as i128,
                    i64::MAX as i128,
                )),
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
            .map(|(range, expr)| {
                cast_range_to_width(range, common_width, expr)
            })
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
        (false, true) => unreachable!(
            "If one of the operands is signed, so must the target."
        ),
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
        ir::IntExpression::UpCastWidth {
            value: Box::new(expr),
            target: target_width,
            signed: type_.signed(),
        }
    }
}
