use crate::ast;
use crate::ir;

#[derive(Clone, Debug)]
enum Type {
    Range(isize, isize),
    Boolean,
}

struct TypedExpression {
    expr: ir::Expression,
    type_: Type,
}

impl TypedExpression {
    fn int(self) -> ir::IntExpression {
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

impl Type {
    fn int_width(&self) -> usize {
        match self {
            Self::Range(min, max) => match () {
                _ if *min >= u8::MIN as isize
                    && *max <= u8::MAX as isize =>
                {
                    8
                }
                _ if *min >= u16::MIN as isize
                    && *max <= u16::MAX as isize =>
                {
                    16
                }
                _ if *min >= u32::MIN as isize
                    && *max <= u32::MAX as isize =>
                {
                    32
                }
                _ if *min >= u64::MIN as isize
                    && *max <= u64::MAX as isize =>
                {
                    64
                }
                _ if *min >= u128::MIN as isize
                    && *max <= u128::MAX as isize =>
                {
                    128
                }
                _ if *min >= i8::MIN as isize
                    && *max <= i8::MAX as isize =>
                {
                    8
                }
                _ if *min >= i16::MIN as isize
                    && *max <= i16::MAX as isize =>
                {
                    16
                }
                _ if *min >= i32::MIN as isize
                    && *max <= i32::MAX as isize =>
                {
                    32
                }
                _ if *min >= i64::MIN as isize
                    && *max <= i64::MAX as isize =>
                {
                    64
                }
                _ if *min >= i128::MIN as isize
                    && *max <= i128::MAX as isize =>
                {
                    128
                }
                _ => panic!("Invalid range"),
            },
            _ => panic!("Not a int"),
        }
    }

    fn underlying(&self) -> ir::Type {
        match self {
            Self::Range(_, _) => ir::Type::Int(self.int_width()),
            Self::Boolean => ir::Type::Bool,
        }
    }

    fn is_sub(&self, other: &Type) -> bool {
        match (self, other) {
            (Self::Range(smin, smax), Self::Range(omin, omax)) => {
                smin <= omin && omax <= smax
            }
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
                    let type_ = Type::Range(value, value);
                    let expr = ir::Expression::Int(
                        ir::IntExpression::Literal {
                            value,
                            width: type_.int_width(),
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
                }
            }
            ast::Expression::Comparison(left, chains) => {
                let left = self.resolve_expression(left).int();
                let chains = chains
                    .iter()
                    .map(|(op, expr)| {
                        let op = match op {
                            ast::Comparisson::Eq => {
                                ir::ComparissonOp::Eq
                            }
                            ast::Comparisson::Ne => {
                                ir::ComparissonOp::Ne
                            }
                            ast::Comparisson::Lt => {
                                ir::ComparissonOp::Lt
                            }
                            ast::Comparisson::Gt => {
                                ir::ComparissonOp::Gt
                            }
                            ast::Comparisson::Ge => {
                                ir::ComparissonOp::Ge
                            }
                            ast::Comparisson::Le => {
                                ir::ComparissonOp::Le
                            }
                        };
                        let expr =
                            self.resolve_expression(expr).int();

                        (op, expr)
                    })
                    .collect();

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
                "u8" => {
                    Type::Range(u8::MIN as isize, u8::MAX as isize)
                }
                "u16" => {
                    Type::Range(u16::MIN as isize, u16::MAX as isize)
                }
                "u32" => {
                    Type::Range(u32::MIN as isize, u32::MAX as isize)
                }
                "u64" => {
                    Type::Range(u64::MIN as isize, u64::MAX as isize)
                }
                "u128" => Type::Range(
                    u128::MIN as isize,
                    u128::MAX as isize,
                ),
                "i8" => {
                    Type::Range(i8::MIN as isize, i8::MAX as isize)
                }
                "i16" => {
                    Type::Range(i16::MIN as isize, i16::MAX as isize)
                }
                "i32" => {
                    Type::Range(i32::MIN as isize, i32::MAX as isize)
                }
                "i64" => {
                    Type::Range(i64::MIN as isize, i64::MAX as isize)
                }
                "i128" => Type::Range(
                    i128::MIN as isize,
                    i128::MAX as isize,
                ),
                "bool" => Type::Boolean,
                name => panic!("Unknown name {name}"),
            },
        }
    }
}
