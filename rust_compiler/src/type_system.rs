use crate::ast;
use crate::ir;

#[derive(Clone, Debug)]
enum Type {
    Range(isize, isize),
}

struct TypedExpression {
    expr: ir::Expression,
    type_: Type,
}

impl TypedExpression {
    fn int(&self) -> &ir::IntExpression {
        match &self.expr {
            ir::Expression::Int(expr) => expr,
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
                // TODO: More ranges
                _ => panic!("Invalid range"),
            },
            _ => panic!("Not a int"),
        }
    }

    fn underlying(&self) -> ir::Type {
        match self {
            Self::Range(_, _) => ir::Type::Int(self.int_width()),
        }
    }

    fn is_sub(&self, other: &Type) -> bool {
        match (self, other) {
            (Self::Range(smin, smax), Self::Range(omin, omax)) => {
                smin <= omin && omax <= smax
            }
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
                .into_iter()
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
            },
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
                name => panic!("Unknown name {name}"),
            },
        }
    }
}
