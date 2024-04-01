//! This module is responsible for type checking the AST and converting it to the IR.

mod scope;
mod types;

mod expressions;
mod statements;

use crate::{ast, ir, span, Error, Result};

/// Information about a function.
struct FunctionInfo {
    /// The return type of the function.
    return_type: span::Spanned<types::Type>,
    /// The variables in the function.
    vars: Vec<(ir::Identifier, types::Type)>,
}

/// The type resolver.
pub struct TypeResolver {
    /// The current scope.
    scope: scope::Scope,
    /// Function info
    function_info: FunctionInfo,
}

impl TypeResolver {
    /// Create a new type resolver.
    pub fn new() -> Self {
        Self {
            scope: scope::Scope::default(),
            // This is a dummy value that will be replaced by the actual return type
            // This is done instead of using an Option to avoid unneded unwraps
            // Because every location that should access this value should be in a function context
            //
            // TODO: See if we can enforce this invariant with the type system
            function_info: FunctionInfo {
                return_type: span::Spanned::new(types::Type::Boolean, 0, 0),
                vars: Vec::new(),
            },
        }
    }

    /// Create a new scope.
    fn new_scope(&mut self) {
        replace_with::replace_with_or_abort(&mut self.scope, scope::Scope::child);
    }
    /// Pop the current scope.
    fn pop_scope(&mut self) {
        replace_with::replace_with_or_abort(&mut self.scope, scope::Scope::restore);
    }

    /// Resolve a file.
    pub fn resolve_file(&mut self, file: &ast::File) -> Result<ir::File> {
        Ok(ir::File(
            file.0
                .iter()
                .map(|decl| self.resolve_declaration(decl))
                .collect::<Result<_>>()?,
        ))
    }

    /// Resolve a top level declaration.
    fn resolve_declaration(&mut self, decl: &ast::Declaration) -> Result<ir::Declaration> {
        match decl {
            ast::Declaration::Function(function) => self.resolve_function(function),
        }
    }

    /// Resolve a function declaration.
    fn resolve_function(&mut self, func: &ast::FunctionDeclration) -> Result<ir::Declaration> {
        match func {
            ast::FunctionDeclration::Function {
                mangled,
                name,
                arguments,
                return_type,
                body,
            } => {
                let return_type = self.resolve_type(return_type)?;
                self.function_info.return_type = return_type;

                self.new_scope();
                let mut args = Vec::new();
                for arg in arguments.iter() {
                    let type_ = self.resolve_type(&arg.type_)?;
                    let id = ir::Identifier::new();
                    self.function_info.vars.push((id, type_.value));
                    self.scope.insert(
                        arg.name.value.clone(),
                        scope::Variable {
                            id,
                            type_: type_.value,
                            true_type: type_.value,
                            mutable: arg.mutable,
                            span_name: arg.name.span,
                            span_type: arg.type_.span,
                        },
                    );
                    args.push((id, type_.value.underlying()));
                }
                let body = self.resolve_body(body)?;
                self.pop_scope();

                let name = if *mangled {
                    ir::FunctionName::Mangled(ir::Identifier::new())
                } else {
                    ir::FunctionName::Named(name.0.clone())
                };

                Ok(ir::Declaration::Function {
                    name,
                    return_type: return_type.value.underlying(),
                    arguments: args.into_boxed_slice(),
                    body,
                    vars: std::mem::take(&mut self.function_info.vars)
                        .into_iter()
                        .map(|(id, type_)| (id, type_.underlying()))
                        .collect(),
                })
            }
        }
    }

    /// Resolve a body, creating a new scope.
    fn resolve_body(&mut self, body: &ast::Body) -> Result<ir::Body> {
        self.new_scope();
        let body = body
            .0
            .iter()
            .map(|stmt| self.resolve_statement(stmt))
            .collect::<Result<_>>()?;
        self.pop_scope();
        Ok(ir::Body(body))
    }
}
