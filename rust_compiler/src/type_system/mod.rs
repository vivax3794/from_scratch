//! This module is responsible for type checking the AST and converting it to the IR.

mod scope;
mod types;

mod expressions;
mod statements;

use std::mem;

use crate::{ast, ir, span, Result};

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
        let mut funcs = Vec::new();
        for item in file.0.iter() {
            match item {
                ast::Declaration::Function(func) => {
                    let (func, body) = self.resolve_function(func)?;
                    funcs.push((func, body));
                }
            }
        }

        let mut decls = Vec::new();
        for (func, body) in funcs {
            decls.push(self.resolve_function_body(func, body)?);
        }

        Ok(ir::File(decls.into_boxed_slice()))
    }

    /// Resolve a function declaration.
    fn resolve_function<'a>(
        &mut self,
        func: &'a ast::FunctionDeclration,
    ) -> Result<(scope::FunctionInfo, &'a ast::Body)> {
        match func {
            ast::FunctionDeclration::Function {
                mangled,
                name,
                arguments,
                return_type,
                body,
            } => {
                let return_type = self.resolve_type(return_type)?;
                let mut args_vars = Vec::new();
                for arg in arguments.iter() {
                    let type_ = self.resolve_type(&arg.type_)?;
                    let id = ir::Identifier::new();

                    let var = scope::Variable {
                        id,
                        type_: type_.value,
                        true_type: type_.value,
                        mutable: arg.mutable,
                        span_name: arg.name.span,
                        span_type: arg.type_.span,
                    };

                    args_vars.push((arg.name.value.clone(), var));
                }

                let ir_name = if *mangled {
                    ir::FunctionName::Mangled(ir::Identifier::new())
                } else {
                    ir::FunctionName::Named(name.0.clone())
                };

                let func = scope::FunctionInfo {
                    name: ir_name,
                    arguments: args_vars.into_boxed_slice(),
                    return_type,
                };

                self.scope
                    .insert(name.clone(), scope::Item::Function(func.clone()));
                Ok((func, body))
            }
        }
    }

    /// Resolve a function body
    fn resolve_function_body(
        &mut self,
        func: scope::FunctionInfo,
        body: &ast::Body,
    ) -> Result<ir::Declaration> {
        self.function_info.return_type = func.return_type;

        self.new_scope();
        for (name, arg) in func.arguments.iter() {
            self.scope.insert(name.clone(), scope::Item::Variable(*arg));
        }
        let body = self.resolve_body(body)?;
        self.pop_scope();

        Ok(ir::Declaration::Function {
            name: func.name,
            arguments: func
                .arguments
                .iter()
                .map(|(_, v)| (v.id, v.type_.underlying()))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            return_type: func.return_type.value.underlying(),
            body,
            vars: mem::take(&mut self.function_info.vars)
                .into_iter()
                .map(|(id, ty)| (id, ty.underlying()))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        })
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
