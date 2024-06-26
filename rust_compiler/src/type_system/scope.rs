//! Scope management.

use std::collections::HashMap;

use super::types::Type;
use crate::{ast, ir, span, CompileError, Result};

/// A variable in the scope.
#[derive(Clone, Copy)]
pub struct Variable {
    /// The identifier of the variable.
    pub id: ir::Identifier,
    /// The type of the variable.
    /// This might be a sub type in if-statements because of being narrowed down.
    /// This should be used when as the type of a expression of the variable.
    pub type_: Type,
    /// The actual type of the value that isnt narrowed.
    /// This should be used in assignments and other places where the actual type is needed.
    pub true_type: Type,
    /// If the variable is mutable.
    pub mutable: bool,
    /// The span of the variable name in the declaration.
    pub span_name: span::Span,
    /// The span of the variable type in the declaration.
    pub span_type: span::Span,
}

/// Function info
#[derive(Clone)]
pub struct FunctionInfo {
    /// the name
    pub name: ir::FunctionName,
    /// The return type of the function.
    pub return_type: span::Spanned<Type>,
    /// The arguments to the function.
    pub arguments: Box<[(ast::Ident, Variable)]>,
}

/// A scope item
pub enum Item {
    /// A variable.
    Variable(Variable),
    /// A function.
    Function(FunctionInfo),
}

impl Item {
    /// Get the variable from the item or return an error.
    pub fn variable(&self, span: span::Span) -> Result<&Variable> {
        if let Item::Variable(var) = self {
            Ok(var)
        } else {
            Err(CompileError::InvalidScopeItem {
                span: span.into(),
                expected: "variable".to_owned(),
            })
        }
    }

    /// Get the function from the item or return an error.
    pub fn function(&self, span: span::Span) -> Result<&FunctionInfo> {
        if let Item::Function(func) = self {
            Ok(func)
        } else {
            Err(CompileError::InvalidScopeItem {
                span: span.into(),
                expected: "function".to_owned(),
            })
        }
    }
}

/// A scope of variables.
#[derive(Default)]
pub struct Scope {
    /// The variables in the scope.
    vars: HashMap<ast::Ident, Item>,
    /// Parent scope.
    parent: Option<Box<Scope>>,
}

impl Scope {
    /// Create a new child scope.
    pub fn child(self) -> Self {
        Self {
            vars: HashMap::new(),
            parent: Some(Box::new(self)),
        }
    }
    /// Restore the parent scope.
    pub fn restore(self) -> Self {
        *self.parent.unwrap_or_default()
    }
    /// Get a variable from the scope.
    pub fn get(&self, ident: &ast::Ident) -> Option<&Item> {
        self.vars.get(ident).or_else(|| {
            let parent = self.parent.as_ref()?;
            parent.get(ident)
        })
    }
    /// Insert a variable into the scope.
    pub fn insert(&mut self, ident: ast::Ident, var: Item) {
        self.vars.insert(ident, var);
    }
}
