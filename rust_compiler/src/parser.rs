//! This module contains the parser for the language.

use std::collections::HashMap;

use lazy_static::lazy_static;
use maplit::hashmap;

use crate::lexer::Token;
use crate::span::Spanned;
use crate::{ast, Error, Result};

/// The type of operator
enum Operator {
    /// A prefix operator
    Prefix(HashMap<Token, ast::PrefixOp>),
    /// A binary operator
    Binary(HashMap<Token, ast::BinaryOp>),
    /// A comparison operator
    Comparison(HashMap<Token, ast::ComparissonOp>),
}

lazy_static! {
    /// The operators defined in precedence order
    static ref OPERATORS: Vec<Operator> = vec![
        Operator::Comparison(hashmap! {
            Token::EqEq => ast::ComparissonOp::Eq,
            Token::BangEq => ast::ComparissonOp::Ne,
            Token::Lt => ast::ComparissonOp::Lt,
            Token::LtEq => ast::ComparissonOp::Le,
            Token::Gt => ast::ComparissonOp::Gt,
            Token::GtEq => ast::ComparissonOp::Ge,
        }),
        Operator::Binary(hashmap! {
            Token::Plus => ast::BinaryOp::Add,
            Token::Minus => ast::BinaryOp::Sub,
        }),
        Operator::Binary(hashmap! {
            Token::Star => ast::BinaryOp::Mul,
            Token::SlashSlash => ast::BinaryOp::FloorDivision,
            Token::Mod => ast::BinaryOp::Mod
        }),
        Operator::Binary(hashmap! {
            Token::StarStar => ast::BinaryOp::Pow,
        }),
        Operator::Prefix(hashmap! {
            Token::Minus => ast::PrefixOp::Neg,
            Token::Bang => ast::PrefixOp::Not
        })
    ];
}

/// The parser
pub struct Parser {
    /// The code to parse
    code: std::collections::VecDeque<Spanned<Token>>,
}

impl Parser {
    /// Create a new parser
    pub fn new(tokens: Vec<Spanned<Token>>) -> Self {
        Self {
            code: tokens.into(),
        }
    }

    /// Expects a token and removes it from the code
    // It is a better api to take the token by value as it avoids having & at the call site
    #[allow(clippy::needless_pass_by_value)]
    fn expect(&mut self, token: Token) -> Result<crate::span::Span> {
        #[allow(clippy::expect_used)]
        let t = self
            .code
            .pop_front()
            .expect("We should never continue parsing after a EOF");
        if t.value == token {
            Ok(t.span)
        } else {
            let span = t.span.into();
            Err(Error::UnexpectedToken {
                token: t.value,
                expected: format!("{token:?}"),
                span,
            })
        }
    }

    /// Peeks the next token including the span
    fn peek_spanned(&self) -> &Spanned<Token> {
        #[allow(clippy::expect_used)]
        self.code
            .front()
            .expect("We should never continue parsing after a EOF")
    }

    /// Peeks the next token
    fn peek(&self) -> &Token {
        &self.peek_spanned().value
    }

    /// get the next spanned token
    fn next_spanned(&mut self) -> Spanned<Token> {
        #[allow(clippy::expect_used)]
        self.code
            .pop_front()
            .expect("We should never continue parsing after a EOF")
    }

    /// Gets the next token
    fn next(&mut self) -> Token {
        self.next_spanned().value
    }

    /// Parses the code
    pub fn parse(mut self) -> Result<ast::File> {
        let mut stmts = Vec::new();
        while self.peek() != &Token::Eof {
            stmts.push(self.parse_declaration()?);
        }
        Ok(ast::File(stmts.into_boxed_slice()))
    }

    /// Parses a top level declaration
    fn parse_declaration(&mut self) -> Result<ast::Declaration> {
        self.expect(Token::Expose)?;
        let name = self.parse_ident()?;
        self.expect(Token::OpenBracket)?;
        self.expect(Token::CloseBracket)?;
        let return_type = self.parse_type()?;
        let body = self.parse_body()?;

        Ok(ast::Declaration::Function(
            ast::FunctionDeclration::ExposedFunction {
                name: name.value,
                return_type,
                body,
            },
        ))
    }

    /// Parses an identifier
    fn parse_ident(&mut self) -> Result<Spanned<ast::Ident>> {
        let token = self.next_spanned();
        let span = token.span;
        let Token::Ident(ident) = token.value else {
            let span = token.span.into();
            return Err(Error::UnexpectedToken {
                token: token.value,
                expected: "identifier".to_owned(),
                span,
            });
        };
        Ok(span.with_value(ast::Ident(ident)))
    }

    /// Parses a type
    fn parse_type(&mut self) -> Result<Spanned<ast::Type>> {
        if let Token::Ident(_) = self.peek() {
            let ident = self.parse_ident()?;
            Ok(ident.span.with_value(ast::Type::Named(ident.value)))
        } else {
            let min = self.parse_num()?;
            self.expect(Token::DotDot)?;
            let max = self.parse_num()?;
            Ok(min
                .span
                .combine(max.span)
                .with_value(ast::Type::Range(min.value, max.value)))
        }
    }

    /// Parses a number
    fn parse_num(&mut self) -> Result<Spanned<i128>> {
        let token = self.next_spanned();
        match token.value {
            Token::Minus => {
                let num = self.parse_num()?;
                Ok(token.span.combine(num.span).with_value(-num.value))
            }
            Token::Number(num) => Ok(token.span.with_value(num)),
            _ => {
                let span = token.span.into();
                Err(Error::UnexpectedToken {
                    token: token.value,
                    expected: "number".to_owned(),
                    span,
                })
            }
        }
    }

    /// Parses a body including the curly braces
    fn parse_body(&mut self) -> Result<ast::Body> {
        let mut stmt = Vec::new();
        self.expect(Token::OpenCurly)?;
        while self.peek() != &Token::CloseCurly {
            stmt.push(self.parse_stmt()?);
        }
        self.expect(Token::CloseCurly)?;
        Ok(ast::Body(stmt.into_boxed_slice()))
    }

    /// Parses a statement
    fn parse_stmt(&mut self) -> Result<ast::Statement> {
        match self.peek() {
            Token::Assert => self.parse_assert(),
            Token::AssertType => self.parse_assert_type(),
            Token::Return => self.parse_return(),
            Token::While => self.parse_while(),
            Token::Let => self.parse_let(),
            Token::If => self.parse_if(),
            _ => self.parse_maybe_assignment(),
        }
    }

    /// Parses a let statement
    fn parse_let(&mut self) -> Result<ast::Statement> {
        self.expect(Token::Let)?;

        let mutable = match self.peek() {
            Token::Mut => {
                self.next();
                true
            }
            _ => false,
        };

        let name = self.parse_ident()?;
        self.expect(Token::Colon)?;
        let type_ = self.parse_type()?;
        self.expect(Token::Eq)?;
        let expression = self.parse_expr()?;
        self.expect(Token::SemiColon)?;

        Ok(ast::Statement::VaribleBinding {
            name,
            type_,
            value: expression,
            mutable,
        })
    }

    /// Parses a while loop
    fn parse_while(&mut self) -> Result<ast::Statement> {
        self.expect(Token::While)?;
        let condition = self.parse_expr()?;
        let body = self.parse_body()?;

        Ok(ast::Statement::WhileLoop { condition, body })
    }

    /// Parses an assert statement
    fn parse_assert(&mut self) -> Result<ast::Statement> {
        let assert_span = self.expect(Token::Assert)?;
        let expr = self.parse_expr()?;
        self.expect(Token::SemiColon)?;

        Ok(ast::Statement::Assert(expr, assert_span))
    }

    /// Parses an assert type statement
    fn parse_assert_type(&mut self) -> Result<ast::Statement> {
        self.expect(Token::AssertType)?;
        let expr = self.parse_expr()?;
        self.expect(Token::Eq)?;
        let type_ = self.parse_type()?;
        self.expect(Token::SemiColon)?;

        Ok(ast::Statement::AssertType(type_, expr))
    }

    /// Parses a return statement
    fn parse_return(&mut self) -> Result<ast::Statement> {
        self.expect(Token::Return)?;
        let expr = self.parse_expr()?;
        self.expect(Token::SemiColon)?;

        Ok(ast::Statement::Return(expr))
    }

    /// Parses an if statement
    fn parse_if(&mut self) -> Result<ast::Statement> {
        self.expect(Token::If)?;
        let condition = self.parse_expr()?;
        let then_body = self.parse_body()?;

        let mut elif = Vec::new();
        let mut else_block = None;
        while let Token::Else = self.peek() {
            self.expect(Token::Else)?;
            if self.peek() == &Token::If {
                self.expect(Token::If)?;
                let condition = self.parse_expr()?;
                let body = self.parse_body()?;
                elif.push((condition, body));
            } else {
                else_block = Some(self.parse_body()?);
                break;
            }
        }

        Ok(ast::Statement::If {
            condition,
            body: then_body,
            elif: elif.into_boxed_slice(),
            else_block,
        })
    }

    /// Parses an assignment or an expression
    fn parse_maybe_assignment(&mut self) -> Result<ast::Statement> {
        let target = self.parse_expr()?;
        let op = match self.peek() {
            Token::Eq => Some(None),
            Token::PlusEq => Some(Some(ast::BinaryOp::Add)),
            Token::MinusEq => Some(Some(ast::BinaryOp::Sub)),
            Token::StarEq => Some(Some(ast::BinaryOp::Mul)),
            Token::DivEq => Some(Some(ast::BinaryOp::FloorDivision)),
            _ => None,
        };
        let result = if let Some(op) = op {
            let op_span = self.code.pop_front().unwrap().span;
            let expr = self.parse_expr()?;
            ast::Statement::Assign {
                target,
                op: op.map(|op| op_span.with_value(op)),
                expr,
            }
        } else {
            ast::Statement::Expr(target)
        };
        self.expect(Token::SemiColon)?;
        Ok(result)
    }

    /// Parses an expression
    fn parse_expr(&mut self) -> Result<Spanned<ast::Expression>> {
        self.parse_operator(0)
    }

    /// Parses an operator expression
    fn parse_operator(&mut self, level: usize) -> Result<Spanned<ast::Expression>> {
        let Some(operators) = OPERATORS.get(level) else {
            return self.parse_group();
        };

        match operators {
            Operator::Prefix(operators) => {
                if let Some(op) = operators.get(self.peek()) {
                    let token = self.next_spanned();
                    let expr = self.parse_operator(level)?;
                    let span = token.span.combine(expr.span);
                    let result = ast::Expression::Prefix(*op, Box::new(expr));
                    Ok(span.with_value(result))
                } else {
                    self.parse_operator(level + 1)
                }
            }
            Operator::Binary(operators) => {
                let mut left = self.parse_operator(level + 1)?;
                while let Some(op) = operators.get(self.peek()) {
                    self.code.pop_front();
                    let right = self.parse_operator(level + 1)?;
                    let span = left.span.combine(right.span);
                    left = span.with_value(ast::Expression::Binary(
                        Box::new(left),
                        *op,
                        Box::new(right),
                    ));
                }

                Ok(left)
            }
            Operator::Comparison(operators) => {
                let left = self.parse_operator(level + 1)?;
                let mut chains = Vec::new();
                while let Some(op) = operators.get(self.peek()) {
                    self.code.pop_front();
                    let right = self.parse_operator(level + 1)?;
                    chains.push((*op, Box::new(right)));
                }

                if chains.is_empty() {
                    Ok(left)
                } else {
                    #[allow(clippy::expect_used)]
                    let last = &chains.last().expect("We have at least one element").1;
                    let span = left.span.combine(last.span);
                    Ok(span.with_value(ast::Expression::Comparison(Box::new(left), chains)))
                }
            }
        }
    }

    /// Parses a parenthesized expression or a literal
    fn parse_group(&mut self) -> Result<Spanned<ast::Expression>> {
        match self.peek() {
            Token::OpenBracket => {
                self.next();
                let expr = self.parse_expr()?;
                self.expect(Token::CloseBracket)?;
                Ok(expr)
            }
            _ => self.parse_literal(),
        }
    }

    /// Parses a literal or an identifier
    fn parse_literal(&mut self) -> Result<Spanned<ast::Expression>> {
        let token = self.next_spanned();
        let result = match token.value {
            Token::Number(num) => ast::Expression::Literal(ast::Literal::Int(num)),
            Token::True => ast::Expression::Literal(ast::Literal::Bool(true)),
            Token::False => ast::Expression::Literal(ast::Literal::Bool(false)),
            Token::Ident(ident) => ast::Expression::Identifier(ast::Ident(ident)),
            _ => {
                let span = token.span.into();
                return Err(Error::UnexpectedToken {
                    token: token.value,
                    expected: "literal or identifier".to_owned(),
                    span,
                });
            }
        };
        Ok(token.span.with_value(result))
    }
}
