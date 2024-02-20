use std::collections::HashMap;

use lazy_static::lazy_static;
use maplit::hashmap;

use crate::ast;
use crate::lexer::Token;

enum Operator {
    Prefix(HashMap<Token, ast::PrefixOp>),
    Binary(HashMap<Token, ast::BinaryOp>),
    Comparison(HashMap<Token, ast::ComparissonOp>),
}

lazy_static! {
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
        Operator::Prefix(hashmap! {
            Token::Minus => ast::PrefixOp::Neg,
            Token::Bang => ast::PrefixOp::Not
        })
    ];
}

pub struct Parser {
    code: std::collections::VecDeque<Token>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            code: tokens.into(),
        }
    }

    fn expect(&mut self, token: Token) {
        let t = self.code.pop_front().unwrap();
        if t != token {
            panic!("Expected {token:?} found {t:?}.")
        }
    }

    pub fn parse(mut self) -> ast::File {
        let mut stmts = Vec::new();
        while self.code.get(0).unwrap() != &Token::Eof {
            stmts.push(self.parse_declaration());
        }
        ast::File(stmts.into_boxed_slice())
    }

    fn parse_declaration(&mut self) -> ast::Declaration {
        self.expect(Token::Expose);
        let name = self.parse_ident();
        self.expect(Token::OpenBracket);
        self.expect(Token::CloseBracket);
        let return_type = self.parse_type();
        let body = self.parse_body();

        ast::Declaration::Function(
            ast::FunctionDeclration::ExposedFunction {
                name,
                return_type,
                body,
            },
        )
    }

    fn parse_ident(&mut self) -> ast::Ident {
        let Token::Ident(ident) = self.code.pop_front().unwrap()
        else {
            panic!("expected ident")
        };
        ast::Ident(ident)
    }

    fn parse_type(&mut self) -> ast::Type {
        match self.code.get(0).unwrap() {
            Token::Ident(_) => ast::Type::Named(self.parse_ident()),
            _ => {
                let min = self.parse_num();
                self.expect(Token::DotDot);
                let max = self.parse_num();
                ast::Type::Range(min, max)
            }
        }
    }

    fn parse_num(&mut self) -> i128 {
        match self.code.pop_front().unwrap() {
            Token::Minus => -self.parse_num(),
            Token::Number(num) => num,
            t => panic!("Not a valid number, got {t:?}"),
        }
    }

    fn parse_body(&mut self) -> ast::Body {
        let mut stmt = Vec::new();
        self.expect(Token::OpenCurly);
        while self.code.get(0).unwrap() != &Token::CloseCurly {
            stmt.push(self.parse_stmt());
        }
        self.expect(Token::CloseCurly);
        ast::Body(stmt.into_boxed_slice())
    }

    fn parse_stmt(&mut self) -> ast::Statement {
        match self.code.get(0).unwrap() {
            Token::Assert => self.parse_assert(),
            Token::Return => self.parse_return(),
            Token::While => self.parse_while(),
            Token::Let => self.parse_let(),
            Token::If => self.parse_if(),
            _ => self.parse_maybe_assignment(),
        }
    }

    fn parse_let(&mut self) -> ast::Statement {
        self.expect(Token::Let);

        let mutable = match self.code.get(0).unwrap() {
            Token::Mut => {
                self.code.pop_front();
                true
            }
            _ => false,
        };

        let name = self.parse_ident();
        self.expect(Token::Colon);
        let type_ = self.parse_type();
        self.expect(Token::Eq);
        let expression = self.parse_expr();
        self.expect(Token::SemiColon);

        ast::Statement::VaribleBinding {
            name,
            type_,
            value: expression,
            mutable,
        }
    }

    fn parse_while(&mut self) -> ast::Statement {
        self.expect(Token::While);
        let condition = self.parse_expr();
        let body = self.parse_body();

        ast::Statement::WhileLoop { condition, body }
    }

    fn parse_assert(&mut self) -> ast::Statement {
        self.expect(Token::Assert);
        let expr = self.parse_expr();
        self.expect(Token::SemiColon);

        ast::Statement::Assert(expr)
    }

    fn parse_return(&mut self) -> ast::Statement {
        self.expect(Token::Return);
        let expr = self.parse_expr();
        self.expect(Token::SemiColon);

        ast::Statement::Return(expr)
    }

    fn parse_if(&mut self) -> ast::Statement {
        self.expect(Token::If);
        let condition = self.parse_expr();
        let then_body = self.parse_body();

        let mut elif = Vec::new();
        let mut else_block = None;
        loop {
            match self.code.get(0).unwrap() {
                Token::Else => {
                    self.expect(Token::Else);
                    match self.code.get(0).unwrap() {
                        Token::If => {
                            self.expect(Token::If);
                            let condition = self.parse_expr();
                            let body = self.parse_body();
                            elif.push((condition, body));
                        }
                        _ => {
                            else_block = Some(self.parse_body());
                            break;
                        }
                    }
                }
                _ => break,
            }
        }

        ast::Statement::If {
            condition,
            body: then_body,
            elif: elif.into_boxed_slice(),
            else_block,
        }
    }

    fn parse_maybe_assignment(&mut self) -> ast::Statement {
        let target = self.parse_expr();
        let op = match self.code.get(0).unwrap() {
            Token::Eq => Some(None),
            Token::PlusEq => Some(Some(ast::BinaryOp::Add)),
            Token::MinusEq => Some(Some(ast::BinaryOp::Sub)),
            Token::StarEq => Some(Some(ast::BinaryOp::Mul)),
            Token::DivEq => Some(Some(ast::BinaryOp::FloorDivision)),
            _ => None,
        };
        let result = if let Some(op) = op {
            self.code.pop_front();
            let expr = self.parse_expr();
            ast::Statement::Assign { target, op, expr }
        } else {
            ast::Statement::Expr(target)
        };
        self.expect(Token::SemiColon);
        result
    }

    fn parse_expr(&mut self) -> ast::Expression {
        self.parse_operator(0)
    }

    fn parse_operator(&mut self, level: usize) -> ast::Expression {
        let Some(operators) = OPERATORS.get(level) else {
            return self.parse_group();
        };

        match operators {
            Operator::Prefix(operators) => {
                let token = self.code.get(0).unwrap();
                if let Some(op) = operators.get(token) {
                    self.code.pop_front();
                    let expr = self.parse_operator(level);
                    ast::Expression::Prefix(*op, Box::new(expr))
                } else {
                    self.parse_operator(level + 1)
                }
            }
            Operator::Binary(operators) => {
                let mut left = self.parse_operator(level + 1);
                loop {
                    match operators.get(self.code.get(0).unwrap()) {
                        Some(op) => {
                            self.code.pop_front();
                            let right =
                                self.parse_operator(level + 1);
                            left = ast::Expression::Binary(
                                Box::new(left),
                                *op,
                                Box::new(right),
                            )
                        }
                        None => break,
                    }
                }

                left
            }
            Operator::Comparison(operators) => {
                let left = self.parse_operator(level + 1);
                let mut chains = Vec::new();
                loop {
                    match operators.get(self.code.get(0).unwrap()) {
                        Some(op) => {
                            self.code.pop_front();
                            let right =
                                self.parse_operator(level + 1);
                            chains.push((*op, Box::new(right)))
                        }
                        None => break,
                    }
                }

                if chains.is_empty() {
                    left
                } else {
                    ast::Expression::Comparison(
                        Box::new(left),
                        chains,
                    )
                }
            }
        }
    }

    fn parse_group(&mut self) -> ast::Expression {
        match self.code.get(0).unwrap() {
            Token::OpenBracket => {
                self.code.pop_front();
                let expr = self.parse_expr();
                self.expect(Token::CloseBracket);
                expr
            }
            _ => self.parse_literal(),
        }
    }

    fn parse_literal(&mut self) -> ast::Expression {
        match self.code.pop_front().unwrap() {
            Token::Number(num) => {
                ast::Expression::Literal(ast::Literal::Int(num))
            }
            Token::True => {
                ast::Expression::Literal(ast::Literal::Bool(true))
            }
            Token::False => {
                ast::Expression::Literal(ast::Literal::Bool(false))
            }
            Token::Ident(ident) => {
                ast::Expression::Identifier(ast::Ident(ident))
            }
            t => panic!("Expected literal, got {t:?}"),
        }
    }
}
