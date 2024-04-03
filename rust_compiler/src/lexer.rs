//! The lexer is responsible for converting a string of characters into a list of tokens.
use crate::span::{Span, Spanned};
use crate::{CompileError, Result};

/// A token
#[allow(clippy::missing_docs_in_private_items)]
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Token {
    Number(i128),
    Ident(Box<str>),
    OpenBracket,
    CloseBracket,
    OpenCurly,
    CloseCurly,
    Colon,
    Dot,
    DotDot,
    Plus,
    Minus,
    Star,
    StarStar,
    SemiColon,
    Slash,
    SlashSlash,
    Mod,
    Eq,
    EqEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    BangEq,
    Expose,
    Return,
    Let,
    Mut,
    If,
    Else,
    While,
    Assert,
    PlusEq,
    MinusEq,
    StarEq,
    DivEq,
    Bang,
    True,
    False,
    AssertType,
    Def,
    Comma,
    Arrow,
    And,
    Or,
    Eof,
}

/// The lexer
pub struct Lexer {
    /// The code to lex
    code: std::collections::VecDeque<char>,
    /// The current position
    pos: usize,
}

impl Lexer {
    /// Create a new lexer
    pub fn new(code: &str) -> Self {
        let code = code.chars().collect();
        Self { code, pos: 0 }
    }

    /// Peek at the next character
    fn peek(&self) -> Option<char> {
        self.code.front().copied()
    }

    /// Get the next character
    fn next(&mut self) -> Option<char> {
        self.pos += 1;
        self.code.pop_front()
    }

    /// Create a spanned token
    fn token(&self, token: Token, length: usize) -> Spanned<Token> {
        Spanned::new(token, self.pos - length, self.pos)
    }

    /// Lex the code
    pub fn lex(mut self) -> Result<Vec<Spanned<Token>>, CompileError> {
        let mut tokens = Vec::new();

        self.eat_whitespace();
        while let Some(c) = self.next() {
            let token = match c {
                '(' => self.token(Token::OpenBracket, 1),
                ')' => self.token(Token::CloseBracket, 1),
                '{' => self.token(Token::OpenCurly, 1),
                '}' => self.token(Token::CloseCurly, 1),
                '+' => self.lex_double(Token::Plus, &[('=', Token::PlusEq)]),
                '*' => {
                    self.lex_double(Token::Star, &[('=', Token::StarEq), ('*', Token::StarStar)])
                }
                '/' => self.lex_double(
                    Token::Slash,
                    &[('=', Token::DivEq), ('/', Token::SlashSlash)],
                ),
                '%' => self.token(Token::Mod, 1),
                ':' => self.token(Token::Colon, 1),
                ';' => self.token(Token::SemiColon, 1),
                '.' => self.lex_double(Token::Dot, &[('.', Token::DotDot)]),
                '-' => self.lex_double(Token::Minus, &[('=', Token::MinusEq), ('>', Token::Arrow)]),
                '=' => self.lex_double(Token::Eq, &[('=', Token::EqEq)]),
                '!' => self.lex_double(Token::Bang, &[('=', Token::BangEq)]),
                '<' => self.lex_double(Token::Lt, &[('=', Token::LtEq)]),
                '>' => self.lex_double(Token::Gt, &[('=', Token::GtEq)]),
                ',' => self.token(Token::Comma, 1),
                '&' => self.token(Token::And, 1),
                '|' => self.token(Token::Or, 1),
                c if c.is_ascii_digit() => self.lex_number(c)?,
                c if c.is_alphabetic() => self.lex_ident(c),
                _ => {
                    return Err(CompileError::UnknownCharacter {
                        character: c,
                        span: miette::SourceSpan::new((self.pos - 1).into(), 1),
                    })
                }
            };
            tokens.push(token);
            self.eat_whitespace();
        }
        tokens.push(self.token(Token::Eof, 0));

        Ok(tokens)
    }

    /// Eat whitespace from the code
    fn eat_whitespace(&mut self) {
        loop {
            match self.peek() {
                Some('#') => self.eat_comment(),
                Some(c) if c.is_whitespace() => {
                    self.next();
                }
                _ => break,
            }
        }
    }

    /// Eat a comment from the code
    fn eat_comment(&mut self) {
        loop {
            match self.peek() {
                None | Some('\n') => break,
                _ => {
                    self.next();
                }
            }
        }
    }

    /// Lex a double character token
    /// gets given a list of characters and their corresponding tokens
    /// and returns the token that matches the first character
    /// if no character matches, it returns the single token
    fn lex_double(&mut self, single: Token, doubles: &[(char, Token)]) -> Spanned<Token> {
        match self.peek() {
            Some(c) => {
                if let Some((_, token)) = doubles.iter().find(|(dc, _)| *dc == c) {
                    self.next();
                    self.token(token.clone(), 2)
                } else {
                    self.token(single, 1)
                }
            }
            _ => self.token(single, 1),
        }
    }

    /// Lex a number
    fn lex_number(&mut self, first: char) -> Result<Spanned<Token>> {
        let mut digits = vec![first];
        loop {
            match self.peek() {
                Some(c) if c.is_ascii_digit() => {
                    self.next();
                    digits.push(c);
                }
                Some(_) | None => break,
            }
        }

        let len = digits.len();
        // TODO: can overflow
        let num = digits
            .into_iter()
            .collect::<String>()
            .parse()
            .map_err(|_| CompileError::IntOverflow {
                span: Span::new(self.pos - len, self.pos).into(),
            })?;
        Ok(self.token(Token::Number(num), len))
    }

    /// Lex an identifier
    /// As well as keywords
    fn lex_ident(&mut self, first: char) -> Spanned<Token> {
        let mut chars = vec![first];
        loop {
            match self.peek() {
                Some(c) if c.is_alphanumeric() || c == '_' => {
                    self.next();
                    chars.push(c);
                }
                Some(_) | None => break,
            }
        }

        let s: String = chars.into_iter().collect();
        match &*s {
            "expose" => self.token(Token::Expose, 6),
            "return" => self.token(Token::Return, 6),
            "assert" => self.token(Token::Assert, 6),
            "if" => self.token(Token::If, 2),
            "while" => self.token(Token::While, 5),
            "let" => self.token(Token::Let, 3),
            "mut" => self.token(Token::Mut, 3),
            "else" => self.token(Token::Else, 4),
            "true" => self.token(Token::True, 4),
            "false" => self.token(Token::False, 5),
            "assert_type" => self.token(Token::AssertType, 11),
            "def" => self.token(Token::Def, 3),
            _ => {
                let len = s.len();
                self.token(Token::Ident(s.into_boxed_str()), len)
            }
        }
    }
}
