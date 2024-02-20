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
    ModEq,
    Bang,
    True,
    False,
    Eof,
}

pub struct Lexer {
    code: std::collections::VecDeque<char>,
}

impl Lexer {
    pub fn new(code: &str) -> Self {
        let code = code.chars().collect();
        Self { code }
    }

    pub fn lex(mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        self.eat_whitespace();
        while !self.code.is_empty() {
            let token = match self.code.pop_front().unwrap() {
                '(' => Token::OpenBracket,
                ')' => Token::CloseBracket,
                '{' => Token::OpenCurly,
                '}' => Token::CloseCurly,
                '+' => self
                    .lex_double(Token::Plus, &[('=', Token::PlusEq)]),
                '*' => self
                    .lex_double(Token::Star, &[('=', Token::StarEq)]),
                '/' => self.lex_double(
                    Token::Slash,
                    &[('=', Token::DivEq), ('/', Token::SlashSlash)],
                ),
                '%' => Token::Mod,
                ':' => Token::Colon,
                ';' => Token::SemiColon,
                '.' => self
                    .lex_double(Token::Dot, &[('.', Token::DotDot)]),
                '-' => self.lex_double(
                    Token::Minus,
                    &[('=', Token::MinusEq)],
                ),
                '=' => {
                    self.lex_double(Token::Eq, &[('=', Token::EqEq)])
                }
                '!' => self
                    .lex_double(Token::Bang, &[('=', Token::BangEq)]),
                '<' => {
                    self.lex_double(Token::Lt, &[('=', Token::LtEq)])
                }
                '>' => {
                    self.lex_double(Token::Gt, &[('=', Token::GtEq)])
                }
                c if c.is_ascii_digit() => self.lex_number(c),
                c if c.is_alphabetic() => self.lex_ident(c),
                _ => panic!("Invalid char"),
            };
            tokens.push(token);
            self.eat_whitespace();
        }
        tokens.push(Token::Eof);

        tokens
    }

    fn eat_whitespace(&mut self) {
        loop {
            match self.code.get(0) {
                Some('#') => self.eat_comment(),
                Some(c) if c.is_whitespace() => {
                    self.code.pop_front();
                }
                _ => break,
            }
        }
    }

    fn eat_comment(&mut self) {
        loop {
            match self.code.get(0) {
                None => break,
                Some('\n') => break,
                _ => {
                    self.code.pop_front();
                }
            }
        }
    }

    fn lex_double(
        &mut self,
        single: Token,
        doubles: &[(char, Token)],
    ) -> Token {
        match self.code.get(0) {
            Some(c) => {
                if let Some((_, token)) =
                    doubles.iter().find(|(dc, _)| dc == c)
                {
                    self.code.pop_front();
                    token.clone()
                } else {
                    single
                }
            }
            _ => single,
        }
    }

    fn lex_number(&mut self, first: char) -> Token {
        let mut digits = vec![first];
        loop {
            match self.code.get(0) {
                None => break,
                Some(c) if c.is_ascii_digit() => {
                    let c = self.code.pop_front().unwrap();
                    digits.push(c);
                }
                Some(_) => break,
            }
        }

        let num =
            digits.into_iter().collect::<String>().parse().unwrap();
        Token::Number(num)
    }

    fn lex_ident(&mut self, first: char) -> Token {
        let mut chars = vec![first];
        loop {
            match self.code.get(0) {
                None => break,
                Some(c) if c.is_alphanumeric() => {
                    let c = self.code.pop_front().unwrap();
                    chars.push(c)
                }
                Some(_) => break,
            }
        }

        let s: String = chars.into_iter().collect();
        match &*s {
            "expose" => Token::Expose,
            "return" => Token::Return,
            "assert" => Token::Assert,
            "if" => Token::If,
            "while" => Token::While,
            "let" => Token::Let,
            "mut" => Token::Mut,
            "else" => Token::Else,
            "true" => Token::True,
            "false" => Token::False,
            _ => Token::Ident(s.into_boxed_str()),
        }
    }
}
