use nom::IResult;

type Result<'a, T> = IResult<&'a str, T>;

use crate::ast;

pub fn parse(content: &str) -> ast::File {
    dbg!(content);
    todo!()
}
