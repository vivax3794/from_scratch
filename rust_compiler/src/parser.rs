use lazy_static::lazy_static;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{
        alpha1, alphanumeric1, digit1, multispace0, multispace1, space0, space1,
    },
    combinator::{eof, map, recognize},
    multi::many0,
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

type Result<'a, T> = IResult<&'a str, T>;

use crate::ast;

enum Operator {
    Prefix(&'static str, ast::PrefixOp),
    Comparison(Vec<(&'static str, ast::Comparisson)>),
}

lazy_static! {
    static ref OPERATORS: Vec<Operator> = vec![
        Operator::Comparison(vec![
            ("==", ast::Comparisson::Eq),
            ("<", ast::Comparisson::Lt),
            (">", ast::Comparisson::Gt),
            (">=", ast::Comparisson::Ge),
            ("<=", ast::Comparisson::Le),
            ("!=", ast::Comparisson::Ne),
        ]),
        Operator::Prefix("!", ast::PrefixOp::Not),
    ];
}

pub fn parse(content: &str) -> ast::File {
    parse_file(content).unwrap().1
}

fn parse_file(input: &str) -> Result<ast::File> {
    let (input, declarations) =
        many0(delimited(multispace0, parse_declaration, multispace0))(input)?;
    let file = ast::File(declarations.into_boxed_slice());

    let (input, _) = eof(input)?;
    Ok((input, file))
}

fn parse_declaration(input: &str) -> Result<ast::Declaration> {
    let (input, function) = parse_function(input)?;
    let declaration = ast::Declaration::Function(function);

    Ok((input, declaration))
}

fn parse_function(input: &str) -> Result<ast::FunctionDeclration> {
    parse_function_exposed(input)
}

fn parse_function_exposed(input: &str) -> Result<ast::FunctionDeclration> {
    let (input, _) = tag("expose")(input)?;
    let (input, name) = preceded(multispace1, parse_ident)(input)?;
    let (input, _) = preceded(multispace0, tag("()"))(input)?;
    let (input, return_type) = preceded(multispace0, parse_type)(input)?;
    let (input, body) = preceded(multispace0, parse_body)(input)?;

    Ok((
        input,
        ast::FunctionDeclration::ExposedFunction {
            name,
            return_type,
            body,
        },
    ))
}

fn parse_type(input: &str) -> Result<ast::Type> {
    let (input, name) = parse_ident(input)?;
    Ok((input, ast::Type::Named(name)))
}

fn parse_body(input: &str) -> Result<ast::Body> {
    let (input, stms) = delimited(
        tag("{"),
        many0(delimited(multispace0, parse_statement, multispace0)),
        tag("}"),
    )(input)?;
    Ok((input, ast::Body(stms.into_boxed_slice())))
}

fn parse_statement(input: &str) -> Result<ast::Statement> {
    alt((parse_return, parse_assert))(input)
}

fn parse_return(input: &str) -> Result<ast::Statement> {
    let (input, _) = terminated(tag("return"), multispace1)(input)?;
    let (input, expression) = terminated(parse_expression, pair(multispace0, tag(";")))(input)?;
    Ok((input, ast::Statement::Return(expression)))
}

fn parse_assert(input: &str) -> Result<ast::Statement> {
    let (input, _) = terminated(tag("assert"), multispace1)(input)?;
    let (input, expression) = terminated(parse_expression, pair(multispace0, tag(";")))(input)?;
    Ok((input, ast::Statement::Assert(expression)))
}

fn parse_expression(input: &str) -> Result<ast::Expression> {
    let (input, value) = parse_operator(0)(input)?;
    Ok((input, value))
}

fn parse_operator(level: usize) -> impl Fn(&str) -> Result<ast::Expression> {
    move |input: &str| {
        let Some(operator) = OPERATORS.get(level) else {
            return parse_literal(input);
        };

        match operator {
            Operator::Prefix(value, op) => alt((
                map(preceded(tag(*value), parse_operator(level)), |expr| {
                    ast::Expression::Prefix(*op, Box::new(expr))
                }),
                parse_operator(level + 1),
            ))(input),
            Operator::Comparison(ops) => {
                let (input, left) = parse_operator(level + 1)(input)?;
                let op_parser = ops
                    .iter()
                    .map(|(op, val)| map(tag(*op), |_| *val))
                    .reduce(|a, b| alt((a, b)))
                    .unwrap();
                todo!()
            }
        }
    }
}

fn parse_literal(input: &str) -> Result<ast::Expression> {
    let (input, value) = alt((
        map(digit1, |val: &str| ast::Literal::Int(val.parse().unwrap())),
        map(tag("false"), |_| ast::Literal::Bool(false)),
        map(tag("true"), |_| ast::Literal::Bool(true)),
    ))(input)?;
    Ok((input, ast::Expression::Literal(value)))
}

fn parse_ident(input: &str) -> Result<ast::Ident> {
    let (input, ident) = recognize(pair(alpha1, many0(alt((tag("_"), alphanumeric1)))))(input)?;
    Ok((input, ast::Ident(ident.into())))
}
