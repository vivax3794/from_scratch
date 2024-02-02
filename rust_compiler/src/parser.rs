use lazy_static::lazy_static;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::{
        alpha1, alphanumeric1, digit1, multispace0, multispace1, space0, space1,
    },
    combinator::{eof, map, opt, recognize},
    multi::many0,
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

type Result<'a, T> = IResult<&'a str, T>;

use crate::ast;

enum Operator {
    Binary(Vec<(&'static str, ast::BinaryOp)>),
    Prefix(Vec<(&'static str, ast::PrefixOp)>),
    Comparison(Vec<(&'static str, ast::Comparisson)>),
}

lazy_static! {
    static ref OPERATORS: Vec<Operator> = vec![
        Operator::Comparison(vec![
            ("==", ast::Comparisson::Eq),
            (">=", ast::Comparisson::Ge),
            ("<=", ast::Comparisson::Le),
            ("!=", ast::Comparisson::Ne),
            ("<", ast::Comparisson::Lt),
            (">", ast::Comparisson::Gt),
        ]),
        Operator::Binary(vec![("+", ast::BinaryOp::Add), ("-", ast::BinaryOp::Sub),]),
        Operator::Prefix(vec![("!", ast::PrefixOp::Not), ("-", ast::PrefixOp::Neg)]),
    ];
}

pub fn parse(content: &str) -> ast::File {
    parse_file(content).unwrap().1
}

fn parse_comment(input: &str) -> Result<()> {
    let (input, _) = tuple((tag("#"), take_until("\n")))(input)?;

    Ok((input, ()))
}

fn parse_line_space(input: &str) -> Result<()> {
    let (input, _) = many0(tuple((multispace1, opt(parse_comment))))(input)?;

    Ok((input, ()))
}

fn parse_file(input: &str) -> Result<ast::File> {
    let (input, declarations) = many0(delimited(
        parse_line_space,
        parse_declaration,
        parse_line_space,
    ))(input)?;
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
        many0(delimited(
            parse_line_space,
            parse_statement,
            parse_line_space,
        )),
        tag("}"),
    )(input)?;
    Ok((input, ast::Body(stms.into_boxed_slice())))
}

fn parse_statement(input: &str) -> Result<ast::Statement> {
    alt((parse_return, parse_assert))(input)
}

fn parse_return(input: &str) -> Result<ast::Statement> {
    let (input, _) = terminated(tag("return"), multispace1)(input)?;
    let (input, expression) =
        terminated(parse_expression, pair(parse_line_space, tag(";")))(input)?;
    Ok((input, ast::Statement::Return(expression)))
}

fn parse_assert(input: &str) -> Result<ast::Statement> {
    let (input, _) = terminated(tag("assert"), multispace1)(input)?;
    let (input, expression) =
        terminated(parse_expression, pair(parse_line_space, tag(";")))(input)?;
    Ok((input, ast::Statement::Assert(expression)))
}

fn parse_expression(input: &str) -> Result<ast::Expression> {
    let (input, value) = parse_operator(0)(input)?;
    Ok((input, value))
}

fn parse_operator(level: usize) -> impl Fn(&str) -> Result<ast::Expression> {
    move |input: &str| {
        let Some(operator) = OPERATORS.get(level) else {
            return parse_group(input);
        };

        match operator {
            Operator::Prefix(values) => alt((
                to_alt(
                    &values
                        .iter()
                        .map(|(value, op)| {
                            move |input| {
                                map(
                                    preceded(
                                        terminated(tag(*value), space0),
                                        parse_operator(level),
                                    ),
                                    |expr| ast::Expression::Prefix(*op, Box::new(expr)),
                                )(input)
                            }
                        })
                        .collect::<Vec<_>>(),
                ),
                parse_operator(level + 1),
            ))(input),
            Operator::Comparison(ops) => {
                let (input, left) = parse_operator(level + 1)(input)?;

                let parsers = ops
                    .iter()
                    .map(|(txt, res)| |input| map(tag(*txt), |_| *res)(input))
                    .collect::<Vec<_>>();
                let parser = to_alt(&parsers);

                let (input, chains) = many0(tuple((
                    preceded(space0, parser),
                    preceded(space0, parse_operator(level + 1)),
                )))(input)?;

                if chains.is_empty() {
                    Ok((input, left))
                } else {
                    Ok((
                        input,
                        ast::Expression::Comparison(
                            Box::new(left),
                            chains
                                .into_iter()
                                .map(|(op, expr)| (op, Box::new(expr)))
                                .collect(),
                        ),
                    ))
                }
            }
            Operator::Binary(ops) => {
                // left ascoativity!

                let parsers = ops
                    .iter()
                    .map(|(text, op)| move |input| map(tag(*text), |_| *op)(input))
                    .collect::<Vec<_>>();
                let mut op_parser = delimited(parse_line_space, to_alt(&parsers), parse_line_space);

                let (mut input, mut expr) = parse_operator(level + 1)(input)?;
                loop {
                    match op_parser(input) {
                        Err(_) => break,
                        Ok((n_input, op)) => {
                            let (n_input, right_side) = parse_operator(level + 1)(n_input)?;
                            input = n_input;
                            expr =
                                ast::Expression::Binary(Box::new(expr), op, Box::new(right_side));
                        }
                    }
                }

                Ok((input, expr))
            }
        }
    }
}

fn to_alt<'a, 'f, P, O>(parsers: &'f [P]) -> impl Fn(&'a str) -> Result<O> + 'f
where
    P: Fn(&'a str) -> Result<O>,
{
    move |input| match parsers {
        [a] => alt((a,))(input),
        [a, b] => alt((a, b))(input),
        [a, b, c] => alt((a, b, c))(input),
        [a, b, c, d] => alt((a, b, c, d))(input),
        [a, b, c, d, e] => alt((a, b, c, d, e))(input),
        [a, b, c, d, e, f] => alt((a, b, c, d, e, f))(input),
        [a, b, c, d, e, f, g] => alt((a, b, c, d, e, f, g))(input),
        [a, b, c, d, e, f, g, h] => alt((a, b, c, d, e, f, g, h))(input),
        [a, b, c, d, e, f, g, h, i] => alt((a, b, c, d, e, f, g, h, i))(input),
        [a, b, c, d, e, f, g, h, i, j] => alt((a, b, c, d, e, f, g, h, i, j))(input),
        [a, b, c, d, e, f, g, h, i, j, k] => alt((a, b, c, d, e, f, g, h, i, j, k))(input),
        [a, b, c, d, e, f, g, h, i, j, k, l] => alt((a, b, c, d, e, f, g, h, i, j, k, l))(input),
        [a, b, c, d, e, f, g, h, i, j, k, l, m] => {
            alt((a, b, c, d, e, f, g, h, i, j, k, l, m))(input)
        }
        [a, b, c, d, e, f, g, h, i, j, k, l, m, n] => {
            alt((a, b, c, d, e, f, g, h, i, j, k, l, m, n))(input)
        }
        [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o] => {
            alt((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o))(input)
        }
        [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p] => {
            alt((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p))(input)
        }
        [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q] => {
            alt((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q))(input)
        }
        [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r] => {
            alt((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r))(input)
        }
        [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s] => {
            alt((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s))(input)
        }
        [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t] => {
            alt((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t))(input)
        }
        [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u] => alt((
            a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u,
        ))(input),
        _ => panic!(),
    }
}

fn parse_group(input: &str) -> Result<ast::Expression> {
    alt((
        delimited(
            delimited(parse_line_space, tag("("), parse_line_space),
            parse_expression,
            delimited(parse_line_space, tag(")"), parse_line_space),
        ),
        parse_literal,
    ))(input)
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
