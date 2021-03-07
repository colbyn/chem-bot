use std::collections::HashMap;
use std::iter::FromIterator;
use nom::{
    IResult,
    bytes::complete::{tag, take_while_m_n, take_while, take_while1},
    combinator::map_res,
    sequence::tuple,
    sequence::delimited,
    character::complete::char,
    bytes::complete::is_not,
    error::ParseError,
    character::complete::multispace0,
    combinator::recognize,
    sequence::pair,
    branch::alt,
    character::complete::{alpha1},
    character::complete::alphanumeric1,
    combinator::{cut, map, opt},
    error::{context, VerboseError},
    multi::{many0, many1},
    sequence::{preceded, terminated},
    character::complete::{digit1, multispace1, one_of},
    multi::separated_list1,
    multi::separated_list0,
    Parser,
};
use num::{FromPrimitive, ToPrimitive, BigRational};

use crate::ast::expr::*;
use crate::parser_utils::{self, identifier, parens, ws};


///////////////////////////////////////////////////////////////////////////////
// HELPERS
///////////////////////////////////////////////////////////////////////////////

#[inline]
pub fn is_lowercase(chr: u8) -> bool {
    (chr as char).is_lowercase()
}

#[inline]
pub fn is_uppercase(chr: char) -> bool {
    (chr as char).is_lowercase()
}

fn parse_integer(source: &str) -> IResult<&str, BigRational> {
    let parser = recognize
        (many1(terminated(one_of("0123456789"), many0(char('_')))));
    let mut parser = map_res(parser, |x: &str| {
        use std::str::FromStr;
        BigRational::from_str(x)
    });
    parser(source)
}

fn parse_float(source: &str) -> IResult<&str, BigRational> {
    fn decimal(source: &str) -> IResult<&str, &str> {
        recognize(many1(terminated(one_of("0123456789"), many0(char('_')))))(source)
    }
    let parser = alt((
        // Case one: .42
        recognize(
            tuple((
                char('.'),
                decimal,
                opt(tuple((
                    one_of("eE"),
                    opt(one_of("+-")),
                    decimal
                )))
            ))
        )
        , // Case two: 42e42 and 42.42e42
        recognize(
            tuple((
                decimal,
                opt(preceded(char('.'), decimal)),
                one_of("eE"),
                opt(one_of("+-")),
                decimal
            ))
        )
        , // Case three: 42. and 42.42
        recognize(tuple(( decimal, char('.'), opt(decimal))))
    ));
    let mut parser = map_res(parser, |x: &str| {
        use std::str::FromStr;
        BigRational::from_str(x)
    });
    parser(source) 
}

///////////////////////////////////////////////////////////////////////////////
// AST
///////////////////////////////////////////////////////////////////////////////

fn parse_ast(source: &str) -> Result<(&str, Expr), nom::Err<nom::error::Error<&str>>> {
    fn inner(source: &str) -> Result<(&str, Expr), nom::Err<nom::error::Error<&str>>> {
        alt((parse_literal, parse_function_call))(source)
    }
    ws(inner)(source)
}

fn parse_literal(source: &str) -> Result<(&str, Expr), nom::Err<nom::error::Error<&str>>> {
    let (source, literal) = alt((parse_integer, parse_float))(source)?;
    let ast = Expr::Num(literal);
    Ok((source, ast))
}

fn parse_function_call(source: &str) -> Result<(&str, Expr), nom::Err<nom::error::Error<&str>>> {
    fn positional_argument(
        source: &str
    ) -> Result<(&str, Expr), nom::Err<nom::error::Error<&str>>> {
        parse_ast(source)
    }
    fn keyword_argument(
        source: &str
    ) -> Result<(&str, (String, Expr)), nom::Err<nom::error::Error<&str>>> {
        let (source, ident) = identifier(source)?;
        let (source, _) = ws(tag("="))(source)?;
        let (source, ast) = parse_ast(source)?;
        Ok((source, (ident, ast)))
    }
    fn arguments(
        source: &str
    ) -> Result<(&str, (Vec<Expr>, HashMap<String, Expr>)), nom::Err<nom::error::Error<&str>>> {
        let (source, pos_args) = separated_list0(
            parser_utils::comma,
            positional_argument,
        )(source)?;
        let (source, key_args) = separated_list0(
            parser_utils::comma,
            keyword_argument,
        )(source)?;
        let key_args = HashMap::<String, Expr>::from_iter(key_args);
        Ok((source, (pos_args, key_args)))
    }
    let (source, name) = identifier(source)?;
    let (source, (pos_args, key_args)) = parens(arguments)(source)?;
    let ast = Expr::Call(Box::new(FunCall{
        name,
        pos_args,
        key_args,
    }));
    Ok((source, ast))
}

///////////////////////////////////////////////////////////////////////////////
// ROOT PARSER
///////////////////////////////////////////////////////////////////////////////

pub(crate) fn run_parser(source: &str) -> Result<Expr, ()> {
    let (rest, result) = parse_ast(&source).unwrap();
    assert!(rest.is_empty());
    Ok(result)
}

///////////////////////////////////////////////////////////////////////////////
// DEV
///////////////////////////////////////////////////////////////////////////////

pub(crate) fn main() {
    let source = std::fs::read_to_string("sample.txt").unwrap();
    let result = run_parser(&source).unwrap();
    println!("{:#?}", result);
}

