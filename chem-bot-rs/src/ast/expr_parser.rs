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
use crate::ast::value::Value;
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

pub fn parse_literal_number(
    source: &str
) -> Result<(&str, isize), nom::Err<nom::error::Error<&str>>> {
    let (source, mut subscript) = take_while1(|x: char| {
        match x {
            '0' => true,
            '1' => true,
            '2' => true,
            '3' => true,
            '4' => true,
            '5' => true,
            '6' => true,
            '7' => true,
            '8' => true,
            '9' => true,
            _ => false,
        }
    })(source)?;
    let value = subscript.parse::<isize>().unwrap();
    Ok((source, value))
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
    let (source, literal) = parse_literal_number(source)?;
    let ast = Expr::Num(BigRational::from_i64(literal as i64).unwrap());
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

