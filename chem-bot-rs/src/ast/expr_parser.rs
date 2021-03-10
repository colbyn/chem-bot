use std::collections::HashMap;
use std::iter::FromIterator;
use std::str::FromStr;
use nom::number::complete::float;
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
    combinator::{cut, map, opt, verify, not},
    error::{context, VerboseError},
    multi::{many0, many1},
    sequence::{preceded, terminated},
    character::complete::{digit1, multispace1, one_of},
    multi::separated_list1,
    multi::separated_list0,
    Parser,
    map,
};
use num::{FromPrimitive, ToPrimitive, BigRational};

use crate::ast::expr::*;
use crate::parser_utils::{self, identifier, parens, ws, Error};


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

fn parse_number(source: &str) -> Result<(&str, BigRational), Error<&str>> {
    let (source, val) = nom::number::complete::double(source)?;
    let val = BigRational::from_f64(val).unwrap();
    Ok((source, val))
}

///////////////////////////////////////////////////////////////////////////////
// AST
///////////////////////////////////////////////////////////////////////////////

fn parse_ast(source: &str, full_consume: bool) -> Result<(&str, Expr), Error<&str>> {
    use nom::{eof, named};
    fn inner(source: &str) -> Result<(&str, Expr), Error<&str>> {
        let parsers = (
            parse_function_call,
            parse_constant,
            parse_literal,
            parse_product,
        );
        alt(parsers)(source)
    }
    // fn inner(source: &str) -> Result<(&str, Expr), Error<&str>> {
    //     let parsers = &[
    //         parse_function_call,
    //         parse_literal,
    //         parse_constant,
    //         parse_product,
    //     ];
    //     parser_utils::choice(parsers)(source)
    // }
    let (rest, value) = ws(inner)(source)?;
    if !rest.is_empty() && full_consume {
        let e: Error<&str> = nom::Err::Error(nom::error::Error::new(
            source,
            nom::error::ErrorKind::Verify
        ));
        return Err(e)
    }
    Ok((rest, value))
}

fn parse_literal(source: &str) -> Result<(&str, Expr), Error<&str>> {
    let (source, literal) = parse_number(source)?;
    let ast = Expr::Num(literal);
    Ok((source, ast))
}

fn parse_constant(source: &str) -> Result<(&str, Expr), Error<&str>> {
    fn parse_reciprocal(source: &str) -> Result<(&str, bool), Error<&str>> {
        let p = opt(tag("^-1"));
        map(p, |x| x.is_some())(source)
    }
    let (source, constant) = nom::character::complete::alpha1(source)?;
    let (source, has_reciprocal) = parse_reciprocal(source)?;
    let ast = {
        let expr = Expr::con(constant);
        if has_reciprocal {
            Expr::unit_fraction(expr)
        } else {
            expr
        }
    };
    Ok((source, ast))
}

fn parse_product(source: &str) -> Result<(&str, Expr), Error<&str>> {
    fn inner(source: &str) -> Result<(&str, Expr), Error<&str>> {
        let parsers = (
            parse_function_call,
            parse_constant,
            parse_literal,
        );
        alt(parsers)(source)
    }
    let (source, xs) = separated_list1(ws(char('*')), ws(inner))(source)?;
    let ast = Expr::Product(xs);
    Ok((source, ast))
}

fn parse_function_call(source: &str) -> Result<(&str, Expr), Error<&str>> {
    fn positional_argument(
        source: &str
    ) -> Result<(&str, Expr), Error<&str>> {
        let (source, value) = parse_ast(source, false)?;
        let (source, _) = not(ws(char('=')))(source)?;
        Ok((source, value))
    }
    fn keyword_argument(
        source: &str
    ) -> Result<(&str, (String, Expr)), Error<&str>> {
        let (source, ident) = identifier(source)?;
        let (source, _) = ws(char('='))(source)?;
        let (source, ast) = parse_ast(source, false)?;
        Ok((source, (ident, ast)))
    }
    fn arguments(
        source: &str
    ) -> Result<(&str, (Vec<Expr>, HashMap<String, Expr>)), Error<&str>> {
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
    let (rest, result) = parse_ast(&source, false).unwrap();
    if !rest.is_empty() {
        eprintln!("[error] unparsed source code!");
        eprintln!("\t☞ given: {}", source);
        eprintln!("\t✔︎ parsed: {:?}", result);
        eprintln!("\t✘ unparsed: {:?}", rest);
        eprintln!("");
    }
    assert!(rest.is_empty());
    Ok(result)
}

///////////////////////////////////////////////////////////////////////////////
// DEV
///////////////////////////////////////////////////////////////////////////////

pub(crate) fn main() {
    // let source = std::fs::read_to_string("sample.txt").unwrap();
    // let result = run_parser(&source).unwrap();
    // println!("{:#?}", result);
    let result = run_parser("mole(energy(photon(wavelength = nm(325))))");
    println!("\nDONE");
    println!("{:#?}", result.map(|x| x.to_string()));
}

