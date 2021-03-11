use nom::{
    IResult,
    bytes::complete::{tag, take_while_m_n, take_while},
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
    Parser,
};
use crate::chem::data::*;
use crate::parser_utils::{identifier, parens, ws};

pub fn parse_state(source: &str) -> Result<(&str, State), nom::Err<nom::error::Error<&str>>> {
    let mut parser = alt((
        tag("(aq)"),
        tag("(l)"),
        tag("(g)"),
        tag("(s)"),
    ));
    parser(source).map(|(rest, state)| {
        let state = match state {
            "(aq)" => State::Aq,
            "(l)" => State::L,
            "(g)" => State::G,
            "(s)" => State::S,
            _ => panic!(),
        };
        (rest, state)
    })
}

#[inline]
pub fn is_lowercase(chr: u8) -> bool {
    (chr as char).is_lowercase()
}

#[inline]
pub fn is_uppercase(chr: char) -> bool {
    (chr as char).is_lowercase()
}

pub fn parse_element(source: &str) -> Result<(&str, String), nom::Err<nom::error::Error<&str>>> {
    let (source, head) = nom::character::complete::satisfy(|c: char| c.is_uppercase())(source)?;
    let (source, rest) = take_while(|x: char| x.is_lowercase())(source)?;
    Ok((source, format!("{}{}", head, rest)))
}
pub fn parse_unit(source: &str) -> Result<(&str, Node), nom::Err<nom::error::Error<&str>>> {
    let (source, head) = nom::character::complete::satisfy(|c: char| c.is_uppercase())(source)?;
    let (source, rest) = take_while(|x: char| x.is_lowercase())(source)?;
    let element = Element(format!("{}{}", head, rest));
    let (source, subscript) = parse_num(source)?;
    let value = Node::Unit(
        element,
        subscript,
    );
    Ok((source, value))
}
pub fn parse_num(source: &str) -> Result<(&str, usize), nom::Err<nom::error::Error<&str>>> {
    let (source, mut subscript) = take_while(|x: char| {
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
    if subscript.is_empty() {
        subscript = "1";
    }
    Ok((source, subscript.parse::<usize>().unwrap()))
}
pub fn parse_parens(source: &str) -> Result<(&str, Node), nom::Err<nom::error::Error<&str>>> {
    fn inner_parser(source: &str) -> Result<(&str, Vec<Node>), nom::Err<nom::error::Error<&str>>> {
        many1(parse_unit)(source)
    }
    let (source, xs) = parens(inner_parser)(source)?;
    let (source, subscript) = parse_num(source)?;
    let value = Node::Parens(xs, subscript);
    Ok((source, value))
}
pub fn parse_group(source: &str) -> Result<(&str, Node), nom::Err<nom::error::Error<&str>>> {
    let (source, coefficient) = opt(parse_num)(source)?;
    let (source, values) = many1(alt((parse_parens, parse_unit)))(source)?;
    let (source, state) = opt(parse_state)(source)?;
    let value = Node::Chunk(coefficient.unwrap_or(1), values, state);
    Ok((source, value))
}

pub fn parse_sum(source: &str) -> Result<(&str, Vec<Node>), nom::Err<nom::error::Error<&str>>> {
    fn inner_parser(source: &str) -> Result<(&str, Vec<Node>), nom::Err<nom::error::Error<&str>>> {
        separated_list1(ws(tag("+")), parse_group)(source)
    }
    inner_parser(source)
}

pub fn parse_reaction(source: &str) -> Result<(&str, Reaction), nom::Err<nom::error::Error<&str>>> {
    let (source, left) = parse_sum(source)?;
    let (source, _) = ws(tag("->"))(source)?;
    let (source, right) = parse_sum(source)?;
    let reaction = Reaction {
        reactants: Sequence(left),
        products: Sequence(right),
    };
    Ok((source, reaction))
}


pub fn run() {
    let source = "Ca(O3H2)2(aq) + 2HCl(aq) -> CaCl2(aq) + H2O(l)";
    let (source, result) = parse_reaction(source).unwrap();
    println!("{:#?}", result);
    println!("{}", result.to_string());
}


