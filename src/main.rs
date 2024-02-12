use bigdecimal::{BigDecimal, FromPrimitive, Num};
use nom::{AsChar, IResult, Parser};
use nom::branch::alt;
use nom::character::complete::{char, hex_digit1};
use nom::combinator::fail;
use nom::multi::many0;
use nom::number::complete::float;
use nom::sequence::{preceded, terminated};

mod runtime;

fn main() {
    println!("{val:?}", val = parse_string("\"Hello  dsfjhvjbasldj b%%#&%r7235972\""));
}

fn string_char(input: &str) -> IResult<&str, char> {
    let mut chars = input.chars();
    // TODO: Escaping
    match chars.next() {
        None | Some('\\' | '\"' | '\n' | '\r') => fail(input),
        Some(c) => Ok((&input[c.len()..], c)),
    }
}


fn parse_string(input: &str) -> IResult<&str, Box<str>> {
    terminated(preceded(
        char('"'), many0(string_char).map(|v| Box::from(v.iter().collect::<String>()))), char('"'),
    )(input)
}

fn parse_number(input: &str) -> IResult<&str, BigDecimal> {
    fn hex_number(input: &str) -> IResult<&str, BigDecimal> {
        hex_digit1.map(|res| BigDecimal::from_str_radix(res, 16).unwrap()).parse(input)
    }

    alt((
        preceded(char('0'), preceded(char('x'), hex_number)),
        float.map(|f| BigDecimal::from_f32(f).unwrap())
    ))(input)
}