use std::str::FromStr;

use bigdecimal::{BigDecimal, Num};
use bigdecimal::num_bigint::BigInt;
use nom::{AsChar, IResult, Parser};
use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case};
use nom::character::complete::{char, hex_digit1, multispace1};
use nom::combinator::{eof, fail};
use nom::multi::{many0, separated_list0};
use nom::number::complete::recognize_float;
use nom::sequence::{preceded, terminated};

use crate::runtime::{Command, Kroth, KrothType, KrothValue};

mod runtime;

fn main() {
    let input = std::fs::read_to_string("main.kr").unwrap();
    match terminated(parse_input, eof)(&input) {
        Ok((_, program)) => {
            let mut kroth = Kroth::default();
            for command in dbg!(program) {
                if let Err(err) = command.operate(&mut kroth) {
                    println!("{err:?}");
                    return;
                }
            }
        }
        Err(err) => println!("{err:?}")
    };
}

fn parse_input(input: &str) -> IResult<&str, Vec<Command>> {
    separated_list0(multispace1, alt((
        string_literal.map(|s| Command::Push(KrothValue::String(s))),
        number_literal.map(|n| Command::Push(KrothValue::Number(n))),
        type_literal.map(|t| Command::Push(KrothValue::Type(t))),
        basic_command,
        block.map(|cmds| Command::Push(KrothValue::Block(cmds)))
    )))(input)
}



fn block(input: &str) -> IResult<&str, Vec<Command>> {
    terminated(preceded(char('{'), parse_input), char('}'))(input)
}

fn basic_command(input: &str) -> IResult<&str, Command> {
    macro_rules! literal {
        ($command:expr) => {{
            tag_no_case(stringify!($command)).map(|_| $command)
        }};
    }
    use Command::*;
    alt((
        literal!(Drop),
        literal!(Dup),
        literal!(Over),
        literal!(Swap),
        literal!(Print),
        literal!(Input),
        literal!(Cast),
        literal!(Call),
    ))(input)
}

fn type_literal(input: &str) -> IResult<&str, KrothType> {
    terminated(preceded(char('<'), alt((
        tag("Number").map(|_| KrothType::Num),
        tag("String").map(|_| KrothType::Str),
        tag("Type").map(|_| KrothType::Typ),
    ))), char('>'))(input)
}

fn string_char(input: &str) -> IResult<&str, char> {
    let mut chars = input.chars();
    // TODO: Escaping
    match chars.next() {
        None | Some('\\' | '\"' | '\n' | '\r') => fail(input),
        Some(c) => Ok((&input[c.len()..], c)),
    }
}

fn string_literal(input: &str) -> IResult<&str, Box<str>> {
    terminated(preceded(
        char('"'), many0(string_char).map(|v| Box::from(v.iter().collect::<String>()))), char('"'),
    )(input)
}

fn number_literal(input: &str) -> IResult<&str, BigDecimal> {
    fn hex_number(input: &str) -> IResult<&str, BigDecimal> {
        hex_digit1.map(|res| BigDecimal::from(BigInt::from_str_radix(res, 16).unwrap())).parse(input)
    }

    alt((
        preceded(char('0'), preceded(char('x'), hex_number)),
        recognize_float.map(|s| BigDecimal::from_str(s).unwrap())
    ))(input)
}