use std::env::args;
use std::process::abort;
use std::str::FromStr;

use bigdecimal::{BigDecimal, Num};
use bigdecimal::num_bigint::BigInt;
use nom::{AsChar, IResult, Parser};
use nom::branch::alt;
use nom::bytes::complete::{tag, tag_no_case};
use nom::character::complete::{char as single_char, hex_digit1, multispace0, multispace1};
use nom::combinator::{eof, fail};
use nom::multi::{many0, many1, separated_list0};
use nom::number::complete::recognize_float;
use nom::sequence::{preceded, terminated};

use crate::runtime::{Command, Kroth, KrothType, KrothValue};

mod runtime;

fn main() -> anyhow::Result<()> {
    let mut args = args();
    let program_path = args.next().unwrap();
    let Some(file) = args.next()
        else {
            eprintln!("ERROR: No input file.\nUsage: {program_path} [input-file]");
            abort()
        };
    let input = remove_comments(&std::fs::read_to_string(file)?);
    let program = match terminated(parse_input, eof)(&input) {
        Ok((_, program)) => program,
        Err(err) => {
            eprintln!("{err}");
            abort()
        }
    };
    let mut kroth = Kroth::default();
    kroth.run_program(program)?;
    Ok(())
}

fn remove_comments(input: &str) -> String {
    input.lines().map(|line| {
        match line.find("//") {
            None => line,
            Some(index) => &line[..index]
        }.to_string() + "\n"
    }).collect()
}

fn parse_input(input: &str) -> IResult<&str, Vec<Command>> {
    separated_list0(multispace1, alt((
        string_literal.map(|s| Command::Push(KrothValue::String(s))),
        number_literal.map(|n| Command::Push(KrothValue::Number(n))),
        type_literal.map(|t| Command::Push(KrothValue::Type(t))),
        bool_literal.map(|b| Command::Push(KrothValue::Bool(b))),
        basic_command,
        block.map(|cmds| Command::Push(KrothValue::Block(cmds))),
        var,
        empty_check
    )))(input.trim())
}

fn block(input: &str) -> IResult<&str, Vec<Command>> {
    terminated(preceded(terminated(single_char('{'), multispace0), parse_input), preceded(multispace0, single_char('}')))(input.trim())
}

fn basic_command(input: &str) -> IResult<&str, Command> {
    macro_rules! literal {
        ($($command:expr),+) => {($(tag_no_case(stringify!($command)).map(|_| $command),)+)};
    }
    use Command::*;
    alt((
        alt(literal!(Drop, Dup, Over, Swap, Rev, Rot, Count)),
        alt(literal!(Print, Input, Cast, Call)),
        alt(literal!(IfElse, If, While)),
        alt(literal!(Add, Sub, Mul, Div, Mod)),
        alt(literal!(Xor, Or, And, Not)),
        alt(literal!(LtEq, GtEq, Gt, Lt, Neq, Eq))
    ))(input)
}

fn type_literal(input: &str) -> IResult<&str, KrothType> {
    terminated(preceded(single_char('<'), alt((
        tag("Number").map(|_| KrothType::Num),
        tag("String").map(|_| KrothType::Str),
        tag("Type").map(|_| KrothType::Typ),
        tag("Bool").map(|_| KrothType::Bool),
    ))), single_char('>'))(input)
}

fn empty_check(input: &str) -> IResult<&str, Command> {
    tag("empty?").map(|_| Command::Empty).parse(input)
}

fn string_char(input: &str) -> IResult<&str, char> {
    let mut chars = input.chars();
    match chars.next() {
        None | Some('"') => fail(input),
        Some('\\') => match chars.next() {
            Some('"') => Ok((&input[2..], '"')),
            Some('\\') => Ok((&input[2..], '\\')),
            Some('n') => Ok((&input[2..], '\n')),
            Some('r') => Ok((&input[2..], '\r')),
            Some('\r') => if let Some('\n') = chars.next() {
                Ok((&input[3..], '\n'))
            } else {
                fail(input)
            }
            Some('\n') => Ok((&input[2..], '\n')),
            _ => fail(input)
        }
        Some(c) => Ok((&input[c.len()..], c)),
    }
}

fn var(input: &str) -> IResult<&str, Command> {
    alt((
        preceded(single_char('&'), var_name).map(Command::SetVar),
        preceded(single_char('*'), var_name).map(Command::GetVar)
    ))(input)
}

fn var_name(input: &str) -> IResult<&str, Box<str>> {
    many1(var_name_char).map(|c| Box::from(c.into_iter().collect::<String>())).parse(input)
}

fn var_name_char(input: &str) -> IResult<&str, char> {
    let mut chars = input.chars();
    match chars.next() {
        Some('_') => Ok((&input[1..], '_')),
        Some(c) if c.is_alphabetic() => Ok((&input[c.len()..], c)),
        _ => fail(input),
    }
}

fn string_literal(input: &str) -> IResult<&str, Box<str>> {
    terminated(preceded(
        single_char('"'), many0(string_char).map(|v| Box::from(v.iter().collect::<String>()))), single_char('"'),
    )(input)
}

fn bool_literal(input: &str) -> IResult<&str, bool> {
    alt((
        tag("true").map(|_| true),
        tag("false").map(|_| false),
    ))(input)
}

fn number_literal(input: &str) -> IResult<&str, BigDecimal> {
    fn hex_number(input: &str) -> IResult<&str, BigDecimal> {
        hex_digit1.map(|res| BigDecimal::from(BigInt::from_str_radix(res, 16).unwrap())).parse(input)
    }

    alt((
        preceded(single_char('0'), preceded(single_char('x'), hex_number)),
        recognize_float.map(|s| BigDecimal::from_str(s).unwrap())
    ))(input)
}