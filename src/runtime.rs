use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};

use bigdecimal::BigDecimal;

use crate::runtime::RuntimeError::InvalidType;

macro_rules! ok {
    ($s:stmt) => {{
        $s
        Ok(())
    }};
}

macro_rules! err {
    ($err:expr) => {{return Err($err);}};
}

macro_rules! ensure_size {
    ($kroth:expr, $size:expr) => {
        let len = $kroth.stack.len();
        if len < $size {
            return Err(NotEnoughElements {
                expected: $size,
                found: len,
            });
        }
    };
}

#[derive(Default, Debug)]
pub struct Kroth {
    stack: Vec<KrothValue>,
    vars: HashMap<Box<str>, KrothValue>,
}

impl Kroth {
    pub fn get(&self, name: &str) -> Option<&KrothValue> {
        self.vars.get(name)
    }

    pub fn peek(&self) -> Option<&KrothValue> {
        self.stack.last()
    }

    pub fn push(&mut self, val: KrothValue) {
        self.stack.push(val)
    }

    pub fn pop(&mut self) -> Option<KrothValue> {
        self.stack.pop()
    }

    pub fn set(&mut self, name: &str, val: KrothValue) {
        self.vars.insert(Box::from(name), val);
    }
}

#[derive(Copy, Clone, Debug)]
pub enum KrothType {
    Num,
    Str,
    Typ,
    Blck,
}

impl KrothType {
    fn of(val: &KrothValue) -> Self {
        use KrothType::*;
        match val {
            KrothValue::Number(_) => Num,
            KrothValue::String(_) => Str,
            KrothValue::Type(_) => Typ,
            KrothValue::Block(_) => Blck
        }
    }
}

impl Display for KrothType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            KrothType::Num => f.write_str("Number"),
            KrothType::Str => f.write_str("String"),
            KrothType::Typ => f.write_str("Type"),
            KrothType::Blck => f.write_str("Block"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum KrothValue {
    Number(BigDecimal),
    String(Box<str>),
    Type(KrothType),
    Block(Vec<Command>),
}

impl Display for KrothValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            KrothValue::Number(num) => f.write_fmt(format_args!("{num}")),
            KrothValue::String(str) => f.write_str(str),
            KrothValue::Type(typ) => f.write_fmt(format_args!("<{typ}>")),
            KrothValue::Block(_) => f.write_fmt(format_args!("{{block}}")),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Command {
    Drop,
    Dup,
    Over,
    Swap,
    Print,
    Input,
    Cast,
    Call,
    Push(KrothValue),
}

impl Command {
    pub fn operate(self, kroth: &mut Kroth) -> Result<(), RuntimeError> {
        use RuntimeError::{EmptyStackError, NotEnoughElements};

        match self {
            Command::Drop => {
                match kroth.pop() {
                    None => Err(EmptyStackError),
                    Some(_) => Ok(())
                }
            }
            Command::Dup => {
                match kroth.peek() {
                    None => Err(EmptyStackError),
                    Some(val) => ok! { kroth.push(val.clone()) },
                }
            }
            Command::Swap => {
                ensure_size!(kroth, 2);
                let val = kroth.stack.remove(kroth.stack.len() - 2);
                ok! { kroth.push(val) }
            }
            Command::Over => {
                ensure_size!(kroth, 2);
                ok! { kroth.push(kroth.stack[kroth.stack.len() - 2].clone()) }
            }
            Command::Print => {
                match kroth.pop() {
                    None => Err(EmptyStackError),
                    Some(val) => ok! { println!("{val}") }
                }
            }
            Command::Input => {
                let mut buf = String::new();
                let _ = std::io::stdin().read_line(&mut buf);
                let buf = if buf.ends_with("\r\n") { &buf[0..buf.len() - 2] } else { &buf[0..buf.len() - 1] };
                ok! { kroth.push(KrothValue::String(Box::from(buf))) }
            }
            Command::Cast => {
                use RuntimeError::{CastError};

                let typ = match kroth.pop() {
                    None => { return Err(NotEnoughElements { expected: 2, found: 1 }); }
                    Some(KrothValue::Type(typ)) => typ,
                    Some(val) => { return Err(InvalidType { expected_type: KrothType::Typ, found_val: val }); }
                };
                let val = match kroth.pop() {
                    None => { return Err(NotEnoughElements { expected: 2, found: 0 }); }
                    Some(val) => val,
                };

                let res = match (typ, &val) {
                    (KrothType::Blck, _) => { return Err(CastError { to_type: KrothType::Blck, failed_val: val }); }
                    (typ, KrothValue::Block(_)) => { return Err(CastError { to_type: typ, failed_val: val }); }
                    (KrothType::Str, _) => KrothValue::String(Box::from(val.to_string())),
                    (KrothType::Num, KrothValue::Number(_)) => val,
                    (KrothType::Num, KrothValue::String(str)) => {
                        match str.parse::<BigDecimal>() {
                            Ok(num) => KrothValue::Number(num),
                            Err(_) => { return Err(CastError { to_type: KrothType::Num, failed_val: val }); }
                        }
                    }
                    (KrothType::Num, _) => { return Err(CastError { to_type: KrothType::Num, failed_val: val }); }
                    (KrothType::Typ, KrothValue::Number(_)) => { return Err(CastError { to_type: KrothType::Typ, failed_val: val }); }
                    (KrothType::Typ, KrothValue::Type(_)) => val,
                    (KrothType::Typ, KrothValue::String(str)) => {
                        match str.as_ref() {
                            "Number" => KrothValue::Type(KrothType::Num),
                            "String" => KrothValue::Type(KrothType::Str),
                            "Type" => KrothValue::Type(KrothType::Typ),
                            _ => err! {CastError { to_type: KrothType::Typ, failed_val: val }}
                        }
                    }
                };

                ok! { kroth.push(res) }
            }
            Command::Call => {
                match kroth.pop() {
                    None => err!(EmptyStackError),
                    Some(KrothValue::Block(commands)) => ok! {
                        for command in commands {
                            command.operate(kroth)?
                        }
                    },
                    Some(val) => err! { InvalidType {expected_type: KrothType::Blck, found_val: val} },
                }
            }
            Command::Push(val) => {
                ok! { kroth.stack.push(val) }
            }
        }
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    EmptyStackError,
    NotEnoughElements {
        expected: usize,
        found: usize,
    },
    InvalidType {
        expected_type: KrothType,
        found_val: KrothValue,
    },
    CastError {
        to_type: KrothType,
        failed_val: KrothValue,
    },
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::EmptyStackError => f.write_str("Stack was empty"),
            RuntimeError::NotEnoughElements { expected, found } => f.write_fmt(format_args!("Expected stack to have {expected} elements, but found {found}")),
            RuntimeError::InvalidType { expected_type, found_val } => f.write_fmt(format_args!("Value {found_val} of type {found_typ}, does not match expected type {expected_type}", found_typ = KrothType::of(found_val))),
            RuntimeError::CastError { to_type, failed_val } => f.write_fmt(format_args!("Failed to cast value {failed_val} of type {failed_typ} to type {to_type}", failed_typ = KrothType::of(failed_val)))
        }
    }
}

impl Error for RuntimeError {}