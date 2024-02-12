#![allow(dead_code)]

use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::io::stdin;

use num::BigRational;

use crate::RuntimeError::{CastError, InvalidType};

macro_rules! ok {
    ($s:stmt) => {{
        $s
        Ok(())
    }};
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

fn main() {
    println!("Hello, world!");
}

struct Kroth {
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

#[derive(Copy, Clone)]
enum KrothType {
    Num,
    Str,
    Typ,
}

impl KrothType {
    fn of(val: &KrothValue) -> Self {
        use KrothType::*;
        match val {
            KrothValue::Number(_) => Num,
            KrothValue::String(_) => Str,
            KrothValue::Type(_) => Typ,
        }
    }
}

impl Display for KrothType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            KrothType::Num => f.write_str("Number"),
            KrothType::Str => f.write_str("String"),
            KrothType::Typ => f.write_str("Type")
        }
    }
}

#[derive(Clone)]
enum KrothValue {
    Number(BigRational),
    String(Box<str>),
    Type(KrothType),
}

impl Display for KrothValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            KrothValue::Number(num) => f.write_fmt(format_args!("{num}")),
            KrothValue::String(str) => f.write_str(str),
            KrothValue::Type(typ) => f.write_fmt(format_args!("{typ}")),
        }
    }
}

trait Operation {
    fn operate(self, kroth: &mut Kroth) -> Result<(), RuntimeError>;
}

struct Push(KrothValue);

impl Operation for Push {
    fn operate(self, kroth: &mut Kroth) -> Result<(), RuntimeError> {
        ok! { kroth.stack.push(self.0) }
    }
}

enum BasicCommand {
    Drop,
    Dup,
    Over,
    Swap,
    Print,
    Input,
    Cast,
}

impl Operation for BasicCommand {
    fn operate(self, kroth: &mut Kroth) -> Result<(), RuntimeError> {
        use RuntimeError::{EmptyStackError, NotEnoughElements};

        match self {
            BasicCommand::Drop => {
                match kroth.pop() {
                    None => Err(EmptyStackError),
                    Some(_) => Ok(())
                }
            }
            BasicCommand::Dup => {
                match kroth.peek() {
                    None => Err(EmptyStackError),
                    Some(val) => ok! { kroth.push(val.clone()) },
                }
            }
            BasicCommand::Swap => {
                ensure_size!(kroth, 2);
                let val = kroth.stack.remove(kroth.stack.len() - 2);
                ok! { kroth.push(val) }
            }
            BasicCommand::Over => {
                ensure_size!(kroth, 2);
                ok! { kroth.push(kroth.stack[kroth.stack.len() - 2].clone()) }
            }
            BasicCommand::Print => {
                match kroth.pop() {
                    None => Err(EmptyStackError),
                    Some(val) => ok! { println!("{val}") }
                }
            }
            BasicCommand::Input => {
                let mut buf = String::new();
                let _ = stdin().read_line(&mut buf);
                ok! { kroth.push(KrothValue::String(Box::from(buf))) }
            }
            BasicCommand::Cast => {
                let val = match kroth.pop() {
                    None => { return Err(NotEnoughElements { expected: 2, found: 0 }); }
                    Some(val) => val,
                };
                let typ = match kroth.pop() {
                    None => { return Err(NotEnoughElements { expected: 2, found: 1 }); }
                    Some(KrothValue::Type(typ)) => typ,
                    Some(val) => { return Err(InvalidType { expected: KrothType::Typ, found: KrothType::of(&val) }); }
                };

                let res = match typ {
                    KrothType::Str => { KrothValue::String(Box::from(val.to_string())) }
                    KrothType::Num => match val {
                        KrothValue::Number(_) => val,
                        KrothValue::String(ref str) => {
                            match str.parse::<BigRational>() {
                                Ok(num) => KrothValue::Number(num),
                                Err(_) => { return Err(CastError { to_type: KrothType::Num, failed_val: val }); }
                            }
                        }
                        KrothValue::Type(_) => { return Err(CastError { to_type: KrothType::Num, failed_val: val }); }
                    }
                    KrothType::Typ => match val {
                        KrothValue::Number(_) => { return Err(CastError { to_type: KrothType::Typ, failed_val: val }); }
                        KrothValue::String(ref str) => {
                            if str.as_ref() == "Number" {
                                KrothValue::Type(KrothType::Num)
                            } else if str.as_ref() == "String" {
                                KrothValue::Type(KrothType::Str)
                            } else if str.as_ref() == "Type" {
                                KrothValue::Type(KrothType::Typ)
                            } else {
                                return Err(CastError { to_type: KrothType::Typ, failed_val: val });
                            }
                        }
                        KrothValue::Type(_) => { val }
                    }
                };

                ok! { kroth.push(res) }
            }
        }
    }
}

enum RuntimeError {
    EmptyStackError,
    NotEnoughElements {
        expected: usize,
        found: usize,
    },
    InvalidType {
        expected: KrothType,
        found: KrothType,
    },
    CastError {
        to_type: KrothType,
        failed_val: KrothValue,
    },
}