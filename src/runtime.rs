use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};

use bigdecimal::{BigDecimal, One, Zero};

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
            err!(NotEnoughElements {
                expected: $size,
                found: len,
            });
        }
    };
}

macro_rules! pop_block {
    ($kroth:expr) => {
        match $kroth.pop() {
            Some(KrothValue::Block(block)) => block,
            Some(val) => err!(InvalidType { expected_type:KrothType::Block, found_val: val }),
            None => err!(EmptyStackError)
        }
    };
}

macro_rules! pop_bool {
    ($kroth:expr) => {
        match $kroth.pop() {
            Some(KrothValue::Bool(bool)) => bool,
            Some(val) => err!(InvalidType {expected_type: KrothType::Bool, found_val: val }),
            None => err!(EmptyStackError)
        }
    };
}

macro_rules! binary_math {
    ($kroth:expr, $op:tt) => {{
        let res = match $kroth.pops() {
            Some([KrothValue::Number(a), KrothValue::Number(b)]) => KrothValue::Number(a $op b),
            Some([KrothValue::Number(_), val] | [val, _]) => err!(RuntimeError::InvalidType {expected_type: KrothType::Num, found_val:val}),
            None => err!(RuntimeError::NotEnoughElements { expected: 2, found: $kroth.stack.len() })
        };
        ok! { $kroth.push(res) }
    }};
}

macro_rules! binary_logic {
    ($kroth:expr, $op:tt) => {{
        let res = match $kroth.pops() {
            Some([KrothValue::Bool(a), KrothValue::Bool(b)]) => KrothValue::Bool(a $op b),
            Some([KrothValue::Bool(_), val] | [val, _]) => err!(RuntimeError::InvalidType { expected_type: KrothType::Bool, found_val: val}),
            None => err!(RuntimeError::NotEnoughElements {expected: 2, found: $kroth.stack.len()}),
        };
        ok! { $kroth.push(res) }
    }};
}

macro_rules! binary_equality {
    ($kroth:expr, $equality:tt) => {{
        let res = match $kroth.pops() {
            Some([a, b]) => KrothValue::Bool(a $equality b),
            None => err!(RuntimeError::NotEnoughElements { expected: 2, found: $kroth.stack.len() })
        };
        ok! { $kroth.push(res) }
    }};
}

macro_rules! binary_cmp {
    ($kroth:expr, $cmp:tt) => {{
        let res = match $kroth.pops() {
            Some([KrothValue::Number(a), KrothValue::Number(b)]) => KrothValue::Bool(a $cmp b),
            Some([KrothValue::String(a), KrothValue::String(b)]) => KrothValue::Bool(a $cmp b),
            Some([KrothValue::Bool(a), KrothValue::Bool(b)]) => KrothValue::Bool(a $cmp b),
            Some([KrothValue::Number(_), val]) => err!(RuntimeError::InvalidType { expected_type: KrothType::Num, found_val: val }),
            Some([KrothValue::String(_), val]) => err!(RuntimeError::InvalidType { expected_type: KrothType::Str, found_val: val }),
            Some([KrothValue::Bool(_), val]) => err!(RuntimeError::InvalidType { expected_type: KrothType::Bool, found_val: val }),
            Some([ val, _ ]) => err!(RuntimeError::InvalidType { expected_type: KrothType::Num, found_val: val }),
            None => err!(RuntimeError::NotEnoughElements { expected: 2, found: $kroth.stack.len() })
        };
        ok! { $kroth.push(res) }
    }};
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
    pub fn pops<const N: usize>(&mut self) -> Option<[KrothValue; N]> {
        if self.stack.len() < N { return None; }
        let mut arr = [(); N].map(|_| self.stack.pop().unwrap());
        arr.reverse();
        Some(arr)
    }

    pub fn set(&mut self, name: &str, val: KrothValue) {
        self.vars.insert(Box::from(name), val);
    }

    pub fn run_program(&mut self, program: Vec<Command>) -> Result<(), RuntimeError> {
        ok! {
            for command in program {
                command.operate(self)?
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum KrothType {
    Num,
    Str,
    Typ,
    Block,
    Bool,
}

impl KrothType {
    fn of(val: &KrothValue) -> Self {
        use KrothType::*;
        match val {
            KrothValue::Number(_) => Num,
            KrothValue::String(_) => Str,
            KrothValue::Type(_) => Typ,
            KrothValue::Block(_) => Block,
            KrothValue::Bool(_) => Bool
        }
    }
}

impl Display for KrothType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            KrothType::Num => f.write_str("Number"),
            KrothType::Str => f.write_str("String"),
            KrothType::Typ => f.write_str("Type"),
            KrothType::Block => f.write_str("Block"),
            KrothType::Bool => f.write_str("Bool")
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum KrothValue {
    Number(BigDecimal),
    String(Box<str>),
    Type(KrothType),
    Block(Vec<Command>),
    Bool(bool),
}

impl Display for KrothValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            KrothValue::Number(num) => f.write_fmt(format_args!("{num}")),
            KrothValue::String(str) => f.write_str(str),
            KrothValue::Type(typ) => f.write_fmt(format_args!("<{typ}>")),
            KrothValue::Block(_) => f.write_str("{block}"),
            KrothValue::Bool(bool) => f.write_fmt(format_args!("{bool}"))
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Command {
    Drop,
    Dup,
    Over,
    Swap,
    Print,
    Input,
    Cast,
    Call,
    Rev,
    Rot,
    Empty,
    Count,
    Push(KrothValue),

    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Or,
    And,
    Not,
    Xor,

    Eq,
    Neq,
    Gt,
    Lt,
    GtEq,
    LtEq,

    If,
    IfElse,

    SetVar(Box<str>),
    GetVar(Box<str>),

    While,
}

impl Command {
    pub fn operate(self, kroth: &mut Kroth) -> Result<(), RuntimeError> {
        use RuntimeError::*;

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
                let typ = match kroth.pop() {
                    None => { err!(NotEnoughElements { expected: 2, found: 1 }); }
                    Some(KrothValue::Type(typ)) => typ,
                    Some(val) => { err!(InvalidType { expected_type: KrothType::Typ, found_val: val }); }
                };
                let val = match kroth.pop() {
                    None => { err!(NotEnoughElements { expected: 2, found: 0 }); }
                    Some(val) => val,
                };

                let res = match (typ, &val) {
                    (KrothType::Block, _) => { err!(CastError { to_type: KrothType::Block, failed_val: val }); }
                    (typ, KrothValue::Block(_)) => { err!(CastError { to_type: typ, failed_val: val }); }

                    (KrothType::Str, _) => KrothValue::String(Box::from(val.to_string())),

                    (KrothType::Num, KrothValue::Number(_)) => val,
                    (KrothType::Num, KrothValue::String(str)) => {
                        match str.parse::<BigDecimal>() {
                            Ok(num) => KrothValue::Number(num),
                            Err(_) => { err!(CastError { to_type: KrothType::Num, failed_val: val }); }
                        }
                    }
                    (KrothType::Num, &KrothValue::Bool(bool)) => if bool { KrothValue::Number(BigDecimal::one()) } else { KrothValue::Number(BigDecimal::zero()) }
                    (KrothType::Num, _) => { err!(CastError { to_type: KrothType::Num, failed_val: val }); }

                    (KrothType::Bool, KrothValue::Bool(_)) => val,
                    (KrothType::Bool, KrothValue::Number(num)) => KrothValue::Bool(!num.is_zero()),
                    (KrothType::Bool, KrothValue::String(str)) => match str.as_ref() {
                        "true" => KrothValue::Bool(true),
                        "false" => KrothValue::Bool(false),
                        _ => err!(CastError {to_type: KrothType::Bool, failed_val: val})
                    },
                    (KrothType::Bool, KrothValue::Type(_)) => err!(CastError { to_type: KrothType::Bool, failed_val: val }),

                    (KrothType::Typ, KrothValue::Number(_)) => { err!(CastError { to_type: KrothType::Typ, failed_val: val }); }
                    (KrothType::Typ, KrothValue::Type(_)) => val,
                    (KrothType::Typ, KrothValue::String(str)) => {
                        match str.as_ref() {
                            "Number" => KrothValue::Type(KrothType::Num),
                            "String" => KrothValue::Type(KrothType::Str),
                            "Type" => KrothValue::Type(KrothType::Typ),
                            _ => err! {CastError { to_type: KrothType::Typ, failed_val: val }}
                        }
                    }
                    (KrothType::Typ, KrothValue::Bool(_)) => err!(CastError {to_type: KrothType::Typ, failed_val: val})
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
                    Some(val) => err! { InvalidType {expected_type: KrothType::Block, found_val: val} },
                }
            }
            Command::Push(val) => {
                ok! { kroth.stack.push(val) }
            }
            Command::Add => {
                let res = match kroth.pops() {
                    Some([KrothValue::String(str), val]) => KrothValue::String(Box::from(str.to_string() + &val.to_string())),
                    Some([val, KrothValue::String(str)]) => KrothValue::String(Box::from(val.to_string() + str.as_ref())),
                    Some([KrothValue::Number(a), KrothValue::Number(b)]) => KrothValue::Number(a + b),
                    Some([val, KrothValue::Number(_)]) => err!(InvalidType {expected_type: KrothType::Num, found_val: val}),
                    Some([KrothValue::Number(_), val]) => err!(InvalidType {expected_type: KrothType::Num, found_val: val}),
                    Some([val, _]) => err!(InvalidType {expected_type: KrothType::Str, found_val: val}),
                    None => { err!(EmptyStackError); }
                };
                ok! { kroth.push(res) }
            }
            Command::Sub => binary_math!(kroth, -),
            Command::Mul => binary_math!(kroth, *),
            Command::Div => binary_math!(kroth, /),
            Command::Mod => binary_math!(kroth, %),

            Command::Not => {
                let res = match kroth.pop() {
                    Some(KrothValue::Bool(bool)) => KrothValue::Bool(!bool),
                    Some(val) => err!(InvalidType {expected_type: KrothType::Bool, found_val: val}),
                    None => err!(EmptyStackError)
                };
                ok! { kroth.push(res)}
            }
            Command::Or => binary_logic!(kroth, ||),
            Command::And => binary_logic!(kroth, &&),
            Command::Xor => binary_logic!(kroth, ^),

            Command::Eq => binary_equality!(kroth, ==),
            Command::Neq => binary_equality!(kroth, !=),

            Command::Gt => binary_cmp!(kroth, >),
            Command::Lt => binary_cmp!(kroth, <),
            Command::GtEq => binary_cmp!(kroth, >=),
            Command::LtEq => binary_cmp!(kroth, <=),

            Command::If => {
                let then_block = pop_block!(kroth);
                let cond = pop_bool!(kroth);
                ok! {
                    if cond {
                        kroth.run_program(then_block)?
                    }
                }
            }
            Command::IfElse => {
                let else_block = pop_block!(kroth);
                let then_block = pop_block!(kroth);
                let cond = pop_bool!(kroth);
                ok! {
                    if cond {
                        kroth.run_program(then_block)?
                    } else {
                        kroth.run_program(else_block)?
                    }
                }
            }
            Command::While => {
                let block = pop_block!(kroth);
                ok! {
                    while pop_bool!(kroth) {
                        kroth.run_program(block.clone())? // FIXME: Maybe make Command::operate take &self instead of self
                    }
                }
            }

            Command::SetVar(name) => {
                let Some(val) = kroth.pop() else { err!(EmptyStackError) };
                ok! { kroth.set(&name, val) }
            }
            Command::GetVar(name) => {
                let Some(val) = kroth.get(&name) else { err!(NoVarError(name)) };
                ok! { kroth.push(val.clone()) }
            }
            Command::Rev => {
                ok! { kroth.stack.reverse() }
            }
            Command::Rot => {
                ensure_size!(kroth, 3);
                let el = kroth.stack.remove(kroth.stack.len() - 3);
                ok! { kroth.push(el) }
            }
            Command::Empty => {
                ok! {
                    kroth.push(KrothValue::Bool(kroth.stack.is_empty()))
                }
            }
            Command::Count => {
                ok! {
                    kroth.stack.push(KrothValue::Number(BigDecimal::from(kroth.stack.len() as u128)))
                }
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
    NoVarError(Box<str>),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::EmptyStackError => f.write_str("Stack was empty"),
            RuntimeError::NotEnoughElements { expected, found } => f.write_fmt(format_args!("Expected stack to have {expected} elements, but found {found}")),
            RuntimeError::InvalidType { expected_type, found_val } => f.write_fmt(format_args!("Value {found_val} of type {found_typ}, does not match expected type {expected_type}", found_typ = KrothType::of(found_val))),
            RuntimeError::CastError { to_type, failed_val } => f.write_fmt(format_args!("Failed to cast value {failed_val} of type {failed_typ} to type {to_type}", failed_typ = KrothType::of(failed_val))),
            RuntimeError::NoVarError(name) => f.write_fmt(format_args!("Unknown variable name: {name}"))
        }
    }
}

impl Error for RuntimeError {}