use std::rc::Rc;
use std::cell::RefCell;
use std::iter::FromIterator;
use std::path::{Path, PathBuf};
use std::convert::AsRef;
use std::collections::{HashMap, LinkedList, HashSet};
pub use num::rational::{Ratio, Rational};

use crate::ast::value::{self, Value};

pub type Index = usize;
pub type KeyWord = String;


///////////////////////////////////////////////////////////////////////////////
// EXPRESSION AST (UNEVALUATED)
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub struct FunCall {
    pub name: String,
    /// positional arguments.
    pub pos_args: Vec<Expr>,
    /// keyword arguments.
    pub key_args: HashMap<String, Expr>,
}

impl FunCall {
    pub fn to_value(self) -> Value {
        match (self.name.as_str(), &self.pos_args[..]) {
            ("nm", [value]) => Value::Product(vec![
                value.clone().eval(),
                Value::Var(self.name),
            ]),
            ("photon", []) if self.key_args.contains_key("wavelength") => {
                unimplemented!()
            }
            ("photon", []) if self.key_args.contains_key("frequency") => {
                unimplemented!()
            }
            ("mole", [value]) => Value::Product(vec![
                unimplemented!()
            ]),
            ("energy", [argument]) => Value::Product(vec![
                unimplemented!()
            ]),
            _ => unimplemented!()
        }
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Num(Rational),
}

impl Literal {
    pub fn to_value(&self) -> Value {
        match self {
            Literal::Num(rat) => {
                let num = *rat.numer();
                let den = *rat.denom();
                if den == 1 {
                    return Value::Num(num)
                }
                if den == -1 {
                    return Value::Num(num * -1)
                }
                Value::ratio(
                    Value::Num(num),
                    Value::Num(den),
                )
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Call(Box<FunCall>),
    Literal(Literal),
}

impl Expr {
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Expr, ()> {
        let path = path.as_ref();
        let source = std::fs::read_to_string(path).unwrap();
        Expr::from_str(&source)
    }
    pub fn from_str(source: &str) -> Result<Expr, ()> {
        crate::ast::expr_parser::run_parser(source)
    }
    pub fn eval(self) -> Value {
        match self {
            Expr::Call(call) => {
                unimplemented!()
            }
            Expr::Literal(lit) => lit.to_value(),
        }
    }
}

///////////////////////////////////////////////////////////////////////////////
// DEV
///////////////////////////////////////////////////////////////////////////////

pub fn main() {
    // let value = Expr::from_file("sample.txt")
    //     .unwrap()
    //     .eval();
    // println!("EVALED: {:#?}", value);
}

