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
pub enum Expr {
    Value(Value),
    Call(Box<FunCall>),
}

macro_rules! return_value {
    ($expr:expr) => {{
        match $expr {
            Expr::Value(x) => x,
            x => return x,
        }
    }};
}

macro_rules! return_value_num {
    ($expr:expr) => {{
        match $expr {
            Expr::Value(Value::Num(x)) => x,
            x => return x,
        }
    }};
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
    pub fn unsafe_to_value(self) -> Value {
        match self {
            Expr::Value(x) => x,
            _ => panic!()
        }
    }
    pub fn eval(self) -> Value {
        match self {
            Expr::Call(call) => {
                unimplemented!()
            }
            Expr::Value(x) => x,
        }
    }
    pub fn trans(self, f: Rc<dyn Fn(Expr) -> Expr>) -> Expr {
        let result = match self {
            Expr::Value(x) => Expr::Value(x),
            Expr::Call(call) => {
                let pos_args = call.pos_args
                    .into_iter()
                    .map(|arg| {
                        arg.trans(f.clone())
                    })
                    .collect::<Vec<_>>();
                let key_args = call.key_args
                    .into_iter()
                    .map(|(key, arg)| {
                        (key, arg.trans(f.clone()))
                    })
                    .collect::<HashMap<_, _>>();
                Expr::Call(Box::new(FunCall {
                    name: call.name,
                    pos_args,
                    key_args,
                }))
            }
        };
        f(result)
    }
    pub fn trans_fun_wildcard(
        self,
        name: &str,
        f: Rc<dyn Fn(FunCall) -> Expr>
    ) -> Expr {
        self.trans(Rc::new({
            let name = name.to_owned();
            move |expr| {
                match expr {
                    Expr::Call(fun_call) if fun_call.name == name => {
                        f(*fun_call)
                    }
                    _ => expr
                }
            }
        }))
    }
    pub fn trans_fun(
        self,
        name: &str,
        pos_arg: usize,
        key_arg: Vec<String>,
        f: Rc<dyn Fn(FunCall) -> Expr>
    ) -> Expr {
        self.trans_fun_wildcard(name, Rc::new({
            let name = name.to_owned();
            move |fun_call| {
                let valid_name = fun_call.name == name;
                let valid_pos_args = fun_call.pos_args.len() == pos_arg;
                let valid_key_args = key_arg
                    .iter()
                    .all(|key| {
                        fun_call.key_args.contains_key(key)
                    });
                if valid_name && valid_pos_args && valid_key_args {
                    f(fun_call)
                } else {
                    Expr::Call(Box::new(fun_call))
                }
            }
        }))
    }
    pub fn trans_fun_arg1(
        self,
        name: &str,
        f: Rc<dyn Fn(Expr) -> Expr>
    ) -> Expr {
        self.trans_fun(
            name,
            1,
            vec![],
            Rc::new({
                move |fun_call: FunCall| {
                    let arg = fun_call.pos_args[0].clone();
                    f(arg)
                }
            }),
        )
    }
    pub fn trans_fun_key1(
        self,
        name: &str,
        key_arg: &str,
        f: Rc<dyn Fn(Expr) -> Expr>
    ) -> Expr {
        self.trans_fun(
            name,
            0,
            vec![key_arg.clone().to_owned()],
            Rc::new({
                let key_arg = key_arg.to_owned();
                move |fun_call: FunCall| {
                    let key_arg = key_arg.clone();
                    let arg = fun_call.key_args.get(&key_arg).unwrap();
                    f(arg.clone())
                }
            }),
        )
    }
    pub fn apply_rewrites(self) -> Self {
        let pass = self;
        let pass = pass.trans_fun_arg1(
            "nm",
            Rc::new(|arg: Expr| -> Expr {
                let value: isize = return_value_num!(arg);
                Expr::Value(Value::product(&[
                    Value::num(value),
                    Value::con("nm")
                ]))
            }
        ));
        let pass = pass.trans_fun_arg1(
            "energy",
            Rc::new(|arg: Expr| -> Expr {
                arg.trans_fun_key1(
                    "photon",
                    "wavelength",
                    Rc::new(|arg: Expr| -> Expr {
                        let wavelength = return_value!(arg);
                        let numerator = Value::product(&[
                            Value::con("c"),
                            Value::con("h"),
                        ]);
                        let denominator = wavelength;
                        Expr::Value(Value::ratio(
                            numerator,
                            denominator,
                        ))
                    }
                ))
            }
        ));
        pass
    }
}

///////////////////////////////////////////////////////////////////////////////
// DEV
///////////////////////////////////////////////////////////////////////////////

pub fn main() {
    let expr = Expr::from_str("energy(photon(wavelength = nm(325)))").unwrap();
    let expr = expr.apply_rewrites();
    let result = expr.unsafe_to_value().eval();
    println!("{}", result.to_string());
}

