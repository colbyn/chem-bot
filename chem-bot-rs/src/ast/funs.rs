//! FunctionDecl definitions.
use std::rc::Rc;
use std::cell::RefCell;
use std::iter::FromIterator;
use std::path::{Path, PathBuf};
use std::convert::AsRef;
use std::collections::{HashMap, LinkedList, HashSet, VecDeque};
pub use num::rational::{Ratio, Rational};

use crate::ast::expr::{Expr, FunCall};
use crate::ast::value::Value;
use crate::*;

///////////////////////////////////////////////////////////////////////////////
// CONVERSION HELPERS
///////////////////////////////////////////////////////////////////////////////

pub trait ConvertTo<T> {
    fn convert_to(&self) -> Option<T>;
}

impl ConvertTo<isize> for Expr {
    fn convert_to(&self) -> Option<isize> {
        match self {
            Expr::Value(Value::Num(x)) => Some(*x),
            _ => None,
        }
    }
}
impl ConvertTo<Value> for Expr {
    fn convert_to(&self) -> Option<Value> {
        match self {
            Expr::Value(x) => Some(x.clone()),
            _ => None,
        }
    }
}
impl ConvertTo<Expr> for Expr {
    fn convert_to(&self) -> Option<Expr> {
        Some(self.clone())
    }
}



///////////////////////////////////////////////////////////////////////////////
// DATA TYPES
///////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct Body(Rc<dyn Fn(FunCall) -> Option<Expr>>);

impl std::fmt::Debug for Body {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Body").finish()
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Num
}

// #[derive(Debug, Clone)]
// pub enum Arg {
//     Keyword(String, )
// }

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub path: Vec<String>,
    pub pos_args: usize,
    pub key_args: Vec<String>,
    pub body: Body,
}

impl FunctionDecl {
    pub fn call(&self, source: Expr) -> Result<Expr, Expr> {
        let root_fun_call = return_fun_call!(Err, source.clone());
        match (&self.path.clone()[..], self.pos_args.clone()) {
            ([name1], 1) => {
                let fun_call = *root_fun_call.clone();
                let valid_name = &fun_call.name == &self.path[0];
                let valid_pos_args = fun_call.pos_args.len() == self.pos_args;
                let valid_key_args = self.key_args
                    .iter()
                    .all(|key| {
                        fun_call.key_args.contains_key(key)
                    });
                if valid_name && valid_pos_args && valid_key_args {
                    match (self.body.0)(fun_call.clone()) {
                        Some(x) => Ok(x),
                        None => Err(source),
                    }
                } else {
                    Err(source)
                }
            }
            ([name1, name2], _) => {
                let fun_call = *return_fun_call!(Err, return_fun_call_arg0!(Err, source.clone()));
                let valid_name = {
                    &root_fun_call.name == name1 &&
                    &fun_call.name == name2
                };
                let valid_pos_args = fun_call.pos_args.len() == self.pos_args;
                let valid_key_args = self.key_args
                    .iter()
                    .all(|key| {
                        fun_call.key_args.contains_key(key)
                    });
                if valid_name && valid_pos_args && valid_key_args {
                    match (self.body.0)(fun_call.clone()) {
                        Some(x) => Ok(x),
                        None => Err(source),
                    }
                } else {
                    Err(source)
                }
            }
            _ => unimplemented!()
        }
    }
}


///////////////////////////////////////////////////////////////////////////////
// MACRO DSL
///////////////////////////////////////////////////////////////////////////////

/// Internal helper.
#[macro_export]
macro_rules! init_arg_header {
    ($pos_counter:expr, $keyword_state:expr, argument $name:ident : $type:ty) => {
        $pos_counter = $pos_counter + 1;
    };
    ($pos_counter:expr, $keyword_state:expr, keyword $name:ident : $type:ty) => {
        $keyword_state.push(String::from(stringify!($name)));
    };
}

#[macro_export]
macro_rules! init_arg_scope {
    ($pos_args:expr, $key_args:expr, argument $name:ident : $type:ty) => {
        let $name: $type = $pos_args
            .pop_front()
            .and_then(|expr: Expr| -> Option<$type> {
                expr.convert_to()
            })?;
    };
    ($pos_args:expr, $key_args:expr, keyword $name:ident : $type:ty) => {
        let $name: $type = $key_args
            .get(stringify!($name))
            .and_then(|expr: &Expr| -> Option<$type> {
                expr.convert_to()
            })?;
    };
}

#[macro_export]
macro_rules! defintion {
    ($name:ident($($arg:tt)*$(,)?) => $body:expr) => {{
        let name = String::from(stringify!($name));
        let mut pos_counter = 0;
        let mut keyword_state = Vec::<String>::new();
        init_arg_header!(pos_counter, keyword_state, $($arg)*);
        let function_decl = FunctionDecl {
            path: vec![name],
            pos_args: pos_counter,
            key_args: keyword_state,
            body: Body(Rc::new(
                |call: FunCall| -> Option<Expr> {
                    let mut pos_args: LinkedList<Expr> = LinkedList::from_iter(call.pos_args.clone());
                    let mut key_args: HashMap<String, Expr> = call.key_args.clone();
                    init_arg_scope!(pos_args, key_args, $($arg)*);
                    Some($body)
                }
            )),
        };
        function_decl
    }};

    ($name1:ident => $name2:ident($($arg:tt)*$(,)?) => $body:expr) => {{
        let name1 = String::from(stringify!($name1));
        let name2 = String::from(stringify!($name2));
        let mut pos_counter = 0;
        let mut keyword_state = Vec::<String>::new();
        init_arg_header!(pos_counter, keyword_state, $($arg)*);
        let function_decl = FunctionDecl {
            path: vec![name1, name2],
            pos_args: pos_counter,
            key_args: keyword_state,
            body: Body(Rc::new(
                |call: FunCall| -> Option<Expr> {
                    let mut pos_args: LinkedList<Expr> = LinkedList::from_iter(call.pos_args.clone());
                    let mut key_args: HashMap<String, Expr> = call.key_args.clone();
                    init_arg_scope!(pos_args, key_args, $($arg)*);
                    Some($body)
                }
            )),
        };
        function_decl
    }};
}

///////////////////////////////////////////////////////////////////////////////
// FUNCTION DEFINITIONS
///////////////////////////////////////////////////////////////////////////////



///////////////////////////////////////////////////////////////////////////////
// DEV
///////////////////////////////////////////////////////////////////////////////

pub fn main() {
    let expr = Expr::from_str("nm(250)").unwrap();
    let nm_decl = defintion!(
        nm(argument value:isize) => {
            Expr::Value(Value::product(&[
                Value::num(value),
                Value::con("nm")
            ]))
        }
    );
    // let result = nm_decl.call(expr);
    // println!("{:#?}", result);
    // ------------------------------------------------------------------------
    let expr = Expr::from_str("energy(photon(wavelength = nm(325)))").unwrap();
    let decl1 = defintion!(
        energy => photon(keyword wavelength : Value) => {{
            let numerator = Value::product(&[
                Value::con("c"),
                Value::con("h"),
            ]);
            let denominator = wavelength;
            Expr::Value(Value::ratio(
                numerator,
                denominator,
            ))
        }}
    );
    let result = decl1.call(expr);
    println!("{:#?}", result);
}



