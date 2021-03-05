use std::rc::Rc;
use std::cell::RefCell;
use std::iter::FromIterator;
use std::path::{Path, PathBuf};
use std::convert::AsRef;
use std::collections::{HashMap, LinkedList, HashSet};
pub use num::rational::{Ratio, Rational};

pub type Index = usize;
pub type KeyWord = String;

///////////////////////////////////////////////////////////////////////////////
// HELPER TYPES
///////////////////////////////////////////////////////////////////////////////


pub enum Either<L, R> {
    Left(L),
    Right(R),
}

///////////////////////////////////////////////////////////////////////////////
// VALUE AST
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub enum Value {
    Num(isize),
    Var(String),
    Fraction(Box<Value>, Box<Value>),
    Product(Vec<Value>),
}

impl Value {
    fn num(x: isize) -> Self {Value::Num(x)}
    fn var(x: &str) -> Self {Value::Var(x.to_owned())}
    fn fraction(left: Value, right: Value) -> Self {
        Value::Fraction(
            Box::new(left),
            Box::new(right),
        )
    }
    fn product(xs: &[Value]) -> Self {
        Value::Product(xs.to_owned())
    }
    fn is_num(&self) -> bool {
        match self {
            Value::Num(_) => true,
            _ => false
        }
    }
    fn multiplicative_identity() -> Self {
        Value::Num(1)
    }
    fn unpack_num(&self) -> Option<isize> {
        match self {
            Value::Num(x) => Some(*x),
            _ => None
        }
    }
    fn unpack_type(&self) -> Option<String> {
        match self {
            Value::Var(x) => Some(x.clone()),
            _ => None
        }
    }
    fn unpack_fraction(self) -> Option<(Box<Value>, Box<Value>)> {
        match self {
            Value::Fraction(xs, ys) => Some((xs, ys)),
            _ => None
        }
    }
    fn unpack_product(self) -> Vec<Value> {
        match self {
            Value::Product(xs) => xs,
            _ => Vec::new()
        }
    }
    fn type_factors(&self) -> HashSet<String> {
        match self {
            Value::Num(x) => {
                HashSet::default()
            }
            Value::Var(x) => {
                HashSet::from_iter(vec![x.clone()])
            }
            Value::Fraction(top, bot) => top.type_factors(),
            Value::Product(xs) => {
                xs.iter().flat_map(|x| x.type_factors()).collect::<HashSet<_>>()
            }
        }
    }
    fn is_equal_to(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Num(x), Value::Num(y)) => x == y,
            (Value::Var(x), Value::Var(y)) => x == y,
            (Value::Fraction(xn, xd), Value::Fraction(yn, yd)) => {
                unimplemented!()
            }
            (Value::Product(xs), Value::Product(ys)) => {
                unimplemented!()
            }
            _ => unimplemented!()
        }
    }
    fn is_multiplicative_identity(&self) -> bool {
        match self {
            Value::Num(x) => {
                *x == 1
            }
            Value::Var(x) => false,
            Value::Fraction(top, bot) => {
                top.is_multiplicative_identity() &&
                bot.is_multiplicative_identity()
            }
            Value::Product(xs) => {
                xs.iter().all(Value::is_multiplicative_identity)
            }
        }
    }
    fn trans(self, f: impl Fn(Value) -> Option<Value>) -> Option<Value> {
        match self {
            Value::Num(x) => f(Value::Num(x)),
            Value::Var(x) => f(Value::Var(x)),
            Value::Fraction(top, bot) => {
                let top = f(*top);
                let bot = f(*bot);
                let result = match (top, bot) {
                    (Some(top), Some(bot)) => {
                        Some(Value::Fraction(
                            Box::new(top),
                            Box::new(bot),
                        ))
                    }
                    (Some(top), None) => Some(top),
                    (None, Some(bot)) => {
                        Some(Value::Fraction(
                            Box::new(Value::num(1)),
                            Box::new(bot),
                        ))
                    }
                    (None, None) => None,
                };
                f(result?)
            }
            Value::Product(xs) => {
                let xs = xs.into_iter().filter_map(|x| f(x)).collect::<Vec<_>>();
                match xs.len() {
                    0 => None,
                    1 => Some(xs[0].clone()),
                    _ => f(Value::Product(xs)),
                }
            }
        }
    }
    fn map(self, f: impl Fn(Value) -> Value) -> Value {
        self.trans(|x| Some(f(x))).unwrap()
    }
    fn abs(self) -> Self {
        self.map(|value| match value {
            Value::Num(x) => Value::Num(x.abs()),
            x => x
        })
    }
    fn multiply(self, value: Value) -> Self {
        match self {
            Value::Num(x) => {
                if let Some(value) = value.unpack_num() {
                    return Value::Num(x * value)
                }
                let result = vec![
                    vec![Value::Num(x)],
                    value.unpack_product(),
                ];
                Value::Product(result.concat())
            }
            Value::Var(x) => {
                if value.is_multiplicative_identity() {
                    Value::Var(x)
                } else {
                    Value::Product(vec![
                        value,
                        Value::Var(x),
                    ])
                }
            }
            Value::Fraction(top, bot) => {
                Value::Fraction(
                    Box::new(top.multiply(value)),
                    bot,
                )
            }
            Value::Product(mut xs) => {
                xs.push(value);
                Value::Product(xs)
            }
        }
    }
    fn add_product<P: Into<Vec<Value>>>(self, values: P) -> Self {
        let mut values = values.into();
        if values.is_empty() {
            return self
        }
        match self {
            Value::Fraction(top, bot) if top.is_multiplicative_identity() => {
                // values.push(Value::Fraction(
                //     Box::new(Value::num(1)),
                //     bot,
                // ));
                unimplemented!();
                Value::Product(values)
            }
            Value::Fraction(top, bot) => {
                values.push(*top);
                // values.push(Value::Fraction(
                //     Box::new(),
                //     bot,
                // ));
                unimplemented!();
                Value::Product(values)
            }
            Value::Product(xs) => {
                values.extend(xs);
                Value::Product(values)
            }
            Value::Num(x) => {
                values.push(Value::Num(x));
                Value::Product(values)
            }
            Value::Var(x) => {
                values.push(Value::Var(x));
                Value::Product(values)
            }
        }
    }
    /// Consider `None` as the multiplicative identity.
    fn cancel_factor(self, factor: &mut Option<Value>) -> Option<Self> {
        if factor.is_none() {
            return Some(self)
        }
        match (self, factor.as_mut().unwrap()) {
            (Value::Num(x), Value::Num(y)) => {
                // CHECK 1: REDUCE EQUAL VALUES & SIGNS
                let x_sign = x.signum();
                let y_sign = y.signum();
                if x.abs() == (*y).abs() {
                    *factor = None;
                    let result_sign = x_sign * y_sign;
                    if result_sign != 1 {
                        return Some(Value::Num(result_sign))
                    }
                    return None
                }
                // CHECK 2 GCD
                let gcd = num::integer::gcd(
                    x,
                    *y,
                );
                let x = x / gcd;
                *y = *y / gcd;
                // DONE
                return Some(Value::Num(x))
            }
            (Value::Var(x), Value::Var(y)) => {
                if x == *y {
                    *factor = None;
                    return None
                }
                return Some(Value::Var(x))
            }
            (Value::Fraction(xn, xd), Value::Fraction(yn, yd)) => {
                let mut yn = Some(*yn.clone());
                let mut yd = Some(*yd.clone());
                let xn = xn.cancel_factor(&mut yd);
                let xd = xd.cancel_factor(&mut yn);
                match (yn, yd) {
                    (Some(yn), Some(yd)) => {
                        *factor = Some(Value::Fraction(
                            Box::new(yn.clone()),
                            Box::new(yd.clone()),
                        ));
                    }
                    (Some(yn), None) => {
                        *factor = Some(yn.clone());
                    }
                    (None, Some(yd)) => {
                        *factor = Some(Value::Fraction(
                            Box::new(Value::multiplicative_identity()),
                            Box::new(yd.clone()),
                        ));
                    }
                    (None, None) => {
                        *factor = None;
                    }
                }
                match (xn, xd) {
                    (Some(xn), Some(xd)) => {
                        Some(Value::Fraction(
                            Box::new(xn.clone()),
                            Box::new(xd.clone()),
                        ))
                    }
                    (Some(xn), None) => {
                        Some(xn.clone())
                    }
                    (None, Some(xd)) => {
                        Some(Value::Fraction(
                            Box::new(Value::multiplicative_identity()),
                            Box::new(xd.clone()),
                        ))
                    }
                    (None, None) => {
                        None
                    }
                }
            }
            (Value::Fraction(xn, xd), y) => {
                let mut y = Some(y.clone());
                let xn = xn.cancel_factor(&mut y);
                *factor = y.clone();
                match xn {
                    Some(xn) => Some(Value::Fraction(
                        Box::new(xn),
                        xd,
                    )),
                    None => Some(Value::Fraction(
                        Box::new(Value::multiplicative_identity()),
                        xd,
                    )),
                }
            }
            (Value::Product(xs), _) => {
                let mut rs = Vec::<Value>::new();
                for x in xs {
                    if factor.is_some() {
                        match x.cancel_factor(factor) {
                            Some(x) => {
                                rs.push(x);
                            }
                            None => {}
                        }
                    } else {
                        rs.push(x);
                    }
                }
                if rs.is_empty() {
                    return None
                }
                if rs.len() == 1 {
                    return Some(rs[0].clone())
                }
                Some(Value::Product(rs))
            }
            (mut x, Value::Product(ys)) => {
                let mut x = Some(x);
                let mut ys = ys
                    .into_iter()
                    .map(|a| Some(a.clone()))
                    .collect::<Vec<_>>();
                for y in ys.iter_mut() {
                    if let Some(x_val) = x {
                        x = x_val.cancel_factor(y);
                    }
                }
                let ys = ys
                    .into_iter()
                    .filter_map(|x| x)
                    .collect::<Vec<_>>();
                if ys.is_empty() {
                    *factor = None;
                } else if ys.len() == 1 {
                    *factor = Some(ys[0].clone());
                } else {
                    *factor = Some(Value::Product(ys.clone()));
                }
                x.clone()
            }
            (Value::Num(x), Value::Var(_)) => Some(Value::Num(x)),
            (Value::Var(x), Value::Num(_)) => Some(Value::Var(x)),
            (Value::Num(1), Value::Product(_)) => Some(Value::Num(1)),
            (Value::Num(1), Value::Fraction(_, _)) => Some(Value::Num(1)),
            (left, right) => unimplemented!(
                "{:?} {:?}",
                left,
                right,
            )
        }
    }
    fn negate(self) -> Self {
        self.multiply(Value::Num(-1)).simplify()
    }
    fn reciprocal(self) -> Self {
        match self {
            Value::Num(x) => Value::Num(x),
            Value::Var(x) => Value::Var(x),
            Value::Fraction(top, bot) => Value::Fraction(bot, top),
            Value::Product(xs) => {
                let xs = xs
                    .into_iter()
                    .map(Value::reciprocal)
                    .collect();
                Value::Product(xs)
            }
        }
    }
    fn hoist_fractions(self, sink: &mut Vec<Value>) -> Option<Value> {
        match self {
            Value::Num(x) => Some(Value::Num(x)),
            Value::Var(x) => Some(Value::Var(x)),
            Value::Fraction(top, bot) => {
                let mut bot_sink = Vec::new();
                let bot = bot.hoist_fractions(&mut bot_sink);
                let top = top.hoist_fractions(sink);
                let bot_sink = bot_sink
                    .into_iter()
                    .map(Value::reciprocal)
                    .collect::<Vec<_>>();
                sink.extend(bot_sink);
                match (top, bot) {
                    (Some(top), None) => Some(top),
                    (None, None) => None,
                    (Some(top), Some(bot)) => {
                        Some(Value::Fraction(
                            Box::new(top),
                            Box::new(bot),
                        ))
                    }
                    (None, Some(bot)) => {
                        Some(Value::Fraction(
                            Box::new(unimplemented!()),
                            Box::new(bot),
                        ))
                    }
                }
            }
            Value::Product(xs) => {
                let mut rs = Vec::new();
                for x in xs {
                    match x.hoist_fractions(sink) {
                        Some(Value::Fraction(top, bot)) => {
                            sink.push(Value::Fraction(top, bot));
                        }
                        Some(x) => {
                            rs.push(x);
                        }
                        None => {}
                    }
                }
                match rs.len() {
                    0 => None,
                    1 => Some(rs[0].clone()),
                    _ => Some(Value::Product(rs)),
                }
            }
        }
    }
    fn simplify_impl(self) -> Option<Self> {
        match self {
            Value::Num(x) => Some(Value::Num(x)),
            Value::Var(x) => Some(Value::Var(x)),
            Value::Fraction(top, bot) => {
                let mut top_sink = Vec::new();
                let mut bot_sink = Vec::new();
                let top = top
                    .hoist_fractions(&mut top_sink)
                    .and_then(Value::simplify_impl)
                    .unwrap_or_else(|| unimplemented!());
                let mut bot = bot
                    .hoist_fractions(&mut bot_sink)
                    .and_then(Value::simplify_impl);
                let bot_sink = bot_sink
                    .into_iter()
                    .map(Value::reciprocal)
                    .collect::<Vec<_>>();
                let top = top.cancel_factor(&mut bot);
                let sink = vec![top_sink, bot_sink].concat();
                let sink = sink
                    .into_iter()
                    .filter(|x| !x.is_multiplicative_identity())
                    .collect::<Vec<_>>();
                match (top, bot) {
                    (Some(top), None) if top.is_multiplicative_identity() => {
                        Value::Product(sink).simplify_impl()
                    }
                    (Some(top), None) => {
                        Some(top.add_product(sink))
                    }
                    (Some(top), Some(bot)) if bot.is_multiplicative_identity() => {
                        Some(top.add_product(sink))
                    }
                    (Some(top), Some(bot)) if bot.clone().abs().is_multiplicative_identity() => {
                        Some(top.negate().add_product(sink))
                    }
                    (Some(top), Some(bot)) => {
                        let result = Value::Fraction(
                            Box::new(top),
                            Box::new(bot),
                        );
                        Some(result.add_product(sink))
                    }
                    (None, Some(bot)) => {
                        let result = Value::Fraction(
                            Box::new(unimplemented!()),
                            Box::new(bot),
                        );
                        Some(result.add_product(sink))
                    }
                    (None, None) => None,
                }
            }
            Value::Product(xs) => {
                let xs = xs
                    .into_iter()
                    .filter_map(Value::simplify_impl)
                    .filter(|x| !x.is_multiplicative_identity())
                    .collect::<Vec<_>>();
                match xs.len() {
                    0 => None,
                    1 => Some(xs[0].clone()),
                    _ => Some(Value::Product(xs)),
                }
            }
        }
    }
    fn simplify(self) -> Self {
        self.simplify_impl().unwrap_or_else(|| unimplemented!())
    }
}

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
            ("energy", [value]) => Value::Product(vec![
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
                Value::Fraction(
                    Box::new(Value::Num(num)),
                    Box::new(Value::Num(den)),
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
        crate::parser::run_parser(source)
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
    let value = Value::Fraction(
        Box::new(Value::Product(vec![
            Value::Num(100),
            Value::Var(String::from("nm")),
            Value::Var(String::from("a")),
        ])),
        Box::new(Value::Product(vec![
            Value::Num(-100),
            Value::Var(String::from("nm")),
            Value::Var(String::from("b")),
        ])),
    );
    let value = Value::Fraction(
        Box::new(Value::Product(vec![
            Value::Var(String::from("a")),
        ])),
        Box::new(Value::Product(vec![
            Value::Fraction(
                Box::new(Value::Product(vec![
                    Value::Var(String::from("a")),
                ])),
                Box::new(Value::Product(vec![
                    Value::Var(String::from("b")),
                ])),
            )
        ])),
    );
    // let mut sink = Vec::<Value>::new();
    // let value = value.hoist_fractions(&mut sink);
    println!("{:#?}", value.simplify());
    println!("-----------------------------------");
    // println!("{:#?}", sink);
}


