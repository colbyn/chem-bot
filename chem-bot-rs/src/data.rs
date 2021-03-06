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

fn for_each(
    input: Vec<Value>,
    f: Rc<dyn Fn(Value, Value) -> (Value, Value)>,
) -> Vec<Value> {
    let input_length = input.len();
    let mut left = LinkedList::<Value>::new();
    let mut right = LinkedList::<Value>::from_iter(input);
    let mut current = right.pop_front().unwrap();
    fn process(
        left: &mut LinkedList<Value>,
        right: &mut LinkedList<Value>,
        current: &mut Value,
        f: Rc<dyn Fn(Value, Value) -> (Value, Value)>,
    ) {
        for l in left.iter_mut() {
            let (new_l, new_current) = f(l.clone(), current.clone());
            *l = new_l;
            *current = new_current;
        }
        for r in right.iter_mut() {
            let (new_r, new_current) = f(r.clone(), current.clone());
            *r = new_r;
            *current = new_current;
        }
    }
    fn next(
        left: &mut LinkedList<Value>,
        right: &mut LinkedList<Value>,
        current: &mut Value,
        f: Rc<dyn Fn(Value, Value) -> (Value, Value)>,
    ) -> Option<()> {
        process(
            left,
            right,
            current,
            f.clone(),
        );
        left.push_back(current.clone());
        *current = right.pop_front()?;
        Some(())
    }
    let mut done = false;
    while !done {
        done = next(
            &mut left,
            &mut right,
            &mut current,
            f.clone(),
        ).is_none();
    }
    assert!(right.is_empty());
    assert_eq!(left.len(), input_length);
    // DONE
    left
        .into_iter()
        .filter(|x| !x.is_multiplicative_identity())
        .collect()
}


///////////////////////////////////////////////////////////////////////////////
// VALUE AST
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub enum Value {
    Num(isize),
    Var(String),
    /// 1/x
    Fraction(Box<Value>),
    Product(Vec<Value>),
}

impl Value {
    fn num(x: isize) -> Self {Value::Num(x)}
    fn var(x: &str) -> Self {Value::Var(x.to_owned())}
    fn ratio(numerator: Value, denominator: Value) -> Self {
        Value::Product(vec![
            numerator,
            Value::Fraction(
                Box::new(denominator),
            )
        ])
    }
    fn unit_fraction(denominator: Value) -> Self {
        Value::Fraction(Box::new(denominator))
    }
    fn product(xs: &[Value]) -> Self {
        Value::Product(xs.to_owned())
    }
    fn from_vec(xs: Vec<Value>) -> Option<Value> {
        let xs = xs
            .into_iter()
            .filter(|x| !x.is_multiplicative_identity())
            .collect::<Vec<_>>();
        match xs.len() {
            0 => None,
            1 => Some(xs[0].clone()),
            _ => Some(Value::Product(xs)),
        }
    }
    fn flatten(xs: Vec<Option<Value>>) -> Option<Value> {
        let xs = xs
            .into_iter()
            .filter_map(|x| x)
            .collect();
        Value::from_vec(xs)
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
    fn unpack_fraction(self) -> Option<Value> {
        match self {
            Value::Fraction(ys) => Some((*ys)),
            _ => None
        }
    }
    fn unpack_product(self) -> Vec<Value> {
        match self {
            Value::Product(xs) => xs,
            _ => Vec::new()
        }
    }
    fn is_equal_to(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Num(x), Value::Num(y)) => x == y,
            (Value::Var(x), Value::Var(y)) => x == y,
            (Value::Fraction(x), Value::Fraction(y)) => {
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
            Value::Fraction(bot) => {
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
            Value::Fraction(bot) => {
                let bot = f(*bot);
                let result = match (bot) {
                    (Some(bot)) => {
                        Some(Value::Fraction(
                            Box::new(bot),
                        ))
                    }
                    (None) => None,
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
            Value::Fraction(bot) => {
                Value::Fraction(
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
        if self.is_multiplicative_identity() {
            return match values.len() {
                0 => Value::multiplicative_identity(),
                1 => values[0].clone(),
                _ => Value::Product(values)
            }
        }
        match self {
            Value::Fraction(bot) => {
                values.push(Value::Fraction(
                    bot,
                ));
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
    fn is_equal(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Num(x), Value::Num(y)) => {
                x == y
            }
            (Value::Var(x), Value::Var(y)) => {
                x == y
            }
            (Value::Fraction(x), Value::Fraction(y)) => {
                x.is_equal(y)
            }
            (Value::Product(xs), Value::Product(ys)) => {
                let xs = xs
                    .clone()
                    .into_iter()
                    .filter(|x| !x.is_multiplicative_identity())
                    .collect::<Vec<_>>();
                let ys = ys
                    .clone()
                    .into_iter()
                    .filter(|x| !x.is_multiplicative_identity())
                    .collect::<Vec<_>>();
                let mut unmacthed_xs = Vec::<Value>::new();
                let mut unmacthed_ys = Vec::<Value>::new();
                for x in xs.iter() {
                    let mut has_match = false;
                    for y in ys.iter() {
                        if !has_match {
                            has_match = x.is_equal(y);
                            // println!("{:?} ~ {:?} -> {:?}", x, y, x.is_equal(y));
                        }
                    }
                    if !has_match {
                        unmacthed_xs.push(x.clone());
                    }
                }
                for y in ys.iter() {
                    let mut has_match = false;
                    for x in xs.iter() {
                        if !has_match {
                            has_match = y.is_equal(x);
                        }
                    }
                    if !has_match {
                        unmacthed_ys.push(y.clone());
                    }
                }
                let result = unmacthed_xs.len() == 0 && unmacthed_ys.len() == 0;
                println!("is_equal {:?} == {:?} -> {:?}", xs, ys, result);
                result
            }
            _ => false
        }
    }
    fn negate(self) -> Self {
        self.multiply(Value::Num(-1)).simplify()
    }
    fn reciprocal(self) -> Self {
        match self {
            Value::Num(x) => Value::unit_fraction(Value::Num(x)),
            Value::Var(x) => Value::unit_fraction(Value::Var(x)),
            Value::Fraction(bot) => *bot,
            Value::Product(xs) => {
                let xs = xs
                    .into_iter()
                    .map(Value::reciprocal)
                    .collect();
                Value::Product(xs)
            }
        }
    }
    /// Fractions and multiples.
    fn hoist_products(self, sink: &mut Vec<Value>) -> Option<Value> {
        match self {
            Value::Num(x) => Some(Value::Num(x)),
            Value::Var(x) => Some(Value::Var(x)),
            Value::Fraction(bot) => {
                let mut bot_sink = Vec::new();
                if let Some(bot) = bot.hoist_products(&mut bot_sink) {
                    bot_sink.push(bot);
                }
                let bot_sink = bot_sink
                    .into_iter()
                    .map(|x| x.reciprocal())
                    .collect::<Vec<_>>();
                sink.extend(bot_sink);
                None
            }
            Value::Product(xs) => {
                for x in xs {
                    match x.hoist_products(sink) {
                        Some(x) => {
                            sink.push(x);
                        }
                        None => {}
                    }
                }
                None
            }
        }
    }
    fn products(self) -> Vec<Self> {
        let mut sink = Vec::new();
        if let Some(rest) = self.hoist_products(&mut sink) {
            sink.push(rest);
        }
        sink
    }
    fn cancel_matching_factors(self) -> Option<Self> {
        let factors = self.clone().products();
        let result = for_each(
            factors,
            Rc::new(|left: Value, right: Value| -> (Value, Value) {
                if left.is_equal(&right.clone().reciprocal()) {
                    return (
                        Value::multiplicative_identity(),
                        Value::multiplicative_identity(),
                    )
                }
                (left, right)
            })
        );
        Value::from_vec(result)
    }
    fn simplify_impl(self) -> Option<Self> {
        match self {
            Value::Num(x) => Some(Value::Num(x)),
            Value::Var(x) => Some(Value::Var(x)),
            Value::Fraction(bot) => {
                bot
                    .cancel_matching_factors()
                    .map(|x| x.reciprocal())
            }
            Value::Product(xs) => {
                Value::Product(xs).cancel_matching_factors()
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

// pub fn main() {
//     let value = Value::ratio(
//         (Value::Product(vec![
//             Value::Num(100),
//             Value::Var(String::from("nm")),
//             Value::Var(String::from("a")),
//         ])),
//         (Value::Product(vec![
//             Value::Num(-100),
//             Value::Var(String::from("nm")),
//             Value::Var(String::from("b")),
//         ])),
//     );
//     let value = Value::ratio(
//         Value::Product(vec![
//             Value::Var(String::from("a")),
//         ]),
//         Value::ratio(
//             Value::Product(vec![
//                 Value::Var(String::from("a")),
//             ]),
//             Value::Product(vec![
//                 Value::Var(String::from("b")),
//             ]),
//         )
//     );
//     // let mut sink = Vec::<Value>::new();
//     // let value = value.hoist_products(&mut sink);
//     let value = value.simplify();
//     println!("-----------------------------------");
//     println!("{:#?}", value);
//     // println!("{:#?}", sink);
// }

pub fn main() {
    // let left = Value::product(&[
    //     Value::var("a"),
    //     Value::var("b"),
    //     Value::var("c"),
    // ]);
    // let mut right = Value::unit_fraction(Value::product(&[
    //     Value::var("a"),
    //     Value::var("b"),
    //     Value::var("d"),
    // ]));
    // let result = left.cancel_matching_factors(right);
    // println!("-----------------------------------");
    // println!("result: {:#?}", result);
    // // println!("factor: {:#?}", right);
    // let values = vec![
    //     Value::var("a"),
    //     Value::var("b"),
    //     Value::var("c"),
    //     Value::var("d"),
    //     Value::unit_fraction(Value::var("a")),
    //     Value::unit_fraction(Value::var("b")),
    //     Value::unit_fraction(Value::var("c")),
    // ];
    // let value = Value::Product(values);
    let value = Value::ratio(
        Value::Product(vec![
            Value::Var(String::from("a")),
            Value::Var(String::from("c")),
            Value::num(-25),
        ]),
        Value::product(&[
            Value::num(100),
            Value::ratio(
                Value::Product(vec![
                    Value::Var(String::from("a")),
                ]),
                Value::Product(vec![
                    Value::Var(String::from("b")),
                ]),
            )
        ])
    );
    let result = value.simplify();
    // let results = for_each(values, Rc::new(|left: Value, right: Value| {
    //     // println!("{:?}   <->   {:?}", left, right);
    //     let (left, right) = left.cancel_matching_factors(right);
    //     let left = left.unwrap_or(Value::multiplicative_identity());
    //     let right = right.unwrap_or(Value::multiplicative_identity());
    //     (left, right)
    // }));
    println!("^^^^^^^^^^^^^^^^^^^^^");
    println!("main result: {:#?}", result);
}

