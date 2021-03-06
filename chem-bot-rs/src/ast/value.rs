use std::rc::Rc;
use std::cell::RefCell;
use std::iter::FromIterator;
use std::path::{Path, PathBuf};
use std::convert::AsRef;
use std::collections::{HashMap, LinkedList, HashSet};
use num::{FromPrimitive, ToPrimitive, BigRational};


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

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Num(isize),
    Float(f64),
    Var(String),
    /// Represents symbolic constants
    Con(String),
    /// 1/x
    Fraction(Box<Value>),
    Product(Vec<Value>),
}

impl Value {
    pub fn num(x: isize) -> Self {Value::Num(x)}
    pub fn var(x: &str) -> Self {Value::Var(x.to_owned())}
    pub fn con(x: &str) -> Self {Value::Con(x.to_owned())}
    pub fn ratio(numerator: Value, denominator: Value) -> Self {
        Value::Product(vec![
            numerator,
            Value::Fraction(
                Box::new(denominator),
            )
        ])
    }
    pub fn unit_fraction(denominator: Value) -> Self {
        Value::Fraction(Box::new(denominator))
    }
    pub fn product(xs: &[Value]) -> Self {
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
    pub fn is_num(&self) -> bool {
        match self {
            Value::Num(_) => true,
            _ => false
        }
    }
    pub fn is_float(&self) -> bool {
        match self {
            Value::Float(_) => true,
            _ => false
        }
    }
    pub fn multiplicative_identity() -> Self {
        Value::Num(1)
    }
    fn unpack_num(&self) -> Option<isize> {
        match self {
            Value::Num(x) => Some(*x),
            _ => None
        }
    }
    fn unpack_float(&self) -> Option<f64> {
        match self {
            Value::Float(x) => Some(*x),
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
            Value::Float(x) => {
                *x == 1.0
            }
            Value::Var(x) => false,
            Value::Con(x) => false,
            Value::Fraction(bot) => {
                bot.is_multiplicative_identity()
            }
            Value::Product(xs) => {
                xs.iter().all(Value::is_multiplicative_identity)
            }
        }
    }
    fn trans(self, f: Rc<dyn Fn(Value) -> Option<Value>>) -> Option<Value> {
        match self {
            Value::Num(x) => f(Value::Num(x)),
            Value::Float(x) => f(Value::Float(x)),
            Value::Var(x) => f(Value::Var(x)),
            Value::Con(x) => f(Value::Con(x)),
            Value::Fraction(bot) => {
                let bot = bot.trans(f.clone())?;
                f(Value::Fraction(
                    Box::new(bot),
                ))
            }
            Value::Product(xs) => {
                let xs = xs
                    .into_iter()
                    .filter_map(|x| x.trans(f.clone()))
                    .filter_map(|x| f(x))
                    .collect::<Vec<_>>();
                match xs.len() {
                    0 => None,
                    1 => f(xs[0].clone()),
                    _ => f(Value::Product(xs)),
                }
            }
        }
    }
    fn map(self, f: Rc<dyn Fn(Value) -> Value>) -> Value {
        match self {
            Value::Num(x) => f(Value::Num(x)),
            Value::Float(x) => f(Value::Float(x)),
            Value::Var(x) => f(Value::Var(x)),
            Value::Con(x) => f(Value::Con(x)),
            Value::Fraction(bot) => {
                let bot = bot.map(f.clone());
                f(Value::Fraction(Box::new(bot)))
            }
            Value::Product(xs) => {
                let xs = xs
                    .into_iter()
                    .map(|x| x.map(f.clone()))
                    .collect::<Vec<_>>();
                f(Value::Product(xs))
            }
        }
    }
    fn abs(self) -> Self {
        self.map(Rc::new(|value| match value {
            Value::Num(x) => Value::Num(x.abs()),
            Value::Float(x) => Value::Float(x.abs()),
            x => x
        }))
    }
    pub fn expand_constants(self) -> Self {
        // 10e-9
        fn ten_to_neg_9() -> Value {
            Value::ratio(
                Value::num(1),
                Value::num(1000000000)
            )
        }
        fn speed_of_light() -> Value {
            Value::ratio(
                Value::product(&[
                    Value::num(299792458),
                    Value::con("m"),
                ]),
                Value::con("s")
            )
        }
        fn nm() -> Value {
            Value::product(&[
                Value::con("m"),
                ten_to_neg_9(),
            ])
        }
        fn planck_constant() -> Value {
            let x: f64 = 6.62607015 * (10.0f64.powi(-34));
            Value::product(&[
                Value::Float(x),
                Value::con("J"),
                Value::con("s"),
            ])
        }
        self.map(Rc::new(|value| {
            // println!("{:?}", value);
            match value {
                Value::Con(x) if &x == "c" => speed_of_light(),
                Value::Con(x) if &x == "nm" => nm(),
                Value::Con(x) if &x == "h" => planck_constant(),
                x => x
            }
        }))
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
            Value::Float(x) => {
                if let Some(value) = value.unpack_float() {
                    return Value::Float(x * value)
                }
                let result = vec![
                    vec![Value::Float(x)],
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
            Value::Con(x) => {
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
            Value::Float(x) => {
                values.push(Value::Float(x));
                Value::Product(values)
            }
            Value::Var(x) => {
                values.push(Value::Var(x));
                Value::Product(values)
            }
            Value::Con(x) => {
                values.push(Value::Con(x));
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
            (Value::Con(x), Value::Con(y)) => {
                x == y
            }
            (Value::Float(x), Value::Float(y)) => {
                x == y
            }
            (Value::Num(x), Value::Float(y)) => {
                *x as f64 == *y
            }
            (Value::Float(x), Value::Num(y)) => {
                *x == *y as f64
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
                // println!("is_equal {:?} == {:?} -> {:?}", xs, ys, result);
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
            Value::Float(x) => Value::unit_fraction(Value::Float(x)),
            Value::Var(x) => Value::unit_fraction(Value::Var(x)),
            Value::Con(x) => Value::unit_fraction(Value::Con(x)),
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
            Value::Float(x) => Some(Value::Float(x)),
            Value::Var(x) => Some(Value::Var(x)),
            Value::Con(x) => Some(Value::Con(x)),
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
                // CANCEL MATCHING FRACTIONS
                if left.is_equal(&right.clone().reciprocal()) {
                    return (
                        Value::multiplicative_identity(),
                        Value::multiplicative_identity(),
                    )
                }
                // REDUCE INT FRACTIONS
                match (left.unpack_num(), right.clone().reciprocal().unpack_num()) {
                    (Some(left), Some(right)) => {
                        let gcd = num::integer::gcd(left.abs(), right.abs());
                        let left_sign = left.signum();
                        let right_sign = right.signum();
                        let sign = left_sign * right_sign;
                        let left = (left.abs() / gcd) * sign;
                        let right = right.abs() / gcd;
                        return (
                            Value::Num(left),
                            Value::unit_fraction(Value::Num(right)),
                        )
                    }
                    _ => ()
                }
                match (left.unpack_num(), right.unpack_num()) {
                    (Some(left), Some(right)) => {
                        return (
                            Value::multiplicative_identity(),
                            Value::num(left * right),
                        )
                    }
                    _ => ()
                }
                match (left.unpack_float(), right.unpack_float()) {
                    (Some(left), Some(right)) => {
                        return (
                            Value::multiplicative_identity(),
                            Value::Float(left * right),
                        )
                    }
                    _ => ()
                }
                match (left.clone().reciprocal().unpack_num(), right.clone().reciprocal().unpack_num()) {
                    (Some(left), Some(right)) => {
                        return (
                            Value::multiplicative_identity(),
                            Value::unit_fraction(
                                Value::num(left * right)
                            ),
                        )
                    }
                    _ => ()
                }
                match (left.clone().reciprocal().unpack_float(), right.clone().reciprocal().unpack_float()) {
                    (Some(left), Some(right)) => {
                        return (
                            Value::multiplicative_identity(),
                            Value::unit_fraction(
                                Value::Float(left * right)
                            ),
                        )
                    }
                    _ => ()
                }
                // DONE (NOTHING TO DO)
                (left, right)
            })
        );
        Value::from_vec(result)
    }
    fn simplify_impl(self) -> Option<Self> {
        match self {
            Value::Num(x) => Some(Value::Num(x)),
            Value::Float(x) => Some(Value::Float(x)),
            Value::Var(x) => Some(Value::Var(x)),
            Value::Con(x) => Some(Value::Con(x)),
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
    pub fn simplify(self) -> Self {
        self.simplify_impl().unwrap_or_else(|| unimplemented!())
    }
    pub fn eval(self) -> Self {
        let mut done = false;
        let mut state = self;
        fn cycle(inpt: Value) -> Value {
            let f = Rc::new(|expr| match expr {
                Value::Num(x) => Value::Float(x as f64),
                Value::Fraction(x) if x.is_float() => {
                    let x = x.unpack_float().unwrap();
                    Value::Float(x.powi(-1))
                }
                x => x
            });
            inpt.expand_constants()
                .map(f.clone())
                .simplify()
                .map(f.clone())
                .simplify()    
        }
        while !done {
            let latest = cycle(state.clone());
            if latest == state {
                done = true;
            }
            // println!("{:?} ~ {:?}", latest, state);
            state = latest;
        }
        state
    }
    pub fn to_string(&self) -> String {
        match self {
            Value::Num(x) => {
                format!("{}", x)
            }
            Value::Float(x) => {
                format!("{:e}", x)
            }
            Value::Var(x) => {
                x.clone()
            }
            Value::Con(x) => {
                x.clone()
            }
            Value::Fraction(bot) => {
                format!("1/{}", bot.to_string())
            }
            Value::Product(xs) => {
                xs
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            }
        }
    }
}



///////////////////////////////////////////////////////////////////////////////
// DEV
///////////////////////////////////////////////////////////////////////////////

pub fn main() {
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
    println!("^^^^^^^^^^^^^^^^^^^^^");
    println!("main result: {:#?}", result);
}

