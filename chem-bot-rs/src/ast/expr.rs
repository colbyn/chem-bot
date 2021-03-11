use std::rc::Rc;
use std::cell::RefCell;
use std::iter::FromIterator;
use std::path::{Path, PathBuf};
use std::convert::AsRef;
use std::collections::{HashMap, LinkedList, HashSet};
use num::{FromPrimitive, ToPrimitive, BigRational, BigInt, Signed};

use crate::*;

pub type Index = usize;
pub type KeyWord = String;

///////////////////////////////////////////////////////////////////////////////
// INTERNAL UTILS
///////////////////////////////////////////////////////////////////////////////

fn for_each(
    input: Vec<Expr>,
    f: Rc<dyn Fn(Expr, Expr) -> (Expr, Expr)>,
) -> Vec<Expr> {
    let input_length = input.len();
    let mut left = LinkedList::<Expr>::new();
    let mut right = LinkedList::<Expr>::from_iter(input);
    let mut current = right.pop_front().unwrap();
    fn process(
        left: &mut LinkedList<Expr>,
        right: &mut LinkedList<Expr>,
        current: &mut Expr,
        f: Rc<dyn Fn(Expr, Expr) -> (Expr, Expr)>,
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
        left: &mut LinkedList<Expr>,
        right: &mut LinkedList<Expr>,
        current: &mut Expr,
        f: Rc<dyn Fn(Expr, Expr) -> (Expr, Expr)>,
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
// MATRIX DATA TYPE
///////////////////////////////////////////////////////////////////////////////

#[derive(Clone, PartialEq)]
struct Row(LinkedList<Expr>);

/// A dynamic matrix in **row major order**.
#[derive(Clone, PartialEq)]
pub struct Matrix(LinkedList<Row>);

impl Matrix {
    pub fn new() -> Self {Matrix(Default::default())}
    pub fn from_rows(rows: LinkedList<LinkedList<Expr>>) -> Option<Self> {
        let mut column_size = None;
        let mut all_valid = true;
        for row in rows.iter() {
            match column_size {
                None => {
                    column_size = Some(row.len());
                }
                Some(ix) => {
                    all_valid = row.len() == ix;
                }
            }
        }
        let rows = rows
            .into_iter()
            .map(Row)
            .collect();
        if all_valid {
            Some(Matrix(rows))
        } else {
            None
        }
    }
    pub fn append_row<T: Into<LinkedList<Expr>>>(&mut self, row: T) {
        self.0.push_back(Row(row.into()));
    }
    pub fn append_column<T: Into<LinkedList<Expr>>>(
        &mut self,
        column: T,
        on_empty: fn() -> Expr,
    ) {
        let mut column = column.into();
        for row in self.0.iter_mut() {
            let node = column
                .pop_front()
                .unwrap_or_else(on_empty);
            row.0.push_back(node);
        }
    }
    pub fn to_string(&self) -> String {
        let mut rows = Vec::<String>::new();
        let mut max_column_len = 0;
        for row in self.0.iter() {
            let mut column = Vec::<String>::new();
            for expr in row.0.iter() {
                column.push(expr.to_string());
            }
            let column = column.join(", ");
            if column.len() > max_column_len {
                max_column_len = column.len();
            }
            rows.push(column);
        }
        for row in rows.iter_mut() {
            let added_len = max_column_len - row.len();
            let spaces = (0..=added_len).map(|_| " ").collect::<String>();
            *row = format!("  │{}{}│", row, spaces);
        }
        rows.join("\n")
    }
    pub fn rref(&self) -> Self {
        for row in self.0.iter() {
            
        }
        unimplemented!()
    }
}

impl std::fmt::Display for Matrix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl std::fmt::Debug for Matrix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut rows = self.0
            .iter()
            .map(|row| -> String {
                let row = row.0.iter()
                    .map(|x| format!("{:?}", x))
                    .collect::<Vec<_>>()
                    .join(",");
                format!("  [{}]", row)
            })
            .collect::<Vec<_>>()
            .join(",\n");
        write!(f, "[\n{}\n]", rows)
    }
}


///////////////////////////////////////////////////////////////////////////////
// EXPRESSION AST
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub struct FunCall {
    pub name: String,
    /// positional arguments.
    pub pos_args: Vec<Expr>,
    /// keyword arguments.
    pub key_args: HashMap<String, Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Symbol {
    Con(String),
    Var(String),
}

impl Symbol {
    pub fn id(&self) -> &str {
        match self {
            Symbol::Con(x) => x,
            Symbol::Var(x) => x,
        }
    }
    pub fn is_con_id(&self, con: &str) -> bool {
        match self {
            Symbol::Con(x) => x == con,
            Symbol::Var(_) => false,
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum Expr {
    Num(BigRational),
    Sym(Symbol),
    /// 1/x
    Fraction(Box<Expr>),
    Product(Vec<Expr>),
    Call(Box<FunCall>),
}

impl Expr {
    pub fn int<T: Into<i64>>(x: T) -> Self {
        let x = x.into();
        Expr::Num(BigRational::from_i64(x).unwrap())
    }
    pub fn float<T: Into<f64>>(x: T) -> Self {
        let x = x.into();
        Expr::Num(BigRational::from_f64(x).unwrap())
    }
    pub fn con(x: &str) -> Self {
        Expr::Sym(Symbol::Con(x.to_owned()))
    }
    pub fn var(x: &str) -> Self {
        Expr::Sym(Symbol::Var(x.to_owned()))
    }
    pub fn ratio(numerator: Expr, denominator: Expr) -> Self {
        Expr::Product(vec![
            numerator,
            Expr::Fraction(
                Box::new(denominator),
            )
        ])
    }
    // pub fn joule_second() -> Self {
    //     Expr::Product(vec![
    //         Expr::con("J"),
    //         Expr::con("s"),
    //     ])
    // }
    pub fn planck_constant() -> Self {
        Expr::con("h")
    }
    pub fn speed_of_light() -> Self {
        Expr::con("c")
    }
    // pub fn meter_per_sec() -> Self {
    //     Expr::ratio(
    //         Expr::con("m"),
    //         Expr::con("s"),
    //     )
    // }
    pub fn hertz() -> Self {
        Expr::con("Hz")
    }
    pub fn gigahertz() -> Self {
        Expr::Product(vec![
            Expr::hertz(),
            Expr::float(10.0f64.powi(9)),
        ])
    }
    pub fn megahertz() -> Self {
        Expr::Product(vec![
            Expr::hertz(),
            Expr::int(1000000),
        ])
    }
    pub fn avogadro_number() -> Self {
        Expr::float(
            6.02214076f64 * (10.0f64).powi(23)
        )
    }
    pub fn rydberg_constant() -> Self {
        Expr::con("Rₕ")
    }
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Expr, ()> {
        let path = path.as_ref();
        let source = std::fs::read_to_string(path).unwrap();
        Expr::from_str(&source)
    }
    pub fn from_str(source: &str) -> Result<Expr, ()> {
        crate::ast::expr_parser::run_parser(source)
    }
    fn from_vec(xs: Vec<Expr>) -> Option<Expr> {
        let xs = xs
            .into_iter()
            .filter(|x| !x.is_multiplicative_identity())
            .collect::<Vec<_>>();
        match xs.len() {
            0 => None,
            1 => Some(xs[0].clone()),
            _ => Some(Expr::Product(xs)),
        }
    }
    fn flatten(xs: Vec<Option<Expr>>) -> Option<Expr> {
        let xs = xs
            .into_iter()
            .filter_map(|x| x)
            .collect();
        Expr::from_vec(xs)
    }
    pub fn is_num(&self) -> bool {
        match self {
            Expr::Num(_) => true,
            _ => false
        }
    }
    pub fn is_sym(&self) -> bool {
        match self {
            Expr::Sym(_) => true,
            _ => false
        }
    }
    pub fn unit_fraction(val: Expr) -> Expr {
        // Expr::Fraction(Box::new(val))
        val.reciprocal()
    }
    fn unpack_num(&self) -> Option<BigRational> {
        match self {
            Expr::Num(x) => Some(x.clone()),
            _ => None
        }
    }
    fn unpack_sym_id(&self) -> Option<&str> {
        match self {
            Expr::Sym(x) => Some(x.id()),
            _ => None
        }
    }
    pub fn is_multiplicative_identity(&self) -> bool {
        match self {
            Expr::Num(x) => *x == BigRational::from_i32(1).unwrap(),
            Expr::Sym(_) => false,
            Expr::Fraction(x) => x.is_multiplicative_identity(),
            Expr::Product(xs) => xs.iter().all(|x| x.is_multiplicative_identity()),
            Expr::Call(_) => false,
        }
    }
    pub fn multiplicative_identity() -> Self {
        Expr::Num(BigRational::from_i32(1).unwrap())
    }
    pub fn trans(self, f: Rc<dyn Fn(Expr) -> Expr>) -> Expr {
        let result = match self {
            Expr::Num(x) => Expr::Num(x),
            Expr::Sym(x) => Expr::Sym(x),
            Expr::Fraction(x) => Expr::Fraction(Box::new(x.trans(f.clone()))),
            Expr::Product(xs) => Expr::Product(
                xs  .into_iter()
                    .map(|x| x.trans(f.clone()))
                    .collect::<Vec<_>>()
            ),
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
    fn abs(self) -> Self {
        self.trans(Rc::new(|value| match value {
            Expr::Num(x) => Expr::Num(num::abs(x)),
            x => x
        }))
    }
    fn reciprocal(&self) -> Self {
        match self {
            Expr::Num(x) => {
                let num = x.numer().clone();
                let den = x.denom().clone();
                Expr::Num(BigRational::new(den, num))
            },
            Expr::Sym(x) => Expr::Fraction(Box::new(
                Expr::Sym(x.clone())
            )),
            Expr::Fraction(bot) => *bot.clone(),
            Expr::Product(xs) => {
                let xs = xs
                    .into_iter()
                    .map(Expr::reciprocal)
                    .collect();
                Expr::Product(xs)
            }
            Expr::Call(x) => Expr::Fraction(Box::new(
                Expr::Call(x.clone())
            )),
        }
    }
    fn is_equal(&self, other: &Expr) -> bool {
        fn match_xs_ys(xs: &[Expr], ys: &[Expr]) -> bool {
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
            let mut unmacthed_xs = Vec::<Expr>::new();
            let mut unmacthed_ys = Vec::<Expr>::new();
            for x in xs.iter() {
                let mut has_match = false;
                for y in ys.iter() {
                    if !has_match {
                        has_match = x.is_equal(y);
                    }
                }
                if !has_match {
                    unmacthed_xs.push(x.to_owned().clone());
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
                    unmacthed_ys.push(y.to_owned().clone());
                }
            }
            let result = unmacthed_xs.len() == 0 && unmacthed_ys.len() == 0;
            // println!("is_equal {:?} == {:?} -> {:?}", xs, ys, result);
            result
        }
        fn match_hashmap(xs: &HashMap<String, Expr>, ys: &HashMap<String, Expr>) -> bool {
            let keys1 = xs
                .keys()
                .map(|x| x.clone())
                .collect::<HashSet<_>>();
            let keys2 = ys
                .keys()
                .map(|y| y.clone())
                .collect::<HashSet<_>>();
            if keys1 == keys2 {
                let xs = xs.values().map(|x| x.clone()).collect::<Vec<_>>();
                let ys = ys.values().map(|x| x.clone()).collect::<Vec<_>>();
                return match_xs_ys(&xs, &ys)
            }
            false
        }
        match (self, other) {
            (Expr::Num(x), Expr::Num(y)) => {x == y}
            (Expr::Sym(x), Expr::Sym(y)) => {x == y}
            (Expr::Fraction(x), Expr::Fraction(y)) => {x.is_equal(y)}
            (Expr::Product(xs), Expr::Product(ys)) => match_xs_ys(xs, ys),
            (Expr::Call(x), Expr::Call(y)) if x.name == y.name => {
                let check2 = match_xs_ys(
                    &x.pos_args,
                    &y.pos_args,
                );
                let check3 = match_hashmap(
                    &x.key_args,
                    &y.key_args,
                );
                check2 && check3
            }
            // FALSE
            (Expr::Num(_), _) => false,
            (Expr::Sym(_), _) => false,
            (Expr::Fraction(_), _) => false,
            (Expr::Product(_), _) => false,
            (Expr::Call(_), _) => false,
        }
    }
    fn hoist_products(self, sink: &mut Vec<Expr>) {
        match self {
            Expr::Num(x) => {
                sink.push(Expr::Num(x));
            }
            Expr::Sym(x) => {
                sink.push(Expr::Sym(x));
            }
            Expr::Fraction(bot) => {
                let mut xs = Vec::new();
                bot.hoist_products(&mut xs);
                let xs = xs
                    .into_iter()
                    .map(|x| x.reciprocal())
                    .collect::<Vec<_>>();
                sink.extend(xs);
            }
            Expr::Product(xs) => {
                for x in xs {
                    x.hoist_products(sink)
                }
            }
            Expr::Call(x) => {
                sink.push(Expr::Call(x));
            }
        }
    }
    fn products(self) -> Vec<Self> {
        let mut sink = Vec::new();
        self.hoist_products(&mut sink);
        sink
    }
    fn cancel_matching_factors(self) -> Option<Self> {
        fn compute_gcd(left: BigRational, right: BigRational) -> BigInt {
            use num::Integer;
            let left_d = left.denom();
            let right_d = right.denom();
            left_d.gcd(right_d)
        }
        let factors = self.clone().products();
        let result = for_each(
            factors,
            Rc::new(|left: Expr, right: Expr| -> (Expr, Expr) {
                // FUNCTION HELPERS
                fn if_match<T>(
                    left: Option<T>,
                    right: Option<T>,
                    f: impl Fn(T, T)->(Expr, Expr),
                ) -> Option<(Expr, Expr)> {
                    match (left, right) {
                        (Some(l), Some(r)) => Some(f(l, r)),
                        _ => None
                    }
                }
                fn if_match_<T>(
                    left: Option<T>,
                    right: Option<T>,
                    f: impl Fn(T, T)->Option<(Expr, Expr)>,
                ) -> Option<(Expr, Expr)> {
                    match (left, right) {
                        (Some(l), Some(r)) => f(l, r),
                        _ => None
                    }
                }
                // CANCEL MATCHING FRACTIONS
                if left.is_equal(&right.clone().reciprocal()) {
                    return (
                        Expr::multiplicative_identity(),
                        Expr::multiplicative_identity(),
                    )
                }
                // REDUCE INT FRACTIONS
                use num::Integer;
                fn only_int(x: BigRational) -> Option<BigRational> {
                    if x.denom() == &BigInt::from_i64(1).unwrap() {
                        Some(x)
                    } else {
                        None
                    }
                }
                return_some!(if_match_(
                    left.unpack_num()
                        .and_then(only_int)
                        .and_then(|x| x.to_i32()),
                    right.clone()
                         .reciprocal()
                         .unpack_num()
                         .and_then(only_int)
                         .and_then(|x| x.to_i32()),
                    |left, right| {
                        let left_original = left;
                        let right_original = right;
                        let gcd = num::integer::gcd(left.abs(), right.abs());
                        let left_sign = left.signum();
                        let right_sign = right.signum();
                        let sign = left_sign * right_sign;
                        let left = (left.abs() / gcd) * sign;
                        let right = right.abs() / gcd;
                        assert!(left != 0);
                        assert!(right != 0);
                        Some((
                            Expr::int(left as i64),
                            Expr::unit_fraction(Expr::int(right as i64)),
                        ))
                    }
                ));
                return_some!(if_match(
                    left.unpack_num(),
                    right.unpack_num(),
                    |left, right| (
                        Expr::multiplicative_identity(),
                        Expr::Num(left * right),
                    )
                ));
                return_some!(if_match(
                    left.clone().reciprocal().unpack_num(),
                    right.clone().reciprocal().unpack_num(),
                    |left, right| (
                        Expr::multiplicative_identity(),
                        Expr::unit_fraction( Expr::Num(left * right)),
                    )
                ));
                return_some!(if_match_(
                    left.unpack_sym_id(),
                    right.unpack_sym_id(),
                    |left, right| match (left, right) {
                        ("Hz", "s") => Some((
                            Expr::multiplicative_identity(),
                            Expr::multiplicative_identity(),
                        )),
                        _ => None
                    }
                ));
                return_some!(if_match_(
                    left.reciprocal().unpack_sym_id(),
                    right.reciprocal().unpack_sym_id(),
                    |left, right| match (left, right) {
                        ("Hz", "s") => Some((
                            Expr::multiplicative_identity(),
                            Expr::multiplicative_identity(),
                        )),
                        _ => None
                    }
                ));
                // DONE (NOTHING TO DO)
                (left, right)
            })
        );
        Expr::from_vec(result)
    }
    fn simplify_impl(self) -> Option<Self> {
        match self {
            Expr::Num(x) => Some(Expr::Num(x)),
            Expr::Sym(x) => Some(Expr::Sym(x)),
            Expr::Fraction(bot) => {
                let bot = bot
                    .simplify_impl()
                    .and_then(Expr::cancel_matching_factors)
                    .unwrap_or(Expr::multiplicative_identity());
                let mut xs = Vec::<Expr>::new();
                bot.hoist_products(&mut xs);
                let xs = xs
                    .into_iter()
                    .map(|x| x.reciprocal())
                    .collect::<Vec<_>>();
                Expr::from_vec(xs)
            }
            Expr::Product(xs) => {
                let xs = xs
                    .into_iter()
                    .filter_map(Expr::simplify_impl)
                    .collect::<Vec<_>>();
                Expr::Product(xs).cancel_matching_factors()
            }
            Expr::Call(call) => {
                let pos_args = call.pos_args
                    .into_iter()
                    .map(|x| {
                        x   .simplify_impl()
                            .unwrap_or_else(|| unimplemented!())
                    })
                    .collect::<Vec<_>>();
                let key_args = call.key_args
                    .into_iter()
                    .map(|(key, x)| {
                        let x = x
                            .simplify_impl()
                            .unwrap_or(Expr::multiplicative_identity());
                        (key, x)
                    })
                    .collect::<HashMap<_, _>>();
                Some(Expr::Call(Box::new(FunCall {
                    name: call.name,
                    pos_args,
                    key_args,
                })))
            }
        }
    }
    fn simplify(self) -> Self {
        self.simplify_impl().unwrap_or(Expr::multiplicative_identity())
    }
    fn expand_constants(self) -> Self {
        // 10e-9
        fn ten_to_neg_9() -> Expr {
            Expr::ratio(
                Expr::int(1),
                Expr::int(1000000000)
            )
        }
        fn speed_of_light() -> Expr {
            Expr::ratio(
                Expr::Product(vec![
                    Expr::int(299792458),
                    Expr::con("m"),
                ]),
                Expr::con("s")
            )
        }
        fn nm() -> Expr {
            Expr::Product(vec![
                Expr::con("m"),
                ten_to_neg_9(),
            ])
        }
        fn planck_constant() -> Expr {
            let x: f64 = 6.62607015 * (10.0f64.powi(-34));
            Expr::Product(vec![
                Expr::float(x),
                Expr::con("J"),
                Expr::con("s"),
            ])
        }
        fn rydberg_constant() -> Expr {
            let x: f64 = 1.09678f64 * 10.0f64.powi(7);
            Expr::Product(vec![
                Expr::float(x),
                Expr::unit_fraction(Expr::con("m"))
            ])
        }
        self.trans(Rc::new(|value| {
            match value {
                Expr::Sym(x) if x.is_con_id("c") => speed_of_light(),
                Expr::Sym(x) if x.is_con_id("nm") => nm(),
                Expr::Sym(x) if x.is_con_id("h") => planck_constant(),
                x if x == Expr::rydberg_constant() => rydberg_constant(),
                x => x
            }
        }))
    }
    pub fn to_string(&self) -> String {
        match self {
            Expr::Num(x) => {
                fn run_basic(x: BigRational) -> Option<String> {
                    let sign = {
                        if x < BigRational::from_f32(0.0).unwrap() {
                            "−"
                        } else {
                            ""
                        }
                    };
                    let num = x.numer().to_isize()?;
                    let den = x.denom().to_isize()?;
                    let check = |x: isize| {
                        format!("{}", x).len() < 3
                    };
                    if check(num) && check(den) {
                        Some(format!("{}", x))
                    } else {
                        let x = x.to_f32()?;
                        Some(format!("{}", x))
                    }
                }
                run_basic(x.clone()).unwrap_or_else(|| {
                    let x = x.to_f64().unwrap();
                    format!("{:e}", x)
                })
            }
            Expr::Sym(Symbol::Con(x)) => x.clone(),
            Expr::Sym(Symbol::Var(x)) => x.clone(),
            Expr::Fraction(x) if x.is_sym() => {
                let name = x.unpack_sym_id().unwrap();
                format!("{}⁻¹", name)
            }
            Expr::Fraction(bot) => {
                format!("1/({})", bot.to_string())
            }
            Expr::Product(xs) => {
                xs
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(" * ")
            }
            Expr::Call(call) => {
                let pos_args = call.pos_args
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>();
                let key_args = call.key_args
                    .iter()
                    .map(|(key, x)| {
                        let x = x.to_string();
                        format!("{} = {}", key, x)
                    })
                    .collect::<Vec<_>>();
                let args = vec![pos_args, key_args].concat().join(",");
                format!(
                    "{}({})",
                    call.name,
                    args,
                )
            }
        }
    }
    pub fn eval(self) -> Self {
        let mut done = false;
        let mut state = self;
        fn cycle(inpt: Expr) -> Expr {
            inpt.simplify()
                .trans(Rc::new(crate::ast::funs::apply))
                .expand_constants()
                .simplify()
        }
        while !done {
            let latest = cycle(state.clone());
            if latest == state {
                done = true;
            }
            state = latest;
        }
        state
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Num(x) => {
                let zero = BigInt::from_f64(0.0).unwrap();
                let one = BigInt::from_f64(1.0).unwrap();
                let check = |x: &BigInt, max_len| {
                   format!("{}", x).len() > max_len && &x.abs() == x
                };
                if check(x.numer(), 2) || check(x.denom(), 3) {
                    write!(f, "Expr::Num({})", x.to_f64().unwrap())
                }
                else if x.numer() == &zero {
                    write!(f, "Expr::Num(0)")
                }
                else if x.numer() == &one && x.denom() == &one {
                    write!(f, "Expr::Num(1)")
                }
                else {
                    write!(f, "Expr::Num({}, {})", x.numer(), x.denom())
                }
            }
            Expr::Sym(Symbol::Con(x)) => {
                write!(f, "Expr::Con({:?})", x)
            }
            Expr::Sym(Symbol::Var(x)) => {
                write!(f, "Expr::Var({:?})", x)
            }
            Expr::Fraction(x) => {
                write!(f, "Expr::Fraction({:?})", x)
            }
            Expr::Product(xs) => {
                write!(f, "Expr::Product({:?})", xs)
            }
            Expr::Call(fun_call) => {
                let mut args = Vec::<String>::new();
                for arg in fun_call.pos_args.iter() {
                    args.push(format!("{:?}", arg))
                }
                for (key, arg) in fun_call.key_args.iter() {
                    args.push(format!("[{:?}, {:?}]", key, arg))
                }
                write!(f, "Expr::Call({}, {:?})", fun_call.name, args.join(","))
            }
        }
    }
}


///////////////////////////////////////////////////////////////////////////////
// DEV
///////////////////////////////////////////////////////////////////////////////

pub fn dev() {
    let run = |desc: &str, source: &str| {
        let result = Expr::from_str(source)
            .unwrap()
            .eval();
        println!("RUN: {}", desc);
        println!("{:?}\n", result);
    };
    // let expr = Expr::from_str("mole(energy(photon(wavelength = nm(325))))").unwrap();
    // let result = expr.clone().eval();
    // println!("{:#?}", result.to_string());
    // let expr = Expr::from_str("energy(photon(frequency = GHz(275)))").unwrap();
    // let result = expr.clone().eval();
    // println!("{:#?}", result.to_string());
    // let expr = Expr::from_str("wavelength(frequency = MHz(72.5))").unwrap();
    // let result = expr.clone().eval();
    // println!("{:#?}", result.to_string());
    // let expr = Expr::from_str("energy(from=electron(n=3), to=electron(n=4))").unwrap();
    // let result = expr.clone().eval();
    // println!("{:#?}", result.to_string());
    run(
        "energy of one mole of photons given wavelength",
        "mole(energy(photon(wavelength = nm(325))))"
    );
    run(
        "energy of photon given wavelength",
        "energy(photon(wavelength = nm(325)))"
    );
    run(
        "energy of photon given frequency",
        "energy(photon(frequency = GHz(275)))"
    );
    run(
        "wavelength from frequency",
        "wavelength(frequency = MHz(72.5))"
    );
    run(
        "frequency from wavelength",
        "frequency(wavelength = nm(325))"
    );
}


pub fn main() {
    let matrix: Matrix = matrix!{
        Expr::int(8), Expr::int(2), Expr::int(2);
        Expr::int(9), Expr::int(1), Expr::int(1);
    };
    println!("{}", matrix);
}

///////////////////////////////////////////////////////////////////////////////
// TESTS
///////////////////////////////////////////////////////////////////////////////


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basics() {
        let run = |source: &str, expected: Expr| {
            Expr::from_str(source).unwrap().eval()
        };
        run(
            "period(frequency = f)",
            Expr::unit_fraction(Expr::con("f"))
        );
    }
}

