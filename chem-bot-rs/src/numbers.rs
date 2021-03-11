use num::{FromPrimitive, ToPrimitive, BigRational, BigInt, Signed};


/// Rational number type for exact answers with no roundoff error.
#[derive(Clone, PartialEq, PartialOrd)]
pub struct Number(BigRational);

impl Number {
    /// Constructs a `Number` with the given numerator and denominator.
    pub fn fraction(
        numerator: impl Into<i128>,
        denominator: impl Into<i128>,
    ) -> Self {
        let numerator = BigInt::from_i128(numerator.into()).unwrap();
        let denominator = BigInt::from_i128(denominator.into()).unwrap();
        Number(BigRational::new(numerator, denominator))
    }
    /// Constructs a `Number` form an i128 value.
    pub fn int(x: impl Into<i128>) -> Self {
        Number(BigRational::from_i128(x.into()).unwrap())
    }
    /// Constructs a `Number` form an f64 value.
    pub fn double(x: impl Into<f64>) -> Self {
        Number(BigRational::from_f64(x.into()).unwrap())
    }
    pub fn reciprocal(self) -> Self {
        let n = self.0.numer().clone();
        let d = self.0.denom().clone();
        Number(BigRational::new(d, n))
    }
    pub fn abs(&self) -> Self {Number(self.0.abs())}
    pub fn sign(&self) -> isize {
        use num::bigint::Sign;
        let n = self.0.numer().sign();
        let d = self.0.denom().sign();
        let x = n * d;
        match x {
            Sign::Minus => -1,
            Sign::Plus => 1,
            Sign::NoSign => 1,
        }
    }
    pub fn numerator(&self) -> Option<isize> {
        self.0.numer().to_isize()
    }
    pub fn denominator(&self) -> Option<isize> {
        self.0.denom().to_isize()
    }
    pub fn unpack_integer(&self) -> Option<isize> {
        if self.denominator()? == 1 {
            self.numerator()
        } else {
            None
        }
    }
}

impl std::ops::Add for Number {
    type Output = Self;
    fn add(self, other: Self) -> Self {Number(self.0 + other.0)}
}
impl std::ops::Sub for Number {
    type Output = Self;
    fn sub(self, other: Self) -> Self {Number(self.0 - other.0)}
}
impl std::ops::Div for Number {
    type Output = Self;
    fn div(self, other: Self) -> Self {Number(self.0 / other.0)}
}
impl std::ops::Mul for Number {
    type Output = Self;
    fn mul(self, other: Self) -> Self {Number(self.0 * other.0)}
}
impl std::ops::Neg for Number {
    type Output = Self;
    fn neg(self) -> Self {Number(
        self.0 * BigRational::from_f32(-1.0).unwrap()
    )}
}
impl std::ops::Add for &Number {
    type Output = Number;
    fn add(self, other: Self) -> Number {Number(self.0.clone() + other.0.clone())}
}
impl std::ops::Sub for &Number {
    type Output = Number;
    fn sub(self, other: Self) -> Number {Number(self.0.clone() - other.0.clone())}
}
impl std::ops::Div for &Number {
    type Output = Number;
    fn div(self, other: Self) -> Number {Number(self.0.clone() / other.0.clone())}
}
impl std::ops::Mul for &Number {
    type Output = Number;
    fn mul(self, other: Self) -> Number {Number(self.0.clone() * other.0.clone())}
}
impl std::ops::Neg for &Number {
    type Output = Number;
    fn neg(self) -> Number {
        let new_number = self.0.clone() * BigRational::from_f32(-1.0).unwrap();
        Number(new_number)
    }
}
impl std::ops::Add for &mut Number {
    type Output = Number;
    fn add(self, other: Self) -> Number {Number(self.0.clone() + other.0.clone())}
}
impl std::ops::Sub for &mut Number {
    type Output = Number;
    fn sub(self, other: Self) -> Number {Number(self.0.clone() - other.0.clone())}
}
impl std::ops::Div for &mut Number {
    type Output = Number;
    fn div(self, other: Self) -> Number {Number(self.0.clone() / other.0.clone())}
}
impl std::ops::Mul for &mut Number {
    type Output = Number;
    fn mul(self, other: Self) -> Number {Number(self.0.clone() * other.0.clone())}
}

impl std::fmt::Debug for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x = &self.0;
        let zero = BigInt::from_f64(0.0).unwrap();
        let one = BigInt::from_f64(1.0).unwrap();
        let check = |x: &BigInt, max_len| {
            format!("{}", x).len() > max_len && &x.abs() == x
        };
        if check(x.numer(), 2) || check(x.denom(), 3) {
            write!(f, "Number({})", x.to_f64().unwrap())
        }
        else if x.numer() == &zero {
            write!(f, "Number(0)")
        }
        else if x.numer() == &one && x.denom() == &one {
            write!(f, "Number(1)")
        }
        else {
            write!(f, "Number({}, {})", x.numer(), x.denom())
        }
    }
}
impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x = &self.0;
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
        let string = run_basic(x.clone()).unwrap_or_else(|| {
            let x = x.to_f64().unwrap();
            format!("{:e}", x)
        });
        write!(f, "{}", string)
    }
}

