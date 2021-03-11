use std::ops::{Range, Bound, RangeBounds};
use std::rc::Rc;
use std::cell::RefCell;
use std::iter::FromIterator;
use std::path::{Path, PathBuf};
use std::convert::AsRef;
use std::collections::{HashMap, LinkedList, HashSet};
use num::{FromPrimitive, ToPrimitive, BigRational, BigInt, Signed};

use crate::numbers::Number;

///////////////////////////////////////////////////////////////////////////////
// TYPE HELPERS
///////////////////////////////////////////////////////////////////////////////

pub type Index = usize;

#[derive(Debug, Clone, PartialEq)]
pub struct IndexPosition {
    row_index: usize,
    col_index: usize,
}

///////////////////////////////////////////////////////////////////////////////
// FUNCTION HELPERS
///////////////////////////////////////////////////////////////////////////////



///////////////////////////////////////////////////////////////////////////////
// MATRIX DATA TYPES
///////////////////////////////////////////////////////////////////////////////

#[derive(Clone, PartialEq)]
pub struct Column(Vec<Number>);
#[derive(Debug, Clone, PartialEq)]
pub struct Row(Vec<Number>);

impl From<Column> for Vec<Number> {
    fn from(x: Column) -> Self {x.0}
}
impl From<Row> for Vec<Number> {
    fn from(x: Row) -> Self {x.0}
}

impl Row {
    pub fn max(&self) -> Number {
        let mut max_val = None::<Number>;
        for x in self.0.iter() {
            match max_val.clone() {
                Some(max) if x.clone() > max.clone() => {
                    max_val = Some(x.clone());
                }
                Some(_) => {}
                None => {
                    max_val = Some(x.clone());
                }
            }
        }
        max_val.unwrap()
    }
    pub fn map(self, f: impl Fn(Number) -> Number) -> Self {
        let xs = self.0
            .into_iter()
            .map(f)
            .collect();
        Row(xs)
    }
    pub fn add_each(self, value: &Number) -> Self {
        self.map(|x| &x + value)
    }
    pub fn div_each(self, value: &Number) -> Self {
        self.map(|x| &x / value)
    }
    pub fn mul_each(self, value: &Number) -> Self {
        self.map(|x| &x * value)
    }
    pub fn add(&self, other: &Row) -> Self {
        assert_eq!(self.0.len(), other.0.len());
        let xs = self.0
            .iter()
            .zip(other.0.iter())
            .map(|(l, r)| {
                l + r
            })
            .collect();
        Row(xs)
    }
}

/// A dynamic matrix in **row major order**.
#[derive(Clone, PartialEq)]
pub struct Matrix(Vec<Row>);

impl Matrix {
    pub fn new() -> Self {Matrix(Default::default())}
    pub fn len(&self) -> (usize, usize) {
        let mut row_size = self.0.len();
        let mut col_size = None;
        for row in self.0.iter() {
            match col_size {
                None => {
                    col_size = Some(row.0.len());
                }
                Some(ix) => {
                    assert_eq!(ix, row.0.len());
                }
            }
        }
        assert!(col_size.is_some());
        (row_size, col_size.unwrap())
    }
    pub fn row_len(&self) -> usize {
        self.len().0
    }
    pub fn col_len(&self) -> usize {
        self.len().1
    }
    pub fn unpack_singleton(&self) -> Option<Number> {
        match &self.0[..] {
            [r] => {
                match &r.0[..] {
                    [x] => Some(x.clone()),
                    _ => None
                }
            }
            _ => None
        }
    }
    pub fn unpack_column_vector(&self) -> Option<Column> {
        let mut column = Vec::new();
        for r in self.0.iter() {
            match &r.0[..] {
                [c] => {
                    column.push(c.clone());
                }
                _ => {
                    return None
                }
            }
        }
        Some(Column(column))
    }
    fn validate(&self) -> bool {
        let mut column_size = None;
        let mut all_valid = true;
        for row in self.0.iter() {
            match column_size {
                None => {
                    column_size = Some(row.0.len());
                }
                Some(ix) => {
                    all_valid = row.0.len() == ix;
                }
            }
        }
        all_valid
    }
    pub fn from_rows(rows: Vec<Vec<Number>>) -> Option<Self> {
        let matrix = Matrix(
            rows.into_iter()
                .map(|xs| Row(xs))
                .collect::<Vec<_>>()
        );
        if matrix.validate() {
            Some(matrix)
        } else {
            None
        }
    }
    pub fn column_vector(cols: impl Into<Vec<Number>>) -> Option<Self> {
        let cols = cols.into();
        if cols.is_empty() {
            return None
        }
        let matrix = Matrix::from_rows(vec![cols])
            .unwrap()
            .transpose();
        assert!(matrix.validate());
        Some(matrix)
    }
    pub fn transpose(self) -> Matrix {
        let row_len = self.row_len();
        let col_len = self.col_len();
        let mut new_matrix = Matrix::new();
        for c in 0..col_len {
            let column = self.get_column(c).unwrap().0;
            new_matrix.push_row(column);
        }
        new_matrix
    }
    pub fn push_row<T: Into<Vec<Number>>>(&mut self, row: T) {
        self.0.push(Row(row.into()));
    }
    pub fn push_column<T: Into<Vec<Number>>>(
        &mut self,
        column: T,
        on_empty: fn() -> Number,
    ) {
        let mut column = column.into();
        for row in self.0.iter_mut() {
            let node;
            if column.len() > 1 {
                node = column.remove(0);
            } else {
                node = on_empty();
            }
            // let node = column
            //     .remove(0);
            //     .unwrap_or_else(on_empty);
            row.0.push(node);
        }
    }
    pub fn to_string(&self) -> String {
        let mut rows = Vec::<String>::new();
        let row_len = self.row_len();
        let mut max_column_len = 0;
        for row in self.0.iter() {
            let mut column = Vec::<String>::new();
            for expr in row.0.iter() {
                column.push(format!(" {}", expr.to_string()));
            }
            let column = column.join(" ");
            if column.len() > max_column_len {
                max_column_len = column.len();
            }
            rows.push(column);
        }
        for (ix, row) in rows.iter_mut().enumerate() {
            let added_len = max_column_len - row.len();
            let spaces = (0..=added_len).map(|_| " ").collect::<String>();
            let row_as_spaces = (0..row.len())
                .into_iter()
                .map(|_| ' ')
                .collect::<String>();
            // ┐┘
            match ix {
                0 => {
                    *row = format!("  ┌{rowp}{ws}┐\n  │{row}{ws}│", row=row, ws=spaces, rowp=row_as_spaces);
                }
                _ if ix == row_len - 1 => {
                    *row = format!("  │{row}{ws}│\n  └{rowp}{ws}┘", row=row, ws=spaces, rowp=row_as_spaces);
                }
                _ => {
                    *row = format!("  │{}{}│", row, spaces);
                }
            }
        }
        rows.join("\n")
    }
    pub fn set(&mut self, ix: (usize, usize), value: Number) {
        let mut is_set = false;
        for (mut i, row) in self.0.iter_mut().enumerate() {
            if i == ix.0 {
                for (j, column) in row.0.iter_mut().enumerate() {
                    if j == ix.1 {
                        *column = value.clone();
                        is_set = true;
                    }
                }
            }
        }
        assert_eq!(is_set, true);
    }
    pub fn map_range(
        &mut self,
        row_range: impl RangeBounds<usize>,
        col_range: impl RangeBounds<usize>,
        f: impl Fn(IndexPosition, Number) -> Number,
    )
    {
        let mut did_set = false;
        for (r_ix, r) in self.0.iter_mut().enumerate() {
            for (c_ix, c) in r.0.iter_mut().enumerate() {
                if row_range.contains(&r_ix) && col_range.contains(&c_ix) {
                    let pos = IndexPosition {
                        row_index: r_ix,
                        col_index: c_ix,
                    };
                    *c = f(pos, c.clone());
                    did_set = true;
                }
            }
        }
        assert_eq!(did_set, true);
    }
    pub fn new_from(
        &mut self,
        row_range: impl RangeBounds<usize>,
        col_range: impl RangeBounds<usize>,
    ) -> Matrix {
        let mut did_set = false;
        let mut rows = Vec::<Row>::new();
        for (r_ix, r) in self.0.iter_mut().enumerate() {
            let mut row = Vec::<Number>::new();
            for (c_ix, c) in r.0.iter_mut().enumerate() {
                if row_range.contains(&r_ix) && col_range.contains(&c_ix) {
                    did_set = true;
                    row.push(c.clone());
                }
            }
            if !row.is_empty() {
                rows.push(Row(row));
            }
        }
        let new_matrix = Matrix(rows);
        assert!(new_matrix.validate());
        assert!(did_set);
        new_matrix
    }
    pub fn map_row(&mut self, row_ix: usize, f: impl Fn(IndexPosition, Number) -> Number) {
        self.map_range(
            row_ix..=row_ix,
            ..,
            f,
        );
    }
    pub fn map_col(&mut self, col_ix: usize, f: impl Fn(IndexPosition, Number) -> Number) {
        self.map_range(
            ..,
            col_ix..=col_ix,
            f,
        );
    }
    pub fn replace_row(&mut self, row_ix: usize, new_row: Row) {
        let mut did_set = false;
        for (ix, row) in self.0.iter_mut().enumerate() {
            if ix == row_ix {
                *row = new_row.clone();
                did_set = true;
            }
        }
        assert_eq!(did_set, true);
    }
    pub fn mul_row(&mut self, row_ix: usize, mult: &Number) {
        self.map_row(row_ix, move |_, x| x * mult.clone())
    }
    pub fn get(&self, ix: (usize, usize)) -> Option<&Number> {
        let mut is_set = false;
        for (i, row) in self.0.iter().enumerate() {
            if i == ix.0 {
                for (j, column) in row.0.iter().enumerate() {
                    if j == ix.1 {
                        return Some(column)
                    }
                }
            }
        }
        None
    }
    pub fn get_row(&self, ix: usize) -> Option<&Row> {
        let mut is_set = false;
        for (i, row) in self.0.iter().enumerate() {
            if i == ix {
                return Some(row)
            }
        }
        None
    }
    pub fn get_column(&self, ix: usize) -> Option<Column> {
        let mut return_column = Vec::new();
        for (r_ix, r) in self.0.iter().enumerate() {
            for (c_ix, c) in r.0.iter().enumerate() {
                if c_ix == ix {
                    return_column.push(c.clone());
                }
            }
        }
        if !return_column.is_empty() {
            Some(Column(return_column))
        } else {
            None
        }
    }
    pub fn unsafe_get(&self, ix: (usize, usize)) -> &Number {
        self.get(ix).unwrap()
    }
    pub fn unsafe_get_row(&self, ix: usize) -> &Row {
        self.get_row(ix).unwrap()
    }
    pub fn forward_elimination(&mut self) {
        let row_len = self.row_len();
        let col_len = self.col_len();
        for i in 0..(col_len) {
            for j in (i + 1)..row_len {
                let new_row = (|| {
                    let i_mult = self.unsafe_get((j, i)) / self.unsafe_get((i, i));
                    let i_mult = -i_mult;
                    let j_row = self
                        .unsafe_get_row(j);
                    let i_row = self
                        .unsafe_get_row(i)
                        .clone()
                        .mul_each(&i_mult)
                        .add(j_row);
                    i_row
                })();
                self.replace_row(j, new_row);
            }
        }
    }
    pub fn dot(&self, rhs: &Matrix) -> Option<Matrix> {
        let mut new_rows = Vec::new();
        for l in 0..self.row_len() {
            let mut new_row = Vec::new();
            for r in 0..rhs.col_len() {
                let row = self.get_row(l).unwrap();
                let column = rhs.get_column(r).unwrap();
                if row.0.len() != column.0.len() {
                    return None
                }
                let result = row.0
                    .iter()
                    .zip(column.0.iter())
                    .map(|(l, r)| l * r)
                    .fold(Number::int(0), |l, r| l + r);
                new_row.push(result);
            }
            new_rows.push(new_row);
        }
        Some(Matrix::from_rows(new_rows).unwrap())
    }
    pub fn rref(&self) -> Column {
        let mut this = self.clone();
        let row_len = this.row_len();
        let col_len = this.col_len();
        this.forward_elimination();
        // Solution Vector
        let mut solution = (0..(col_len-1))
            .into_iter()
            .map(|_| Number::int(0))
            .collect::<Vec<_>>();
        let mut solution = Matrix::column_vector(solution).unwrap();
        for i in (0..(row_len)).rev() {
            let l = this.get((i, col_len - 1)).unwrap().clone();
            let r = this.new_from(
                i..=i,
                0..(col_len - 1),
            );
            let r = r
                .dot(&solution)
                .unwrap()
                .unpack_singleton()
                .unwrap();
            let current = (l - r) / this.get((i, i)).unwrap().clone();
            solution.set((i, 0), current);
        }
        solution.unpack_column_vector().unwrap()
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


impl std::fmt::Display for Column {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let this = Matrix::column_vector(self.clone().0).unwrap();
        write!(f, "{}", this)
    }
}
impl std::fmt::Debug for Column {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let this = Matrix::column_vector(self.clone().0).unwrap();
        this.fmt(f)
    }
}


///////////////////////////////////////////////////////////////////////////////
// MACRO HELPERS
///////////////////////////////////////////////////////////////////////////////


// $(,)?
/// Internal helper.
#[macro_export]
macro_rules! matrix_row {
    ($row:expr;; $($entry:expr)*) => {
        $(
            $row.push($entry);
        )*
    };
}

// $(,)?
/// Internal helper.
#[macro_export]
macro_rules! matrix_rows {
    ($rows:expr;; $(
        $($entry:expr),*
    );*) => {
        $({
            let mut row: Vec<Number> = Vec::new();
            matrix_row!(row;; $($entry)*);
            if !row.is_empty() {
                $rows.push(row);
            }
        })*
    };
}

#[macro_export]
macro_rules! matrix {
    ($($x:tt)*) => {{
        let mut rows: Vec<Vec<Number>> = Vec::new();
        matrix_rows!(rows;; $($x)*);
        Matrix::from_rows(rows).unwrap()
    }};
}


///////////////////////////////////////////////////////////////////////////////
// DEV
///////////////////////////////////////////////////////////////////////////////

pub fn main() {
    let mut matrix: Matrix = matrix!{
        Number::int(1), Number::int(1), Number::int(1), Number::int(6);
        Number::int(0), Number::int(2), Number::int(5), Number::int(-4);
        Number::int(2), Number::int(5), Number::int(-1), Number::int(27);
    };
    let solution = matrix.rref();
    println!("{}", matrix);
    println!("{}", solution)
}