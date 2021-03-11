//! Common macros.


///////////////////////////////////////////////////////////////////////////////
// GENERAL
///////////////////////////////////////////////////////////////////////////////

#[macro_export]
macro_rules! return_fun_call {
    ($constr:expr, $expr:expr) => {{
        match $expr {
            Expr::Call(x) => x,
            _ => {
                return $constr
            }
        }
    }};
}

#[macro_export]
macro_rules! return_fun_call_arg0 {
    ($constr:expr, $expr:expr) => {{
        match $expr {
            Expr::Call(fun_call) => {
                match fun_call.pos_args.get(0) {
                    Some(x) => {
                        x.clone()
                    }
                    _ => {
                        return $constr
                    }
                }
            }
            _ => {
                return $constr
            }
        }
    }};
}

#[macro_export]
macro_rules! return_some {
    ($expr:expr) => {
        match $expr {
            Some(x) => {
                return x
            }
            None => {
            }
        }
    };
}

///////////////////////////////////////////////////////////////////////////////
// MATRIX
///////////////////////////////////////////////////////////////////////////////


// // $(,)?
// /// Internal helper.
// #[macro_export]
// macro_rules! matrix_row {
//     ($row:expr;; $($entry:expr)*) => {
//         $(
//             $row.push($entry);
//         )*
//     };
// }

// // $(,)?
// /// Internal helper.
// #[macro_export]
// macro_rules! matrix_rows {
//     ($rows:expr;; $(
//         $($entry:expr),*
//     );*) => {
//         $({
//             let mut row: Vec<Expr> = Vec::new();
//             matrix_row!(row;; $($entry)*);
//             if !row.is_empty() {
//                 $rows.push(row);
//             }
//         })*
//     };
// }

// #[macro_export]
// macro_rules! matrix {
//     ($($x:tt)*) => {{
//         let mut rows: Vec<Vec<Expr>> = Vec::new();
//         matrix_rows!(rows;; $($x)*);
//         Matrix::from_rows(rows).unwrap()
//     }};
// }

