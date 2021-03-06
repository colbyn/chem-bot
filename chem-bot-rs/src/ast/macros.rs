//! Common macros.


#[macro_export]
macro_rules! return_fun_call {
    ($constr:expr, $expr:expr) => {{
        match $expr {
            Expr::Call(x) => x,
            x => return $constr(x),
        }
    }};
}

#[macro_export]
macro_rules! return_fun_call_arg0 {
    ($constr:expr, $expr:expr) => {{
        match $expr {
            Expr::Call(x) => {
                match x.pos_args.get(0) {
                    Some(x) => x.clone(),
                    _ => return $constr($expr)
                }
            }
            x => return $constr(x),
        }
    }};
}

