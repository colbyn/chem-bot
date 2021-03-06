//! Common macros.

#[macro_export]
macro_rules! return_value {
    ($expr:expr) => {{
        match $expr {
            Expr::Value(x) => x,
            x => return x,
        }
    }};
}

#[macro_export]
macro_rules! return_value_num {
    ($expr:expr) => {{
        match $expr {
            Expr::Value(Value::Num(x)) => x,
            x => return x,
        }
    }};
}

#[macro_export]
macro_rules! return_fun_call {
    ($expr:expr) => {{
        match $expr {
            Expr::Call(x) => x,
            x => return x,
        }
    }};
}

#[macro_export]
macro_rules! return_fun_call_arg0 {
    ($expr:expr) => {{
        match $expr {
            Expr::Call(x) => {
                match x.pos_args.get(0) {
                    Some(x) => x.clone(),
                    _ => return $expr
                }
            }
            x => return x,
        }
    }};
}

