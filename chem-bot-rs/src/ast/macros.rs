//! Common macros.


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

