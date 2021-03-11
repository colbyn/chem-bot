#![allow(unused)]

pub mod parser_utils;
pub mod ast;
pub mod matrix;
pub mod numbers;
pub mod chem;

fn main() {
    // ast::expr_parser::main();
    // ast::expr::main();
    // ast::funs::main();
    // matrix::main();
    chem::data::main();
}
