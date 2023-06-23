pub mod lexer;
pub mod repl;
mod ast;
mod parser;
use std::io;

fn main() {
    // let input = "=+(){},;";
    //     let input = "let five = 5;
    // let ten = 10;
    //
    // let add = fn(x, y) {
    //   x + y;
    // };
    //
    // if x == 3 {
    // }
    // if y != 2*1 {};
    //
    // let result = add(five, ten);
    // ";
    let _ = repl::start(io::stdin().lock(), io::stdout().lock());
}
