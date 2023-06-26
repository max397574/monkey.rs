use crate::{lexer, parser::Parser};
use std::io;

pub fn start<R: io::BufRead, W: io::Write>(mut reader: R, mut writer: W) -> io::Result<()> {
    println!("Hello! This is the Monkey programming language!\nFeel free to type in commands");
    loop {
        let _ = writer.write(b">> ");
        writer.flush()?;
        let mut line = String::new();
        reader.read_line(&mut line)?;
        let mut parser = Parser::new(lexer::Lexer::new(line.as_str()));
        let program = parser.parse_program();
        match program {
            Ok(prog) => {
                for statement in prog.statements {
                    // println!("{statement}");
                    println!("{statement:?}");
                }
            }
            Err(errs) => {
                for err in errs {
                    println!("{err}");
                }
            }
        }
        // for token in lexer::Lexer::new(line.as_str()) {
        //     println!("{token:?}");
        // }
    }
}
