use crate::lexer;
use std::io;

pub fn start<R: io::BufRead, W: io::Write>(mut reader: R, mut writer: W) -> io::Result<()> {
    println!("Hello! This is the Monkey programming language!\nFeel free to type in commands");
    loop {
        let _ = writer.write(b">> ");
        writer.flush()?;
        let mut line = String::new();
        reader.read_line(&mut line)?;
        for token in lexer::Lexer::new(line.as_str()) {
            println!("{token:?}");
        }
    }
}
