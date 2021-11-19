mod parser;

#[cfg(test)]
mod tests;

use std::io;
use parser::*;

fn main() {
	let mut code = String::new();	
	io::stdin().read_line(&mut code).unwrap();
	
	let interpreter = Interpreter::new(&code);
	
	match interpreter.process() {
		Ok(()) => {},
		Err(err) => println!("{}", err),
	}
}

struct Interpreter<'code> {
	code: &'code str
}

impl<'code> Interpreter<'code> {
	fn new(code: &'code str) -> Self {
		Self { code }
	}
	
	fn process(&self) -> Result<(), parser::TokConstructErr> {
		let parser = Parser::new(&self.code)?;
		
		for token in parser.tokens() {
			println!("{:?}", token);
		}
		println!("Success!");
		
		Ok(())
	}
}
