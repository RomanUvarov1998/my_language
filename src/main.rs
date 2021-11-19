mod parser;
mod lexer;
mod expr;
mod interpreter;

#[cfg(test)]
mod tests;

use std::io;
use interpreter::*;

fn main() {
	let mut code = String::new();	
	io::stdin().read_line(&mut code).unwrap();
	
	let interpreter = Interpreter::new(&code);
	
	match interpreter.process() {
		Ok(()) => {},
		Err(err) => println!("{}", err),
	}
}
