mod interpreter;
mod global_mem;
mod chars_iter;
mod tokens_iter;
mod statements_iter;
mod expr;

#[cfg(test)]
mod tests;

use std::io;
use interpreter::*;

fn main() {
	let mut interpreter = Interpreter::new();
	let mut code = String::new();	

	loop  {
		code.clear();
		io::stdin().read_line(&mut code).unwrap();
		interpreter.process(&code).unwrap();
	}
}
