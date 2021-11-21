mod interpreter;

use std::io;
use interpreter::*;

fn main() {
	let mut interpreter = Interpreter::new();
	let mut code = String::new();	

	loop  {
		code.clear();
		io::stdin().read_line(&mut code).unwrap();
		interpreter.run(&code).unwrap();
	}
}
