mod interpreter;

use std::io;
use interpreter::*;

fn main() {
	let mut interpreter = Interpreter::new();
	let mut code = String::new();	

	loop  {
		code.clear();
		io::stdin().read_line(&mut code).unwrap();
		if let Err(err) = interpreter.run(&code) {
			println!("{}", err);
		}
	}
}
