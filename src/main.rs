mod interpreter;
mod console_reader;

use interpreter::*;
use console_reader::ConsoleReader;

fn main() {
	let mut interpreter = Interpreter::new();
	let mut input = ConsoleReader::new();

	loop  {
		if let Err(err) = interpreter.run(input.next_line()) {
			println!("{}", err);
		}
	}
}
