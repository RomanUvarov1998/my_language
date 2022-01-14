mod interpreter;
mod console_reader;

use interpreter::*;
use console_reader::ConsoleReader;

fn main() {
	let mut interpreter = Interpreter::new();
	let mut input = ConsoleReader::new();

	loop  {
		match interpreter.run(input.next_line()) {
			Ok(msg) => match msg {
				InterpInnerSignal::CanContinue => {},
				InterpInnerSignal::Exit => break,
			},
			Err(err) => println!("{}", err),
		}
	}
}
