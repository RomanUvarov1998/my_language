mod interpreter;
mod console_reader;
mod file_reader;

use interpreter::*;
use console_reader::ConsoleReader;

fn main() {	
	let mut interpreter = Interpreter::new();
	
	let args: Vec<String> = std::env::args().collect();
	
	match args.len() {
		0 => unreachable!(),
		1 => {
			let mut input = ConsoleReader::new();
			loop  {
				match interpreter.check_and_run(input.next_line()) {
					Ok(msg) => match msg {
						InterpInnerSignal::CanContinue => {},
						InterpInnerSignal::Exit => break,
					},
					Err(err) => println!("{}", err),
				}
			}
		},
		2 => {
			let file_name: &str = &args[1];
			let code: String = file_reader::try_read(file_name);
			match interpreter.check_and_run(&code) {
				Ok(_) => {},
				Err(err) => {
					println!("ERROR: {}:{} - {}:{}", 
						err.pos_begin().line() + 1, err.pos_begin().col(),
						err.pos_end().line() + 1, err.pos_end().col());
			
					let code_lines: Vec<&str> = code.lines().collect();
					
					for line_num in err.pos_begin().line()..=err.pos_end().line() {
						println!("{}", code_lines[line_num]);
					}
					
					println!("{}", err);
				},
			}
		}
		_ => print_usage(),
	}
}

fn print_usage() {
	println!("program file_with_code.txt");
}
