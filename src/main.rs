mod interpreter;
mod file_reader;

use interpreter::*;

fn main() {	
	let mut interpreter = Interpreter::new();
	
	let args: Vec<String> = std::env::args().collect();
	
	match args.len() {
		2 => {
			let file_name: &str = &args[1];
			let code: String = file_reader::try_read(file_name);
			match interpreter.check_and_run(&code) {
				Ok(_) => {},
				Err(err) => {
					if *err.inner() == InnerErr::HaltRequest {
						return;
					}
					
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
