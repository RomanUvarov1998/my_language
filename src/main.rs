mod parser;

use std::io;
use parser::*;

fn main() {
	let mut code = String::new();	
	io::stdin().read_line(&mut code).unwrap();
	
	let parser = Parser::new(&code).unwrap();	
	
	for token in parser.tokens() {
		println!("{:?}", token);
	}
}
