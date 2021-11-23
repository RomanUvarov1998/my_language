use std::io;

pub struct ConsoleReader {
	line: String,
}

impl ConsoleReader {
	pub fn new() -> Self {
		Self {
			line: String::new()
		}
	}
	
	pub fn next_line(&mut self) -> &str {
		self.line.clear();
		io::stdin().read_line(&mut self.line).unwrap();
		&self.line
	}
}
	