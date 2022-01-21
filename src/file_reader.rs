use std::fs;

pub fn try_read(file_name: &str) -> String {
	fs::read_to_string(file_name)
		.expect(&format!("Couldn't read file '{}'", file_name))
}