use std::fs;

pub fn try_read(file_name: &str) -> String {
	let mut path = std::env::current_dir().unwrap();
	path.push(file_name);
	fs::read_to_string(path)
		.expect(&format!("Couldn't read file '{}'", file_name))
}