use std::collections::VecDeque;

#[derive(Debug)]
pub struct CharsIter {
	strings_queue: VecDeque<String>,
	chars_queue: VecDeque<CharKind>,
	peeked_char: Option<(CharKind, usize)>,
	pos: usize,
}

const RADIX: u32 = 10_u32;

impl CharsIter {
	pub fn new() -> Self {
		Self {
			strings_queue: VecDeque::<String>::new(),
			chars_queue: VecDeque::<CharKind>::new(),
			peeked_char: None,
			pos: 0_usize,
		}
	}
		
	pub fn push_string(&mut self, string: String) {
		self.strings_queue.push_back(string);
	}
	
	pub fn peek(&mut self) -> Option<(CharKind, usize)> {
		if let None = self.peeked_char {
			self.peeked_char = self.next();
		}
		self.peeked_char
	}
	
	pub fn last_pos(&self) -> usize {
		self.pos
	}
}

impl Iterator for CharsIter {		
	type Item = (CharKind, usize);
	
	fn next(&mut self) -> Option<Self::Item> {
		if self.peeked_char.is_some() {
			return self.peeked_char.take();
		}
		
		while self.strings_queue.len() > 0 && self.chars_queue.is_empty() {
			let cur_string = self.strings_queue.pop_front().unwrap();
			
			for ch in cur_string.chars() {
				self.chars_queue.push_back(CharKind::from(ch));
			}
		}
			
		let res = (self.chars_queue.pop_front()?, self.pos);
		self.pos += 1;
		Some(res)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CharKind {
	Digit (u32, char),
	Dog,
	Dot,
	Plus,
	Minus,
	Asterisk,
	Circumflex,
	LeftSlash,
	LeftBracket,
	RightBracket,
	Eq,
	Letter (char),
	Punctuation (Punctuation),
	Whitespace,
	Control,
	Invalid (char),
}

impl From<char> for CharKind {
	fn from(ch: char) -> Self {
		match ch {
			'@' => CharKind::Dog,
			'.' => CharKind::Dot,
			'+' => CharKind::Plus,
			'-' => CharKind::Minus,
			'*' => CharKind::Asterisk,
			'^' => CharKind::Circumflex,
			'/' => CharKind::LeftSlash,
			'(' => CharKind::LeftBracket,
			')' => CharKind::RightBracket,
			'=' => CharKind::Eq,
			' ' | '\n' | '\t' => CharKind::Whitespace,
			':' => CharKind::Punctuation (Punctuation::Colon),
			';' => CharKind::Punctuation (Punctuation::Semicolon),
			',' => CharKind::Punctuation (Punctuation::Comma),
			_ => if ch.is_alphabetic() {
				CharKind::Letter (ch)
			} else if let Some(value) = ch.to_digit(RADIX) {
				CharKind::Digit(value, ch)
			} else if ch.is_ascii_control() {
				CharKind::Control
			} else {
				CharKind::Invalid (ch)
			}
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Punctuation {
	Colon,
	Semicolon,
	Comma
}

impl std::fmt::Display for CharKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			CharKind::Digit (_value, ch) => write!(f, "{}", ch),
			CharKind::Dot => write!(f, "."),
			CharKind::Dog => write!(f, "@"),
			CharKind::Plus => write!(f, "+"),
			CharKind::Minus => write!(f, "-"),
			CharKind::Asterisk => write!(f, "*"),
			CharKind::Circumflex => write!(f, "^"),
			CharKind::LeftSlash => write!(f, "/"),
			CharKind::LeftBracket => write!(f, "("),
			CharKind::RightBracket => write!(f, ")"),
			CharKind::Eq => write!(f, "="),
			CharKind::Letter (ch) => write!(f, "{}", ch),
			CharKind::Whitespace => write!(f, "Whitespace"),
			CharKind::Control => write!(f, "Control"),
			CharKind::Punctuation (p) => match p {
				Punctuation::Colon => write!(f, ":"),
				Punctuation::Semicolon => write!(f, ";"),
				Punctuation::Comma => write!(f, ","),
			},
			CharKind::Invalid (ch) => write!(f, "{}", ch),
		}
	}
}

#[cfg(test)]
mod tests {	
	use super::{CharsIter, CharKind};
	use std::collections::VecDeque;

	#[test]
	fn try_chars_iter() {
		let mut ch_it = CharsIter::new();
		
		assert_eq!(ch_it.next(), None);
		
		// try 1 strings
		ch_it.push_string("abc".to_string());
		assert_eq!(ch_it.strings_queue, VecDeque::from(vec![
			"abc".to_string(),
		]));
		
		assert_eq!(ch_it.next(), Some((CharKind::Letter('a'), 0_usize)));
		assert_eq!(ch_it.next(), Some((CharKind::Letter('b'), 1_usize)));
		assert_eq!(ch_it.next(), Some((CharKind::Letter('c'), 2_usize)));
		assert_eq!(ch_it.next(), None);
		assert_eq!(ch_it.strings_queue, VecDeque::new());
		
		// try 2 strings
		ch_it.push_string("def".to_string());
		ch_it.push_string("ghi".to_string());
		assert_eq!(ch_it.strings_queue, VecDeque::from(vec![
			"def".to_string(),
			"ghi".to_string(),
		]));
		
		assert_eq!(ch_it.next(), Some((CharKind::Letter('d'), 3_usize)));
		assert_eq!(ch_it.next(), Some((CharKind::Letter('e'), 4_usize)));
		assert_eq!(ch_it.next(), Some((CharKind::Letter('f'), 5_usize)));
		assert_eq!(ch_it.next(), Some((CharKind::Letter('g'), 6_usize)));
		assert_eq!(ch_it.next(), Some((CharKind::Letter('h'), 7_usize)));
		assert_eq!(ch_it.next(), Some((CharKind::Letter('i'), 8_usize)));
		assert_eq!(ch_it.next(), None);		
		assert_eq!(ch_it.strings_queue, VecDeque::new());
		
		// try empty string
		ch_it.push_string("".to_string());
		assert_eq!(ch_it.next(), None);		
		assert_eq!(ch_it.strings_queue, VecDeque::new());
	}
}