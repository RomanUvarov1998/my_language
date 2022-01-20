use std::collections::VecDeque;

//------------------------------ CharsIter ----------------------------

#[derive(Debug)]
pub struct CharsIter {
	strings_queue: VecDeque<String>,
	chars_queue: VecDeque<ParsedChar>,
	peeked_char: Option<ParsedChar>,
	pos: CharPos,
}

const RADIX: u32 = 10_u32;

impl CharsIter {
	pub fn new() -> Self {
		Self {
			strings_queue: VecDeque::<String>::new(),
			chars_queue: VecDeque::<ParsedChar>::new(),
			peeked_char: None,
			pos: CharPos::new(),
		}
	}
		
	pub fn push_string(&mut self, string: String) {
		self.strings_queue.push_back(string);
	}
	
	pub fn peek(&mut self) -> Option<ParsedChar> {
		if let None = self.peeked_char {
			self.peeked_char = self.next();
		}
		self.peeked_char
	}
	
	pub fn last_pos(&self) -> CharPos {
		self.pos
	}
}

impl Iterator for CharsIter {		
	type Item = ParsedChar;
	
	fn next(&mut self) -> Option<Self::Item> {
		if self.peeked_char.is_some() {
			return self.peeked_char.take();
		}
		
		while self.strings_queue.len() > 0 && self.chars_queue.is_empty() {
			let cur_string = self.strings_queue.pop_front().unwrap();
			
			for ch in cur_string.chars() {
				let ch_kind = CharKind::from(ch);
				self.chars_queue.push_back(ParsedChar::new(ch, ch_kind, self.pos));
				self.pos.advance(ch_kind);
			}
		}
			
		self.chars_queue.pop_front()
	}
}

//------------------------------ ParsedChar ----------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParsedChar {
	ch: char,
	kind: CharKind,
	pos: CharPos,
}

impl ParsedChar {
	pub fn new(ch: char, kind: CharKind, pos: CharPos) -> Self {
		Self { ch, kind, pos }
	}
	pub fn ch(&self) -> char { self.ch }
	pub fn kind(&self) -> CharKind { self.kind }
	pub fn pos(&self) -> CharPos { self.pos }
}

impl std::fmt::Display for ParsedChar {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self.kind() {
			CharKind::Digit (_value) => write!(f, "{}", self.ch()),
			CharKind::Dot => write!(f, "."),
			CharKind::Dog => write!(f, "@"),
			CharKind::Plus => write!(f, "+"),
			CharKind::Minus => write!(f, "-"),
			CharKind::Asterisk => write!(f, "*"),
			CharKind::Circumflex => write!(f, "^"),
			CharKind::DoubleQuote => write!(f, "\""),
			CharKind::LeftSlash => write!(f, "/"),
			CharKind::LeftBracket => write!(f, "("),
			CharKind::RightBracket => write!(f, ")"),
			CharKind::Eq => write!(f, "="),
			CharKind::Letter => write!(f, "{}", self.ch()),
			CharKind::Whitespace => write!(f, "Whitespace"),
			CharKind::NewLine => write!(f, "NewLine"),
			CharKind::Control => write!(f, "Control"),
			CharKind::Punctuation (p) => match p {
				Punctuation::Colon => write!(f, ":"),
				Punctuation::Semicolon => write!(f, ";"),
				Punctuation::Comma => write!(f, ","),
			},
			CharKind::Invalid => write!(f, "{}", self.ch()),
		}
	}
}

//------------------------------ CharPos ----------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CharPos {
	line: usize,
	col: usize,
}

impl CharPos {
	fn new() -> Self {
		Self {
			line: 0,
			col: 0,
		}
	}
	
	#[allow(dead_code)]
	pub fn line(&self) -> usize { self.line }
	pub fn col(&self) -> usize { self.col }
	
	fn advance(&mut self, ch_kind: CharKind) {
		match ch_kind {
			CharKind::NewLine => {
				self.col = 0;
				self.line += 1;
			},
			_ => {
				self.col += 1;
			},
		}
	}
}

//------------------------------ CharKind ----------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CharKind {
	Digit (u32),
	Dog,
	Dot,
	Plus,
	Minus,
	Asterisk,
	Circumflex,
	DoubleQuote,
	LeftSlash,
	LeftBracket,
	RightBracket,
	Eq,
	Letter,
	Punctuation (Punctuation),
	Whitespace,
	NewLine,
	Control,
	Invalid,
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
			'\"' => CharKind::DoubleQuote,
			'/' => CharKind::LeftSlash,
			'(' => CharKind::LeftBracket,
			')' => CharKind::RightBracket,
			'=' => CharKind::Eq,
			' ' | '\t' => CharKind::Whitespace,
			'\n' => CharKind::NewLine,
			':' => CharKind::Punctuation (Punctuation::Colon),
			';' => CharKind::Punctuation (Punctuation::Semicolon),
			',' => CharKind::Punctuation (Punctuation::Comma),
			_ => if ch.is_alphabetic() {
				CharKind::Letter
			} else if let Some(value) = ch.to_digit(RADIX) {
				CharKind::Digit(value)
			} else if ch.is_ascii_control() {
				CharKind::Control
			} else {
				CharKind::Invalid
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

#[cfg(test)]
mod tests {	
	use super::{CharsIter, CharKind, CharPos, ParsedChar};
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
		
		assert_eq!(ch_it.next(), Some( ParsedChar { ch: 'a', kind: CharKind::Letter, pos: CharPos {line: 0_usize, col: 0_usize } }));
		assert_eq!(ch_it.next(), Some( ParsedChar { ch: 'b', kind: CharKind::Letter, pos: CharPos {line: 0_usize, col: 1_usize } }));
		assert_eq!(ch_it.next(), Some( ParsedChar { ch: 'c', kind: CharKind::Letter, pos: CharPos {line: 0_usize, col: 2_usize } }));
		assert_eq!(ch_it.next(), None);
		assert_eq!(ch_it.strings_queue, VecDeque::new());
		
		// try 2 strings
		ch_it.push_string("def".to_string());
		ch_it.push_string("ghi".to_string());
		assert_eq!(ch_it.strings_queue, VecDeque::from(vec![
			"def".to_string(),
			"ghi".to_string(),
		]));
		
		assert_eq!(ch_it.next(), Some( ParsedChar { ch: 'd', kind: CharKind::Letter, pos: CharPos {line: 0_usize, col: 3_usize } }));
		assert_eq!(ch_it.next(), Some( ParsedChar { ch: 'e', kind: CharKind::Letter, pos: CharPos {line: 0_usize, col: 4_usize } }));
		assert_eq!(ch_it.next(), Some( ParsedChar { ch: 'f', kind: CharKind::Letter, pos: CharPos {line: 0_usize, col: 5_usize } }));
		assert_eq!(ch_it.next(), Some( ParsedChar { ch: 'g', kind: CharKind::Letter, pos: CharPos {line: 0_usize, col: 6_usize } }));
		assert_eq!(ch_it.next(), Some( ParsedChar { ch: 'h', kind: CharKind::Letter, pos: CharPos {line: 0_usize, col: 7_usize } }));
		assert_eq!(ch_it.next(), Some( ParsedChar { ch: 'i', kind: CharKind::Letter, pos: CharPos {line: 0_usize, col: 8_usize } }));
		assert_eq!(ch_it.next(), None);		
		assert_eq!(ch_it.strings_queue, VecDeque::new());
		
		// try empty string
		ch_it.push_string("".to_string());
		assert_eq!(ch_it.next(), None);		
		assert_eq!(ch_it.strings_queue, VecDeque::new());
	}
}