use std::iter::Peekable;
use std::str::Chars;

pub struct CharsIter<'code> {
	ch_iter: Peekable<Chars<'code>>,
	pos: usize,
}

const RADIX: u32 = 10_u32;

impl<'code> CharsIter<'code> {
	pub fn new(text: &'code str) -> Self {
		Self {
			ch_iter: text.chars().peekable(),
			pos: 0_usize,
		}
	}
	
	pub fn peek(&mut self) -> Option<(CharKind, usize)> {
		let ch = self.ch_iter.peek()?;
		Some( ( CharKind::from(*ch), self.pos ) )
	}
	
	pub fn last_pos(&self) -> usize {
		self.pos
	}
}

impl Iterator for CharsIter<'_> {		
	type Item = (CharKind, usize);
	
	fn next(&mut self) -> Option<Self::Item> {
		let ch: char = self.ch_iter.next()?;
		
		let char_kind = CharKind::from(ch);
		
		let res = Some( ( char_kind, self.pos ) );
		
		self.pos += 1;
		
		res
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

//#[test]
mod tests {
	use std::iter::Peekable;
	use std::str::Chars;
	use std::collections::VecDeque;
	use super::CharKind;
	
	struct ChIt {
		strings_queue: VecDeque<String>,
		chars_queue: VecDeque<char>,
	}
	
	impl ChIt {
		fn new() -> Self {
			Self {
				strings_queue: VecDeque::<String>::new(),
				chars_queue: VecDeque::<char>::new(),
			}
		}
		
		fn push_string(&mut self, string: String) {
			self.strings_queue.push_back(string);
		}
	}
	
	impl Iterator for ChIt {
		type Item = char;
		
		fn next(&mut self) -> Option<Self::Item> {			
			while self.strings_queue.len() > 0 && self.chars_queue.is_empty() {
				let cur_string = self.strings_queue.pop_front().unwrap();
				
				println!("start '{}'", cur_string);
				
				for ch in cur_string.chars() {
					self.chars_queue.push_back(ch);
				}
			}
			
			self.chars_queue.pop_front()
		}
	}
	
	#[test]
	fn try_ch_it() {
		let mut ch_it = ChIt::new();
		use std::collections::VecDeque;
		use std::iter::FromIterator;
		
		assert_eq!(ch_it.next(), None);
		
		// try 1 strings
		ch_it.push_string("abc".to_string());
		assert_eq!(ch_it.strings_queue, VecDeque::from(vec![
			"abc".to_string(),
		]));
		
		assert_eq!(ch_it.next(), Some('a'));
		assert_eq!(ch_it.next(), Some('b'));
		assert_eq!(ch_it.next(), Some('c'));
		assert_eq!(ch_it.next(), None);
		assert_eq!(ch_it.strings_queue, VecDeque::new());
		
		// try 2 strings
		ch_it.push_string("def".to_string());
		ch_it.push_string("ghi".to_string());
		assert_eq!(ch_it.strings_queue, VecDeque::from(vec![
			"def".to_string(),
			"ghi".to_string(),
		]));
		
		assert_eq!(ch_it.next(), Some('d'));
		assert_eq!(ch_it.next(), Some('e'));
		assert_eq!(ch_it.next(), Some('f'));
		assert_eq!(ch_it.next(), Some('g'));
		assert_eq!(ch_it.next(), Some('h'));
		assert_eq!(ch_it.next(), Some('i'));
		assert_eq!(ch_it.next(), None);		
		assert_eq!(ch_it.strings_queue, VecDeque::new());
		
		// try empty string
		ch_it.push_string("".to_string());
		assert_eq!(ch_it.next(), None);		
		assert_eq!(ch_it.strings_queue, VecDeque::new());
	}
}