pub struct CharsIter<'code> {
	ch_iter: std::iter::Peekable<std::str::Chars<'code>>,
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
		Some( ( Self::get_kind(*ch), self.pos ) )
	}
	
	fn get_kind(ch: char) -> CharKind {
		match ch {
			'.' => CharKind::Dot,
			'+' => CharKind::Plus,
			'-' => CharKind::Minus,
			'*' => CharKind::Mul,
			'/' => CharKind::Div,
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
			} else {
				CharKind::Invalid (ch)
			}
		}
	}
}

impl Iterator for CharsIter<'_> {		
	type Item = (CharKind, usize);
	
	fn next(&mut self) -> Option<Self::Item> {
		let ch: char = self.ch_iter.next()?;
		
		let char_kind: CharKind = Self::get_kind(ch);
		
		let res = Some( ( char_kind, self.pos ) );
		
		self.pos += 1;
		
		res
	}
}

#[derive(Debug, Clone, Copy)]
pub enum CharKind {
	Digit (u32, char),
	Dot,
	Plus,
	Minus,
	Mul,
	Div,
	LeftBracket,
	RightBracket,
	Eq,
	Letter (char),
	Punctuation (Punctuation),
	Whitespace,
	Invalid (char),
}

#[derive(Debug, Clone, Copy)]
pub enum Punctuation {
	Colon,
	Semicolon,
	Comma
}

impl std::fmt::Display for CharKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			CharKind::Digit (_value, ch) => write!(f, "'{}'", ch),
			CharKind::Dot => write!(f, "'.'"),
			CharKind::Plus => write!(f, "'+'"),
			CharKind::Minus => write!(f, "'-'"),
			CharKind::Mul => write!(f, "'*'"),
			CharKind::Div => write!(f, "'/'"),
			CharKind::LeftBracket => write!(f, "'('"),
			CharKind::RightBracket => write!(f, "')'"),
			CharKind::Eq => write!(f, "'='"),
			CharKind::Letter (ch) => write!(f, "'{}'", ch),
			CharKind::Whitespace => write!(f, "Whitespace"),
			CharKind::Punctuation (p) => match p {
				Punctuation::Colon => write!(f, "':'"),
				Punctuation::Semicolon => write!(f, "';'"),
				Punctuation::Comma => write!(f, "','"),
			},
			CharKind::Invalid (ch) => write!(f, "invalid char '{}'", ch),
		}
	}
}