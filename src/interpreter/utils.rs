use super::token::{Token, TokenContent, TokenErr};
use super::string_char::CharKind;
use super::InterpErr;
use super::var_data::Value;

//--------------------- ValueToken --------------------------

#[derive(Debug, Clone)]
pub struct ValueToken {
	value: Value,
	pos: CodePos,
}

impl ValueToken {
	pub fn new<T>(value: T, pos: CodePos) -> Self 
	where
		Value: From<T>
	{
		ValueToken {
			value: Value::from(value),
			pos,
		}
	}
	
	pub fn value(&self) -> &Value { &self.value }
	pub fn pos(&self) -> CodePos { self.pos }
}

impl std::fmt::Display for ValueToken {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.value())
	}
}

impl PartialEq for ValueToken {
	fn eq(&self, other: &Self) -> bool {
		self.value() == other.value()
	}
}
impl Eq for ValueToken {}

//--------------------- NameToken --------------------------

#[derive(Debug, Clone)]
pub struct NameToken {
	pub name: String,
	pub pos: CodePos,
}

impl NameToken {
	pub fn from_or_err(tok: Token) -> Result<Self, InterpErr> {
		let pos = tok.pos();
		match tok {
			Token { content: TokenContent::Name (name), .. }
				| Token { content: TokenContent::BuiltinName (name), .. } => 
			Ok( NameToken {
				name,
				pos,
			} ),
			found @ _ => return Err( InterpErr::from( TokenErr::ExpectedButFound {
							expected: vec![TokenContent::Name ("<name>".to_string())], 
							found,
						} ) ),
		}
	}
	
	pub fn new_with_pos(name: &str, pos: CodePos) -> Self {
		Self {
			name: name.to_string(),
			pos,
		}
	}
	
	pub fn new(name: &str) -> Self {
		Self {
			name: name.to_string(),
			pos: CodePos::from(CharPos::new()),
		}
	}
	
	pub fn value(&self) -> &str { &self.name }
	pub fn pos(&self) -> CodePos { self.pos }
}

impl std::fmt::Display for NameToken {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.value())
	}
}

impl PartialEq for NameToken {
	fn eq(&self, other: &Self) -> bool {
		self.value() == other.value()
	}
}
impl Eq for NameToken {}

//------------------------------ CodePos ----------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CodePos {
	begin: CharPos,
	end: CharPos,
}

impl CodePos {
	pub fn new(begin: CharPos, end: CharPos) -> Self {
		Self {
			begin,
			end,
		}
	}
	
	pub fn begin(&self) -> CharPos {
		self.begin
	}
	pub fn end(&self) -> CharPos {
		self.end
	}
}

impl From<CharPos> for CodePos {
	fn from(ch_pos: CharPos) -> CodePos {
		CodePos::new(ch_pos, ch_pos)
	}
}

//------------------------------ CharPos ----------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CharPos {
	pub line: usize,
	pub col: usize,
}

impl CharPos {
	pub fn new() -> Self {
		Self {
			line: 0,
			col: 0,
		}
	}
	
	#[allow(dead_code)]
	pub fn line(&self) -> usize { self.line }
	pub fn col(&self) -> usize { self.col }
	
	pub fn advance(&mut self, ch_kind: CharKind) {
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

use std::cmp::Ordering;
impl std::cmp::PartialOrd for CharPos {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl std::cmp::Ord for CharPos {
	fn cmp(&self, other: &Self) -> Ordering {
		if self.line < other.line {
			Ordering::Less
		} else if self.line > other.line {
			Ordering::Greater
		} else {
			self.col.cmp(&other.col)
		}
	}
}