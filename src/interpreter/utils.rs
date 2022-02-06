use super::token::{Token, TokenContent, TokenErr};
use super::InterpErr;

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
	
	pub fn new_with_pos(name: String, pos: CodePos) -> Self {
		Self {
			name,
			pos,
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

impl std::fmt::Display for CodePos {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{} - {}", self.begin, self.end)
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
	
	pub fn line(&self) -> usize { self.line }
	pub fn col(&self) -> usize { self.col }
	
	pub fn advance_col(&mut self) {
		self.col += 1;
	}
	pub fn advance_line(&mut self) {
		self.col = 0;
		self.line += 1;
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

impl std::fmt::Display for CharPos {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}:{}", self.line, self.col)
	}
}