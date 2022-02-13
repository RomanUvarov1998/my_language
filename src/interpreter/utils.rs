use super::token::{Token, TokenContent, TokenErr};
use super::InterpErr;
use std::collections::HashMap;

//--------------------- NameToken --------------------------

#[derive(Debug, Clone)]
pub struct NameToken {
	pub name: String,
	pub is_builtin: bool,
	pub pos: CodePos,
}

impl NameToken {
	pub fn from_or_err(tok: Token) -> Result<Self, InterpErr> {
		let pos = tok.pos();
		match tok {
			Token { content: TokenContent::Name (name), .. } => 
				Ok( NameToken {
					is_builtin: false,
					name,
					pos,
				} ),
				
			Token { content: TokenContent::BuiltinName (name), .. } => 
				Ok( NameToken {
					is_builtin: true,
					name,
					pos,
				} ),
				
			found @ _ => return Err(TokenErr::ExpectedButFound {
							expected: vec![TokenContent::Name ("<name>".to_string())], 
							found,
						}.into()),
		}
	}
	
	pub fn new_with_pos(name: String, pos: CodePos, is_builtin: bool) -> Self {
		Self {
			name,
			is_builtin,
			pos,
		}
	}
	
	pub fn value(&self) -> &str { &self.name }
	pub fn pos(&self) -> CodePos { self.pos }
	pub fn is_builtin(&self) -> bool { self.is_builtin }
}

impl std::fmt::Display for NameToken {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.value())
	}
}

impl PartialEq for NameToken {
	fn eq(&self, other: &Self) -> bool {
		self.value() == other.value() && self.is_builtin() == other.is_builtin()
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

//------------------------------ HashMapTrait ----------------------------

pub trait HashMapInsertPanic<K, V>
where
	K: std::fmt::Display + Eq + std::hash::Hash,
	V: std::fmt::Debug
{
	fn insert_assert_not_replace(&mut self, key: K, value: V);
}

impl<K, V> HashMapInsertPanic<K, V> for HashMap<K, V>
where
	K: std::fmt::Display + Eq + std::hash::Hash,
	V: std::fmt::Debug
{
	fn insert_assert_not_replace(&mut self, key: K, value: V) {
		if let Some(old_value) = self.get(&key) {
			panic!("HashMap for key {}, already contains value {:#?}", key, old_value);
		}
		self.insert(key, value);
	}
}