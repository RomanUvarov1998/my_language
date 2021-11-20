pub struct Parser<'code> {
	code: &'code str,
}

const RADIX: u32 = 10_u32;

impl<'code> Parser<'code> {
	pub fn new(code: &'code str) -> Self {
		Self { code }
	}
	
	pub fn get_tokens_iter(&self) -> TokensIter<'code> {
		TokensIter::new( CharsIter::new(self.code) )
	}
}

pub struct TokensIter<'code> {
	iter: CharsIter<'code>
}

impl<'code> TokensIter<'code> {
	fn new(iter: CharsIter<'code>) -> Self {
		Self { iter }
	}
	
	pub fn next_token(&mut self) -> Result<Option<Token>, TokConstructErr> {		
		while let Some( ( ch, pos ) ) = self.iter.next(){
			let tok = match ch {
				CharKind::Digit (first_digit) => Self::parse_number(first_digit, &mut self.iter)?,
				CharKind::Dot => return Err(TokConstructErr::new(ch, pos)),
				CharKind::Plus => Token::BinOp ( BinOp::Plus ),
				CharKind::Minus => Token::BinOp ( BinOp::Minus ),
				CharKind::Mul => Token::BinOp ( BinOp::Mul ),
				CharKind::Div => Token::BinOp ( BinOp::Div ),
				CharKind::LeftBracket => Token::Bracket ( Bracket::Left ),
				CharKind::RightBracket => Token::Bracket ( Bracket::Right ),
				CharKind::Whitespace => continue,
				CharKind::Invalid (..) => return Err(TokConstructErr::new(ch, pos)),
			};
			return Ok(Some(tok));
		}
		return Ok(None);
	}
	
	fn parse_number(first_digit: u32, iter: &mut CharsIter) -> Result<Token, TokConstructErr> {
		let mut value: f32 = first_digit as f32;
		let mut has_dot: bool = false;
		let mut frac_multiplier = 0.1_f32;
		loop {
			match iter.peek() {
				Some( ( ch, pos ) ) => {
					match ch {
						CharKind::Digit (val) => {
							if has_dot {
								value += val as f32 * frac_multiplier;
								frac_multiplier *= 0.1_f32;
							} else {
								value *= 10_f32;
								value += val as f32;
							}
							iter.next(); // skip current digit
						},
						CharKind::Dot => {
							iter.next(); // skip dot
							if !has_dot {
								has_dot = true;
							} else {
								return Err(TokConstructErr::new(ch, pos));
							}
						},
						_ => return Ok( Token::Number (value) ),
					}
				},
				None => return Ok( Token::Number (value) ),
			}
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub enum Token {
	Number (f32),
	BinOp (BinOp),
	Bracket (Bracket),
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
	Plus,
	Minus,
	Mul,
	Div
}

#[derive(Debug, Clone, Copy)]
pub enum Bracket {
	Right,
	Left,
}

#[derive(Debug)]
pub struct TokConstructErr { ch: CharKind, pos: usize, }

impl TokConstructErr {
	pub fn new(ch: CharKind, pos: usize) -> Self {
		Self { ch, pos }
	}
}

impl std::fmt::Display for TokConstructErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "Unexpected ")?;
		match self.ch {
			CharKind::Digit (value) => write!(f, "'{}'", value)?,
			CharKind::Dot => write!(f, "'.'")?,
			CharKind::Plus => write!(f, "'+'")?,
			CharKind::Minus => write!(f, "'-'")?,
			CharKind::Mul => write!(f, "'*'")?,
			CharKind::Div => write!(f, "'/'")?,
			CharKind::LeftBracket => write!(f, "'('")?,
			CharKind::RightBracket => write!(f, "')'")?,
			CharKind::Whitespace => write!(f, "Whitespace")?,
			CharKind::Invalid (ch) => write!(f, "invalid '{}'", ch)?,
		};
		write!(f, " in position {}", self.pos)
	}
}

struct CharsIter<'code> {
	ch_iter: std::iter::Peekable<std::str::Chars<'code>>,
	pos: usize,
}

impl<'code> CharsIter<'code> {
	fn new(text: &'code str) -> Self {
		Self {
			ch_iter: text.chars().peekable(),
			pos: 0_usize,
		}
	}
	
	fn peek(&mut self) -> Option<(CharKind, usize)> {
		let ch = self.ch_iter.peek()?;
		Some( ( Self::get_kind(*ch), self.pos ) )
	}
	
	fn get_kind(ch: char) -> CharKind {
		if let Some(value) = ch.to_digit(RADIX) {
			CharKind::Digit(value)
		} else {
			match ch {
				'.' => CharKind::Dot,
				'+' => CharKind::Plus,
				'-' => CharKind::Minus,
				'*' => CharKind::Mul,
				'/' => CharKind::Div,
				'(' => CharKind::LeftBracket,
				')' => CharKind::RightBracket,
				' ' | '\n' | '\t' => CharKind::Whitespace,
				_ => CharKind::Invalid (ch)
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
	Digit (u32),
	Dot,
	Plus,
	Minus,
	Mul,
	Div,
	LeftBracket,
	RightBracket,
	Whitespace,
	Invalid (char),
}