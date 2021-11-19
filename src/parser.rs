pub struct Parser {
	toks: Vec<Token>
}

const RADIX: u32 = 10_u32;

impl Parser {
	pub fn new(code: &str) -> Result<Self, TokConstructErr> {
		let mut toks = Vec::<Token>::new();
				
		let mut iter: CharsIter = CharsIter::new(code);
		
		while let Some( ( ch, pos ) ) = iter.next(){
			let tok = match ch {
				CharKind::Digit (first_digit) => Self::parse_number(first_digit, &mut iter)?,
				CharKind::Dot => return Err(TokConstructErr::new(ch, pos)),
				CharKind::Plus => Token::BinOp ( BinOp::Plus ),
				CharKind::Minus => Token::BinOp ( BinOp::Minus ),
				CharKind::Mul => Token::BinOp ( BinOp::Mul ),
				CharKind::Div => Token::BinOp ( BinOp::Div ),
				CharKind::Whitespace => continue,
				CharKind::Invalid (..) => return Err(TokConstructErr::new(ch, pos)),
			};
			toks.push(tok);
		}
		
		Ok( Parser { toks } )
	}
	
	pub fn tokens(&self) -> &Vec<Token> {
		&self.toks
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
	BinOp (BinOp)
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum BinOp {
	Plus,
	Minus,
	Mul,
	Div
}
impl BinOp {
	fn get_rank(self) -> u32 {
		match self {
			BinOp::Plus | BinOp::Minus => 1,
			BinOp::Mul | BinOp::Div => 2,
		}
	}
}
use std::cmp::Ordering;
impl Ord for BinOp {
    fn cmp(&self, other: &Self) -> Ordering {
		self.get_rank().cmp(&other.get_rank())
    }
}
impl PartialOrd for BinOp {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
		match self {
			Token::Number (val1) => match other {
				Token::Number (val2) => (val1 - val2).abs() <= std::f32::EPSILON,
				_ => false,
			},
			Token::BinOp (bin_op1) => match other {
				Token::BinOp (bin_op2) => bin_op1 == bin_op2,
				_ => false,
			},
		}
    }
}
impl Eq for Token {}

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
	Whitespace,
	Invalid (char),
}