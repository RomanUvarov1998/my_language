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
				CharKind::Plus => Token::Plus,
				CharKind::Minus => Token::Minus,
				CharKind::Mul => Token::Mul,
				CharKind::Div => Token::Div,
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
		let mut value: u32 = first_digit;
		loop {
			match iter.peek() {
				Some( ( ch, _pos ) ) => {
					match ch {
						CharKind::Digit (val) => {
							value *= 10_u32;
							value += val;
							iter.next(); // skip current digit
						},
						CharKind::Dot => {
							iter.next(); // skip dot
							return Self::parse_frac(value, iter);
						},
						_ => return Ok( Token::Int (value) ),
					}
				},
				None => return Ok( Token::Int (value) ),
			}
		}
	}
	
	fn parse_frac(int_part: u32, iter: &mut CharsIter) -> Result<Token, TokConstructErr> {
		let mut value: f32 = int_part as f32;
		let mut frac_multiplier = 0.1_f32;
		loop {
			match iter.peek() {
				Some( ( ch, pos ) ) => {
					match ch {
						CharKind::Digit (val) => {
							value += val as f32 * frac_multiplier;
							frac_multiplier *= 0.1_f32;
							iter.next(); // skip current digit
						},
						CharKind::Dot => return Err(TokConstructErr::new(ch, pos)),
						_ => return Ok( Token::Float (value) ),
					}
				},
				None => return Ok( Token::Float (value) ),
			}
		}
	}
}

#[derive(Debug, Clone, Copy)]
pub enum Token {
	Int (u32),
	Float (f32),
	Plus,
	Minus,
	Mul,
	Div
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
		match self {
			Token::Int (i1) => match other {
				Token::Int (i2) => i1 == i2,
				_ => false,
			},
			Token::Float (f1) => match other {
				Token::Float (f2) => (f1 - f2).abs() < std::f32::EPSILON,
				_ => false,
			},
			Token::Plus => match other {
				Token::Plus => true,
				_ => false,
			},
			Token::Minus => match other {
				Token::Minus => true,
				_ => false,
			},
			Token::Mul => match other {
				Token::Mul => true,
				_ => false,
			},
			Token::Div => match other {
				Token::Div => true,
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