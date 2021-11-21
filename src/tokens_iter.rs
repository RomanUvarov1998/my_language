use super::chars_iter::*;

pub struct TokensIter<'code> {
	iter: CharsIter<'code>,
	peeked_token: Option<Token>,
}

impl<'code> TokensIter<'code> {
	pub fn new(iter: CharsIter<'code>) -> Self {
		Self { 
			iter,
			peeked_token: None,
		}
	}
	
	pub fn peek<'iter>(&'iter mut self) -> Result<Option<&'iter Token>, TokConstructErr> {
		if let None = self.peeked_token {
			self.peeked_token = self.next()?;
		}		
		Ok( self.peeked_token.as_ref() )
	}
	
	pub fn next(&mut self) -> Result<Option<Token>, TokConstructErr> {	
		if self.peeked_token.is_some() {
			return Ok( self.peeked_token.take() );
		}
	
		while let Some( ( ch, pos ) ) = self.iter.next(){
			let token: Token = match ch {
				CharKind::Digit (first_digit, _) => Self::parse_number(first_digit, &mut self.iter)?,
				CharKind::Dot => return Err(TokConstructErr::new(ch, pos)),
				CharKind::Plus => Token::ArithmeticalOp ( ArithmeticalOp::Plus ),
				CharKind::Minus => Token::ArithmeticalOp ( ArithmeticalOp::Minus ),
				CharKind::Mul => Token::ArithmeticalOp ( ArithmeticalOp::Mul ),
				CharKind::Div => Token::ArithmeticalOp ( ArithmeticalOp::Div ),
				CharKind::LeftBracket => Token::Bracket ( Bracket::Left ),
				CharKind::RightBracket => Token::Bracket ( Bracket::Right ),
				CharKind::Eq => Self::parse_assignment_or_equality(&mut self.iter),
				CharKind::Letter (first_char) => Self::parse_name_or_keyword(first_char, &mut self.iter)?,
				CharKind::Whitespace => continue,
				CharKind::Punctuation (p) => match p {
					Punctuation::Colon => Token::StatementOp (StatementOp::Colon),
					Punctuation::Semicolon => Token::StatementOp (StatementOp::Semicolon),
					Punctuation::Comma => Token::StatementOp (StatementOp::Comma),
				},
				CharKind::Invalid (..) => return Err(TokConstructErr::new(ch, pos)),
			};
			return Ok(Some(token));
		}
		return Ok(None);
	}
	
	fn parse_number(first_digit: u32, iter: &mut CharsIter) -> Result<Token, TokConstructErr> {
		let mut value: f32 = first_digit as f32;
		let mut has_dot: bool = false;
		let mut frac_multiplier = 0.1_f32;
		loop {
			match iter.peek() {
				Some( ( ch, pos ) ) => match ch {
					CharKind::Digit (val, _) => {
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
							break Err(TokConstructErr::new(ch, pos));
						}
					},
					CharKind::Letter (..) => break Err(TokConstructErr::new(ch, pos)),
					_ => break Ok( Token::Number (value) ),
				},
				None => break Ok( Token::Number (value) ),
			}
		}
	}

	fn parse_name_or_keyword(first_char: char, iter: &mut CharsIter) -> Result<Token, TokConstructErr> {
		let mut name = String::from(first_char);
		
		loop {
			match iter.peek() {
				Some( ( ch, _pos ) ) => match ch {
					CharKind::Digit (_, ch) => {
						name.push(ch);
						iter.next(); 
					},
					CharKind::Letter (ch) => {
						name.push(ch);
						iter.next(); 
					},
					_ => break,
				},
				None => break,
			}
		}
		
		let token = match name.as_str() {
			"var" => Token::Keyword ( Keyword::Var ),
			_ => Token::Name (name),
		};
		
		Ok( token )
	}

	fn parse_assignment_or_equality(iter: &mut CharsIter) -> Token {
		match iter.peek() {
			Some( ( ch, _pos ) ) => match ch {
				CharKind::Eq => {
					iter.next(); 
					Token::LogicalOp(LogicalOp::Equals)
				},
				_ => Token::AssignOp,
			},
			None => Token::AssignOp,
		}
	}
}

#[derive(Debug, Clone)]
pub enum Token {
	Number (f32),
	ArithmeticalOp (ArithmeticalOp),
	LogicalOp (LogicalOp),
	Bracket (Bracket),
	AssignOp,
	Name (String),
	StatementOp (StatementOp),
	Keyword (Keyword),
}

impl PartialEq for Token {
	fn eq(&self, other: &Self) -> bool {
		match self {
			Token::Number (f1) => match other {
				Token::Number (f2) => (f1 - f2).abs() <= std::f32::EPSILON,
				_ => false,
			},
			Token::ArithmeticalOp (op1) => match other {
				Token::ArithmeticalOp (op2) => op1 == op2,
				_ => false,
			},
			Token::LogicalOp (op1) => match other {
				Token::LogicalOp (op2) => op1 == op2,
				_ => false,
			},
			Token::Bracket (br1) => match other {
				Token::Bracket (br2) => br1 == br2,
				_ => false,
			},
			Token::Name (s1) => match other {
				Token::Name (s2) => s1 == s2,
				_ => false,
			},
			Token::StatementOp (so1) => match other {
				Token::StatementOp (so2) => so1 == so2,
				_ => false,
			},
			Token::Keyword (kw1) => match other {
				Token::Keyword (kw2) => kw1 == kw2,
				_ => false,
			},
			Token::AssignOp => match other {
				Token::AssignOp => true,
				_ => false,
			},
		}
	}
}
impl Eq for Token {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArithmeticalOp {
	Plus,
	Minus,
	Mul,
	Div,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicalOp {
	Equals,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Bracket {
	Right,
	Left,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StatementOp {
	Colon,
	Semicolon,
	Comma
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
	Var,
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
		write!(f, "Unexpected {} in position {}", self.ch, self.pos)
	}
}