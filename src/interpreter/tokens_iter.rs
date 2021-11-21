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
	
	pub fn peek<'iter>(&'iter mut self) -> Result<&'iter Token, TokenErr> {
		if let None = self.peeked_token {
			self.peeked_token = Some( self.next()? );
		}		
		Ok( self.peeked_token.as_ref().unwrap() )
	}
	
	pub fn next(&mut self) -> Result<Token, TokenErr> {	
		if self.peeked_token.is_some() {
			return Ok( self.peeked_token.take().unwrap() );
		}
	
		while let Some( ( ch, pos ) ) = self.iter.next(){
			let token: Token = match ch {
				CharKind::Digit (first_digit, _) => Self::parse_number(first_digit, &mut self.iter)?,
				CharKind::Dot => return Err(TokenErr::Construct { ch, pos }),
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
				CharKind::Invalid (..) => return Err(TokenErr::Construct { ch, pos } ),
			};
			return Ok(token);
		}
		return Err( TokenErr::EndReached );
	}
	
	pub fn expect(&mut self, expected_token: Token) -> Result<TokenExpectation, TokenErr> {
		let found = self.next()?;
		Ok( TokenExpectation::new(expected_token, found) )
	}
	
	pub fn expect_name(&mut self) -> Result<String, TokenErr> {
		let found = self.next()?;
		match found {
			Token::Name ( name ) => Ok( name ),
			_ => return Err( TokenErr::ExpectedButFound { 
							expected: vec![
								Token::Name(String::from("<name>")),
							], 
							found
						} ),
		}
	}
	
	fn parse_number(first_digit: u32, iter: &mut CharsIter) -> Result<Token, TokenErr> {
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
							break Err(TokenErr::Construct { ch, pos });
						}
					},
					CharKind::Letter (..) => break Err(TokenErr::Construct { ch, pos }),
					_ => break Ok( Token::Number (value) ),
				},
				None => break Ok( Token::Number (value) ),
			}
		}
	}

	fn parse_name_or_keyword(first_char: char, iter: &mut CharsIter) -> Result<Token, TokenErr> {
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

#[derive(Debug, PartialEq, Eq)]
pub enum TokenErr {
	Construct { ch: CharKind, pos: usize },
	ExpectedButFound { expected: Vec<Token>, found: Token },
	EndReached,
}

impl std::fmt::Display for TokenErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			TokenErr::Construct { ch, pos } => write!(f, "Unexpected character '{}' in position {}", ch, pos),
			TokenErr::ExpectedButFound { expected, found } => write!(f, "Expected {:?} but found {:?}", expected, found),
			TokenErr::EndReached => write!(f, "End reached"),
		}
	}
}

pub struct TokenExpectation {
	expected: Vec<Token>, found: Token
}

impl TokenExpectation {
	fn new(expected_token: Token, found: Token) -> Self {
		Self {
			expected: vec![expected_token], 
			found,
		}
	}
	
	#[allow(unused)]
	pub fn or(mut self, expected_token: Token) -> Self {
		self.expected.push(expected_token);
		self
	}
	
	pub fn check(self) -> Result<(), TokenErr> {
		match self.expected.iter().find(|t| **t == self.found) {
			Some (_) => Ok(()),
			None => {
				let TokenExpectation { expected, found } = self;
				Err( TokenErr::ExpectedButFound { expected, found } )
			},
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	
	#[test]
	pub fn can_parse_int() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("123"));
		assert_eq!(tokens_iter.next().unwrap(), Token::Number (123_f32));
		assert_eq!(tokens_iter.next().unwrap_err(), TokenErr::EndReached);
	}

	#[test]
	pub fn can_parse_float() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("123.456"));
		assert_eq!(tokens_iter.next().unwrap(), Token::Number (123.456_f32));
		assert_eq!(tokens_iter.next().unwrap_err(), TokenErr::EndReached);
	}

	#[test]
	pub fn can_parse_plus() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("+"));
		assert_eq!(tokens_iter.next().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Plus));
		assert_eq!(tokens_iter.next().unwrap_err(), TokenErr::EndReached);
	}

	#[test]
	pub fn can_parse_minus() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("-"));
		assert_eq!(tokens_iter.next().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Minus));
		assert_eq!(tokens_iter.next().unwrap_err(), TokenErr::EndReached);
	}

	#[test]
	pub fn can_parse_mul() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("*"));
		assert_eq!(tokens_iter.next().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Mul));
		assert_eq!(tokens_iter.next().unwrap_err(), TokenErr::EndReached);
	}

	#[test]
	pub fn can_parse_div() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("/"));
		assert_eq!(tokens_iter.next().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Div));
		assert_eq!(tokens_iter.next().unwrap_err(), TokenErr::EndReached);
	}

	#[test]
	pub fn can_parse_name() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("var1"));
		assert_eq!(tokens_iter.next().unwrap(), Token::Name (String::from("var1")));
		assert_eq!(tokens_iter.next().unwrap_err(), TokenErr::EndReached);
	}

	#[test]
	pub fn can_parse_keyword() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("var"));
		assert_eq!(tokens_iter.next().unwrap(), Token::Keyword ( Keyword::Var ));
		assert_eq!(tokens_iter.next().unwrap_err(), TokenErr::EndReached);
	}

	#[test]
	pub fn can_parse_simple_expr() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("1+23.4-45.6*7.8/9 var1var"));
		assert_eq!(tokens_iter.next().unwrap(), Token::Number (1_f32));
		assert_eq!(tokens_iter.next().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Plus));
		assert_eq!(tokens_iter.next().unwrap(), Token::Number (23.4_f32));
		assert_eq!(tokens_iter.next().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Minus));
		assert_eq!(tokens_iter.next().unwrap(), Token::Number (45.6_f32));
		assert_eq!(tokens_iter.next().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Mul));
		assert_eq!(tokens_iter.next().unwrap(), Token::Number (7.8_f32));
		assert_eq!(tokens_iter.next().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Div));
		assert_eq!(tokens_iter.next().unwrap(), Token::Number (9_f32));
		assert_eq!(tokens_iter.next().unwrap(), Token::Name (String::from("var1var")));
		assert_eq!(tokens_iter.next().unwrap_err(), TokenErr::EndReached);
	}

	#[test]
	pub fn can_parse_simple_expr_with_whitespaces() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("1\t+  23.4 \n-  45.6\n\n *7.8  / \t\t9 \n var1 var"));
		assert_eq!(tokens_iter.next().unwrap(), Token::Number (1_f32));
		assert_eq!(tokens_iter.next().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Plus));
		assert_eq!(tokens_iter.next().unwrap(), Token::Number (23.4_f32));
		assert_eq!(tokens_iter.next().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Minus));
		assert_eq!(tokens_iter.next().unwrap(), Token::Number (45.6_f32));
		assert_eq!(tokens_iter.next().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Mul));
		assert_eq!(tokens_iter.next().unwrap(), Token::Number (7.8_f32));
		assert_eq!(tokens_iter.next().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Div));
		assert_eq!(tokens_iter.next().unwrap(), Token::Number (9_f32));
		assert_eq!(tokens_iter.next().unwrap(), Token::Name (String::from("var1")));
		assert_eq!(tokens_iter.next().unwrap(), Token::Keyword ( Keyword::Var ));
		assert_eq!(tokens_iter.next().unwrap_err(), TokenErr::EndReached);
	}
}