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
				CharKind::Digit (first_digit, _) => Self::parse_number(pos, first_digit, &mut self.iter)?,
				CharKind::Dot => return Err(TokenErr::Construct { ch, pos_begin: pos , pos_end: pos }),
				CharKind::Plus => Token::new(pos, pos, TokenContent::ArithmeticalOp ( ArithmeticalOp::Plus )),
				CharKind::Minus => Token::new(pos, pos, TokenContent::ArithmeticalOp ( ArithmeticalOp::Minus )),
				CharKind::Mul => Token::new(pos, pos, TokenContent::ArithmeticalOp ( ArithmeticalOp::Mul )),
				CharKind::Div => Token::new(pos, pos, TokenContent::ArithmeticalOp ( ArithmeticalOp::Div )),
				CharKind::LeftBracket => Token::new(pos, pos, TokenContent::Bracket ( Bracket::Left )),
				CharKind::RightBracket => Token::new(pos, pos, TokenContent::Bracket ( Bracket::Right )),
				CharKind::Eq => Self::parse_assignment_or_equality(pos, &mut self.iter),
				CharKind::Letter (first_char) => Self::parse_name_or_keyword(pos, first_char, &mut self.iter)?,
				CharKind::Whitespace => continue,
				CharKind::Punctuation (p) => match p {
					Punctuation::Colon => Token::new(pos, pos, TokenContent::StatementOp (StatementOp::Colon)),
					Punctuation::Semicolon => Token::new(pos, pos, TokenContent::StatementOp (StatementOp::Semicolon)),
					Punctuation::Comma => Token::new(pos, pos, TokenContent::StatementOp (StatementOp::Comma)),
				},
				CharKind::Invalid (..) => return Err(TokenErr::Construct { ch, pos_begin: pos , pos_end: pos } ),
			};
			return Ok(token);
		}
		return Err( TokenErr::EndReached { pos: self.iter.last_pos() } );
	}
	
	pub fn expect(&mut self, expected_token: TokenContent) -> Result<TokenExpectation, TokenErr> {
		let found = self.next()?;
		Ok( TokenExpectation::new(expected_token, found) )
	}
	
	pub fn expect_name(&mut self) -> Result<String, TokenErr> {
		let found = self.next()?;
		match found.content {
			TokenContent::Name ( name ) => Ok( name ),
			_ => return Err( TokenErr::ExpectedButFound { 
							expected: vec![
								TokenContent::Name(String::from("<name>")),
							], 
							found
						} ),
		}
	}
	
	fn parse_number(pos_begin: usize, first_digit: u32, iter: &mut CharsIter) -> Result<Token, TokenErr> {
		let mut value: f32 = first_digit as f32;
		let mut has_dot: bool = false;
		let mut frac_multiplier = 0.1_f32;
		let mut pos_end: usize = pos_begin;
		loop {
			match iter.peek() {
				Some( ( ch, pos ) ) => {
					match ch {
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
								break Err(TokenErr::Construct { ch, pos_begin: pos , pos_end: pos });
							}
						},
						CharKind::Letter (..) => break Err(TokenErr::Construct { ch, pos_begin: pos , pos_end: pos }),
						_ => break Ok( Token::new(pos_begin, pos_end, TokenContent::Number (value) )),
					};
					pos_end = pos;
				},
				None => break Ok( Token::new(pos_begin, pos_end, TokenContent::Number (value) )),
			}
		}
	}

	fn parse_name_or_keyword(pos_begin: usize, first_char: char, iter: &mut CharsIter) -> Result<Token, TokenErr> {
		let mut name = String::from(first_char);
		let mut pos_end: usize = pos_begin;
		
		loop {
			match iter.peek() {
				Some( ( ch, pos ) ) => {
					pos_end = pos;
					match ch {
						CharKind::Digit (_, ch) => {
							name.push(ch);
							iter.next(); 
						},
						CharKind::Letter (ch) => {
							name.push(ch);
							iter.next(); 
						},
						_ => break,
					}
				},
				None => break,
			}
		}
		
		let token = match name.as_str() {
			"var" => Token::new(pos_begin, pos_end, TokenContent::Keyword ( Keyword::Var )),
			_ => Token::new(pos_begin, pos_end, TokenContent::Name (name)),
		};
		
		Ok( token )
	}

	fn parse_assignment_or_equality(pos_begin: usize, iter: &mut CharsIter) -> Token {
		let mut pos_end: usize = pos_begin;
		match iter.peek() {
			Some( ( ch, pos ) ) => {
				pos_end = pos;
				match ch {
					CharKind::Eq => {
						iter.next(); 
						Token::new(pos_begin, pos_end, TokenContent::LogicalOp(LogicalOp::Equals))
					},
					_ => Token::new(pos_begin, pos_end, TokenContent::AssignOp),
				}
			},
			None => Token::new(pos_begin, pos_end, TokenContent::AssignOp),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
	pub pos_begin: usize,
	pub pos_end: usize,
	pub content: TokenContent,
}

impl Token {
	fn new(pos_begin: usize, pos_end: usize, content: TokenContent) -> Self {
		Self { pos_begin, pos_end, content }
	}
	
	pub fn content(&self) -> &TokenContent {
		&self.content
	}
}

impl std::fmt::Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.content())
	}
}

#[derive(Debug, Clone)]
pub enum TokenContent {
	Number (f32),
	ArithmeticalOp (ArithmeticalOp),
	LogicalOp (LogicalOp),
	Bracket (Bracket),
	AssignOp,
	Name (String),
	StatementOp (StatementOp),
	Keyword (Keyword),
}

impl std::fmt::Display for TokenContent {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			TokenContent::Number (val) => write!(f, "'{}'", val),
			TokenContent::ArithmeticalOp (op) => match op {
				ArithmeticalOp::Plus => write!(f, "'{}'", "+"),
				ArithmeticalOp::Minus => write!(f, "'{}'", "-"),
				ArithmeticalOp::Mul => write!(f, "'{}'", "*"),
				ArithmeticalOp::Div => write!(f, "'{}'", "/"),
			},
			TokenContent::LogicalOp (op) => match op {
				LogicalOp::Equals => write!(f, "'{}'", "=="),
			},
			TokenContent::Bracket (br) => match br {
				Bracket::Right => write!(f, "'{}'", ")"),
				Bracket::Left => write!(f, "'{}'", "("),
			},
			TokenContent::AssignOp => write!(f, "'{}'", "="),
			TokenContent::Name (name) => write!(f, "'{}'", name),
			TokenContent::StatementOp (op) => match op {
				StatementOp::Colon => write!(f, "'{}'", ":"),
				StatementOp::Semicolon => write!(f, "'{}'", ";"),
				StatementOp::Comma => write!(f, "'{}'", ","),
			},
			TokenContent::Keyword (kw) => match kw {
				Keyword::Var => write!(f, "'{}'", "var"),
			},
		}
	}
}

impl PartialEq for TokenContent {
	fn eq(&self, other: &Self) -> bool {
		match self {
			TokenContent::Number (f1) => match other {
				TokenContent::Number (f2) => (f1 - f2).abs() <= std::f32::EPSILON,
				_ => false,
			},
			TokenContent::ArithmeticalOp (op1) => match other {
				TokenContent::ArithmeticalOp (op2) => op1 == op2,
				_ => false,
			},
			TokenContent::LogicalOp (op1) => match other {
				TokenContent::LogicalOp (op2) => op1 == op2,
				_ => false,
			},
			TokenContent::Bracket (br1) => match other {
				TokenContent::Bracket (br2) => br1 == br2,
				_ => false,
			},
			TokenContent::Name (s1) => match other {
				TokenContent::Name (s2) => s1 == s2,
				_ => false,
			},
			TokenContent::StatementOp (so1) => match other {
				TokenContent::StatementOp (so2) => so1 == so2,
				_ => false,
			},
			TokenContent::Keyword (kw1) => match other {
				TokenContent::Keyword (kw2) => kw1 == kw2,
				_ => false,
			},
			TokenContent::AssignOp => match other {
				TokenContent::AssignOp => true,
				_ => false,
			},
		}
	}
}
impl Eq for TokenContent {}

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
	Comma,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
	Var,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenErr {
	Construct { ch: CharKind, pos_begin: usize, pos_end: usize },
	ExpectedButFound { expected: Vec<TokenContent>, found: Token },
	EndReached { pos: usize },
}

impl std::fmt::Display for TokenErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			TokenErr::Construct { ch, pos_begin, pos_end } => {
				super::display_error_pos(f, *pos_begin, *pos_end)?;
				write!(f, "Unexpected character '{}'", ch)
			},
			TokenErr::ExpectedButFound { expected, found } => {
				super::display_error_pos(f, found.pos_begin, found.pos_end)?;
				
				writeln!(f, "Expected: ")?;
				for tc in expected {
					writeln!(f, "\t{}", tc)?;
				}
				write!(f, "But found {}", found.content())
			},
			TokenErr::EndReached { pos } => {
				super::display_error_pos(f, *pos, *pos)?;
				write!(f, "Unexpected end")
			},
		}
	}
}

pub struct TokenExpectation {
	expected: Vec<TokenContent>, found: Token
}

impl TokenExpectation {
	fn new(expected_token: TokenContent, found: Token) -> Self {
		Self {
			expected: vec![expected_token], 
			found,
		}
	}
	
	#[allow(unused)]
	pub fn or(mut self, expected_token: TokenContent) -> Self {
		self.expected.push(expected_token);
		self
	}
	
	pub fn check(self) -> Result<(), TokenErr> {
		match self.expected.iter().find(|c| c.eq(&self.found.content())) {
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
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::Number (123_f32));
		assert_eq!(tokens_iter.next().unwrap_err(), TokenErr::EndReached { pos: 3 });
	}

	#[test]
	pub fn can_parse_float() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("123.456"));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::Number (123.456_f32));
		assert_eq!(tokens_iter.next().unwrap_err(), TokenErr::EndReached { pos: 7 });
	}

	#[test]
	pub fn can_parse_plus() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("+"));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::ArithmeticalOp (ArithmeticalOp::Plus));
		assert_eq!(tokens_iter.next().unwrap_err(), TokenErr::EndReached { pos: 1 });
	}

	#[test]
	pub fn can_parse_minus() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("-"));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::ArithmeticalOp (ArithmeticalOp::Minus));
		assert_eq!(tokens_iter.next().unwrap_err(), TokenErr::EndReached { pos: 1 });
	}

	#[test]
	pub fn can_parse_mul() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("*"));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::ArithmeticalOp (ArithmeticalOp::Mul));
		assert_eq!(tokens_iter.next().unwrap_err(), TokenErr::EndReached { pos: 1 });
	}

	#[test]
	pub fn can_parse_div() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("/"));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::ArithmeticalOp (ArithmeticalOp::Div));
		assert_eq!(tokens_iter.next().unwrap_err(), TokenErr::EndReached { pos: 1 });
	}

	#[test]
	pub fn can_parse_name() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("var1"));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::Name (String::from("var1")));
		assert_eq!(tokens_iter.next().unwrap_err(), TokenErr::EndReached { pos: 4 });
	}

	#[test]
	pub fn can_parse_keyword() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("var"));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::Keyword ( Keyword::Var ));
		assert_eq!(tokens_iter.next().unwrap_err(), TokenErr::EndReached { pos: 3 });
	}

	#[test]
	pub fn can_parse_simple_expr() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("1+23.4-45.6*7.8/9 var1var"));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::Number (1_f32));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::ArithmeticalOp (ArithmeticalOp::Plus));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::Number (23.4_f32));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::ArithmeticalOp (ArithmeticalOp::Minus));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::Number (45.6_f32));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::ArithmeticalOp (ArithmeticalOp::Mul));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::Number (7.8_f32));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::ArithmeticalOp (ArithmeticalOp::Div));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::Number (9_f32));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::Name (String::from("var1var")));
		assert_eq!(tokens_iter.next().unwrap_err(), TokenErr::EndReached { pos: 25 });
	}

	#[test]
	pub fn can_parse_simple_expr_with_whitespaces() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("1\t+  23.4 \n-  45.6\n\n *7.8  / \t\t9 \n var1 var"));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::Number (1_f32));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::ArithmeticalOp (ArithmeticalOp::Plus));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::Number (23.4_f32));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::ArithmeticalOp (ArithmeticalOp::Minus));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::Number (45.6_f32));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::ArithmeticalOp (ArithmeticalOp::Mul));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::Number (7.8_f32));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::ArithmeticalOp (ArithmeticalOp::Div));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::Number (9_f32));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::Name (String::from("var1")));
		assert_eq!(*tokens_iter.next().unwrap().content(), TokenContent::Keyword ( Keyword::Var ));
		assert_eq!(tokens_iter.next().unwrap_err(), TokenErr::EndReached { pos: 43 });
	}
}