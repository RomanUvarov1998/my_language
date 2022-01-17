use super::string_char::*;

pub struct TokensIter<'code> {
	iter: CharsIter<'code>,
	peeked_token: Option<Result<Token, TokenErr>>,
}

impl<'code> TokensIter<'code> {
	pub fn new(iter: CharsIter<'code>) -> Self {
		Self { 
			iter,
			peeked_token: None,
		}
	}
	
	pub fn peek_or_err<'iter>(&'iter mut self) -> Result<&'iter Token, &'iter TokenErr> {
		if self.peeked_token.is_none() {
			self.peeked_token = Some( self.next_or_err() );
		}		
		self.peeked_token.as_ref().unwrap().as_ref()
	}
	
	pub fn next_or_err(&mut self) -> Result<Token, TokenErr> {
		self.next().unwrap_or( Err( TokenErr::EndReached { pos: self.iter.last_pos() } ) )
	}
	
	pub fn expect(&mut self, expected_token: TokenContent) -> Result<TokenExpectation, TokenErr> {
		let found = self.next_or_err()?;
		Ok( TokenExpectation::new(expected_token, found) )
	}
	
	pub fn expect_name(&mut self) -> Result<String, TokenErr> {
		let found = self.next_or_err()?;
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
	
	fn parse_number(&mut self, pos_begin: usize, first_digit: u32, already_encountered_dot : bool) -> Result<Token, TokenErr> {
		let mut value: f32 = first_digit as f32;
		let mut has_dot: bool = already_encountered_dot;
		let mut frac_multiplier = 0.1_f32;
		let mut pos_end: usize = pos_begin;
		loop {
			match self.iter.peek() {
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
							self.iter.next(); // skip current digit
						},
						CharKind::Dot => {
							self.iter.next(); // skip dot
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

	fn parse_name_or_keyword(&mut self, pos_begin: usize, first_char: char) -> Result<Token, TokenErr> {
		let is_builtin: bool;
		let mut name = if first_char == '@' {
			is_builtin = true;
			String::new()
		} else {
			is_builtin = false;
			String::from(first_char)
		};
		
		let mut pos_end: usize = pos_begin;
		
		loop {
			match self.iter.peek() {
				Some( ( ch, pos ) ) => {
					match ch {
						CharKind::Digit (_, ch) => {
							name.push(ch);
							self.iter.next(); 
						},
						CharKind::Letter (ch) => {
							name.push(ch);
							self.iter.next(); 
						},
						_ => break,
					};
					pos_end = pos;
				},
				None => break,
			}
		}
		
		let token = match name.as_str() {
			"var" => Token::new(pos_begin, pos_end, TokenContent::Keyword ( Keyword::Var )),
			_ => if is_builtin {
					Token::new(pos_begin, pos_end, TokenContent::BuiltinName (name))
				} else {
					Token::new(pos_begin, pos_end, TokenContent::Name (name))
				},
		};
		
		Ok( token )
	}

	fn parse_assignment_or_equality(&mut self, pos_begin: usize) -> Result<Token, TokenErr> {
		let mut pos_end: usize = pos_begin;
		match self.iter.peek() {
			Some( ( ch, pos ) ) => {
				pos_end = pos;
				match ch {
					CharKind::Eq => {
						self.iter.next(); 
						Ok( Token::new(pos_begin, pos_end, TokenContent::Operator (Operator::Equals)) )
					},
					_ => Ok( Token::new(pos_begin, pos_end, TokenContent::Operator (Operator::Assign)) ),
				}
			},
			None => Ok( Token::new(pos_begin, pos_end, TokenContent::Operator (Operator::Assign)) ),
		}
	}
}

impl Iterator for TokensIter<'_> {
	type Item = Result<Token, TokenErr>;
	
	fn next(&mut self) -> Option<Self::Item> {
		if self.peeked_token.is_some() {
			return Some( self.peeked_token.take().unwrap() );
		}

		for ( ch, pos ) in self.iter.by_ref() {			
			let token_result = match ch {
				CharKind::Digit (first_digit, _) => self.parse_number(pos, first_digit, false),
				CharKind::Dot => match self.iter.peek() {
					Some( ( ch, pos ) ) => match ch {
						CharKind::Digit (_, _) => self.parse_number(pos, 0_u32, true),
						_ => Err(TokenErr::Construct { ch, pos_begin: pos , pos_end: pos } ),
					},
					None => Err(TokenErr::Construct { ch, pos_begin: pos , pos_end: pos }),
				},
				CharKind::Plus => Ok( Token::new(pos, pos, TokenContent::Operator ( Operator::Plus )) ),
				CharKind::Minus => Ok( Token::new(pos, pos, TokenContent::Operator ( Operator::Minus )) ),
				CharKind::Asterisk => Ok( Token::new(pos, pos, TokenContent::Operator ( Operator::Mul )) ),
				CharKind::Circumflex => Ok( Token::new(pos, pos, TokenContent::Operator ( Operator::Pow )) ),
				CharKind::LeftSlash => Ok( Token::new(pos, pos, TokenContent::Operator ( Operator::Div )) ),
				CharKind::LeftBracket => Ok( Token::new(pos, pos, TokenContent::Bracket ( Bracket::Left )) ),
				CharKind::RightBracket => Ok( Token::new(pos, pos, TokenContent::Bracket ( Bracket::Right )) ),
				CharKind::Eq => self.parse_assignment_or_equality(pos),
				CharKind::Letter (first_char) => self.parse_name_or_keyword(pos, first_char),
				CharKind::Dog => self.parse_name_or_keyword(pos, '@'),
				CharKind::Whitespace | CharKind::Control => continue,
				CharKind::Punctuation (p) => match p {
					Punctuation::Colon => Ok( Token::new(pos, pos, TokenContent::StatementOp (StatementOp::Colon)) ),
					Punctuation::Semicolon => Ok( Token::new(pos, pos, TokenContent::StatementOp (StatementOp::Semicolon)) ),
					Punctuation::Comma => Ok( Token::new(pos, pos, TokenContent::StatementOp (StatementOp::Comma)) ),
				},
				CharKind::Invalid (..) => Err(TokenErr::Construct { ch, pos_begin: pos , pos_end: pos } ),
			};
			
			return Some( token_result );
		}
		
		Some( Err( TokenErr::EndReached { pos: self.iter.last_pos() } ) )
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
	BuiltinName (String),
	Operator (Operator),
	Bracket (Bracket),
	Name (String),
	StatementOp (StatementOp),
	Keyword (Keyword),
}

impl std::fmt::Display for TokenContent {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			TokenContent::Number (val) => write!(f, "'{}'", val),
			TokenContent::Operator (op) => match op {
				Operator::Plus => write!(f, "'{}'", "+"),
				Operator::Minus => write!(f, "'{}'", "-"),
				Operator::Mul => write!(f, "'{}'", "*"),
				Operator::Div => write!(f, "'{}'", "/"),
				Operator::Pow => write!(f, "'{}'", "^"),
				Operator::Equals => write!(f, "'{}'", "=="),
				Operator::Assign => write!(f, "'{}'", "="),
			},
			TokenContent::Bracket (br) => match br {
				Bracket::Right => write!(f, "'{}'", ")"),
				Bracket::Left => write!(f, "'{}'", "("),
			},
			TokenContent::Name (name) => write!(f, "'{}'", name),
			TokenContent::BuiltinName (name) => write!(f, "'{}'", name),
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
			TokenContent::Operator (op1) => match other {
				TokenContent::Operator (op2) => op1 == op2,
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
			TokenContent::BuiltinName (n1) => match other {
				TokenContent::BuiltinName (n2) => n1 == n2,
				_ => false,
			},
		}
	}
}
impl Eq for TokenContent {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
	Plus,
	Minus,
	Mul,
	Pow,
	Div,
	Equals,
	Assign,
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

#[derive(Debug, Clone, PartialEq, Eq)]
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
	pub fn can_parse_tokens() {
		let test_token_content_detection = |code: &str, tc: TokenContent| {
			let mut tokens_iter = TokensIter::new(CharsIter::new(code));
			assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), tc);
			let end_pos: usize = code.len();
			assert_eq!(tokens_iter.next_or_err().unwrap_err(), TokenErr::EndReached { pos: end_pos });
		};
		
		test_token_content_detection("123", TokenContent::Number (123_f32));
		test_token_content_detection("123.456", TokenContent::Number (123.456_f32));
		test_token_content_detection("123.", TokenContent::Number (123_f32));
		test_token_content_detection(".456", TokenContent::Number (0.456_f32));
		test_token_content_detection("+", TokenContent::Operator (Operator::Plus));
		test_token_content_detection("-", TokenContent::Operator (Operator::Minus));
		test_token_content_detection("*", TokenContent::Operator (Operator::Mul));
		test_token_content_detection("/", TokenContent::Operator (Operator::Div));
		test_token_content_detection("==", TokenContent::Operator (Operator::Equals));
		test_token_content_detection("(", TokenContent::Bracket (Bracket::Left));
		test_token_content_detection(")", TokenContent::Bracket (Bracket::Right));
		test_token_content_detection("=", TokenContent::Operator (Operator::Assign));
		test_token_content_detection("var1", TokenContent::Name (String::from("var1")));
		test_token_content_detection(":", TokenContent::StatementOp (StatementOp::Colon));
		test_token_content_detection(";", TokenContent::StatementOp (StatementOp::Semicolon));
		test_token_content_detection(",", TokenContent::StatementOp (StatementOp::Comma));
		test_token_content_detection("var", TokenContent::Keyword ( Keyword::Var ));
		test_token_content_detection("@print", TokenContent::BuiltinName (String::from("print")));
	}

	#[test]
	pub fn can_parse_simple_expr() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("1+23.4-45.6*7.8/9 var1var"));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Number (1_f32));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::Plus));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Number (23.4_f32));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::Minus));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Number (45.6_f32));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::Mul));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Number (7.8_f32));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::Div));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Number (9_f32));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Name (String::from("var1var")));
		assert_eq!(tokens_iter.next_or_err().unwrap_err(), TokenErr::EndReached { pos: 25 });
	}

	#[test]
	pub fn can_parse_simple_expr_with_whitespaces() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("1\t+  23.4 \n-  45.6\n\n *7.8  / \t\t9 \n var1 var"));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Number (1_f32));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::Plus));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Number (23.4_f32));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::Minus));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Number (45.6_f32));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::Mul));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Number (7.8_f32));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::Div));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Number (9_f32));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Name (String::from("var1")));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Keyword ( Keyword::Var ));
		assert_eq!(tokens_iter.next_or_err().unwrap_err(), TokenErr::EndReached { pos: 43 });
	}
}