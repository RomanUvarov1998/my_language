use super::string_char::{ CharsIter, CharPos, CharKind, Punctuation, ParsedChar };
use std::collections::VecDeque;

//---------------------------- TokensIter --------------------------------

#[derive(Debug)]
pub struct TokensIter {
	iter: CharsIter,
	peeked_token: Option<Token>,
	cached_queue: VecDeque<Token>,
}

impl TokensIter {
	pub fn new() -> Self {
		Self { 
			iter: CharsIter::new(),
			peeked_token: None,
			cached_queue: VecDeque::new(),
		}
	}
	
	pub fn push_string(&mut self, text: String) {
		self.iter.push_string(text);
	}
	
	pub fn cache_until_semicolon(&mut self) -> Result<bool, TokenErr> { 
		loop {
			match self.parse_next_token() {
				Some(Ok(token)) => {
					self.cached_queue.push_back(token);
					
					match self.cached_queue.back().unwrap() {
						Token { content: TokenContent::StatementOp (StatementOp::Semicolon), .. } 
							=> return Ok(true),
						_ => {},
					}
				},
				
				Some(Err(err)) => return Err(TokenErr::from(err)),
				
				None => return Ok(false),
			}
		}
	}
	
	pub fn cached_queue_is_empty(&self) -> bool {
		self.cached_queue.is_empty()
	}
	
	pub fn clear_cached_queue(&mut self) {
		self.cached_queue.clear();
	}
	
	pub fn peek_or_err(&mut self) -> Result<&Token, TokenErr> {
		if self.peeked_token.is_none() {
			self.peeked_token = Some(self.next_or_err()?);
		}
		Ok(self.peeked_token.as_ref().unwrap())
	}
	
	pub fn next_or_err(&mut self) -> Result<Token, TokenErr> {
		match self.next() {
			Some(token_result) => token_result,
			None => Err( TokenErr::EndReached { pos: self.iter.last_pos() } ),
		}
	}
	
	pub fn next_name_or_err(&mut self) -> Result<String, TokenErr> {
		match self.next() {
			Some(token_result) => match token_result? {
				Token { content: TokenContent::Name (name), .. } => Ok(name),
				found @ _ => Err( TokenErr::ExpectedButFound {
							expected: vec![TokenContent::Name ("<name>".to_string())], 
							found,
						} )
			},
			None => Err( TokenErr::EndReached { pos: self.iter.last_pos() } ),
		}
	}
	
	pub fn next_expect_semicolon(&mut self) -> Result<(), TokenErr> {
		match self.next() {
			Some(token_result) => Self::expect(
				token_result?, 
				TokenContent::StatementOp (StatementOp::Semicolon)),
			None => Err( TokenErr::EndReached { pos: self.iter.last_pos() } ),
		}
	}
	
	pub fn next_expect_colon(&mut self) -> Result<(), TokenErr> {
		match self.next() {
			Some(token_result) => Self::expect(
				token_result?, 
				TokenContent::StatementOp (StatementOp::Colon)),
			None => Err( TokenErr::EndReached { pos: self.iter.last_pos() } ),
		}
	}
	
	fn parse_number(&mut self, pos_begin: CharPos, first_digit: u32, already_encountered_dot : bool) -> Result<Token, TokenErr> {
		let mut value: f32 = first_digit as f32;
		let mut has_dot: bool = already_encountered_dot;
		let mut frac_multiplier = 0.1_f32;
		let mut pos_end: CharPos = pos_begin;
		loop {
			match self.iter.peek() {
				Some(parsed_char) => {
					match parsed_char.kind() {
						CharKind::Digit (val) => {
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
								break Err(TokenErr::Construct (parsed_char));
							}
						},
						CharKind::Letter => break Err(TokenErr::Construct (parsed_char)),
						_ => break Ok( Token::new(pos_begin, pos_end, TokenContent::Number (value) )),
					};
					pos_end = parsed_char.pos();
				},
				None => break Ok( Token::new(pos_begin, pos_end, TokenContent::Number (value) )),
			}
		}
	}

	fn parse_string_literal(&mut self, pos_begin: CharPos) -> Result<Token, TokenErr> {
		let mut string_content = String::new();
		
		loop {
			match self.iter.peek() {
				Some(parsed_char) => match parsed_char.kind() {
					CharKind::Digit (_) |
					CharKind::Dog |
					CharKind::Dot |
					CharKind::Plus |
					CharKind::Minus |
					CharKind::Asterisk |
					CharKind::Circumflex |
					CharKind::LeftSlash |
					CharKind::LeftBracket |
					CharKind::RightBracket |
					CharKind::Eq |
					CharKind::Letter |
					CharKind::Punctuation (_) |
					CharKind::Whitespace |
					CharKind::Invalid => {
						self.iter.next().unwrap();
						string_content.push(parsed_char.ch());
					},
					
					CharKind::Control => continue,
					
					CharKind::DoubleQuote => {
						self.iter.next().unwrap();
						return Ok( Token::new(pos_begin, parsed_char.pos(), TokenContent::StringLiteral (string_content)) );
					},
					
					CharKind::NewLine => return Err( TokenErr::Construct (parsed_char) ),
				},
				None => return Err( TokenErr::EndReached { pos: self.iter.last_pos() } ),
			}
		}
	}

	fn parse_name_or_keyword(&mut self, pos_begin: CharPos, first_char: char) -> Result<Token, TokenErr> {
		let is_builtin: bool;
		let mut name = if first_char == '@' {
			is_builtin = true;
			String::new()
		} else {
			is_builtin = false;
			String::from(first_char)
		};
		
		let mut pos_end: CharPos = pos_begin;
		
		loop {
			match self.iter.peek() {
				Some(parsed_char) => {
					match parsed_char.kind() {
						CharKind::Digit (_) => {
							name.push(parsed_char.ch());
							self.iter.next(); 
						},
						CharKind::Letter => {
							name.push(parsed_char.ch());
							self.iter.next(); 
						},
						_ => break,
					};
					pos_end = parsed_char.pos();
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

	fn parse_assignment_or_equality(&mut self, pos_begin: CharPos) -> Result<Token, TokenErr> {
		let mut pos_end: CharPos = pos_begin;
		match self.iter.peek() {
			Some(parsed_char) => {
				pos_end = parsed_char.pos();
				match parsed_char.kind() {
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

	fn expect(actual_token: Token, expected_tc: TokenContent) -> Result<(), TokenErr> {
		if *actual_token.content() == expected_tc {
			Ok(())
		} else {
			Err( TokenErr::ExpectedButFound {
				expected: vec![expected_tc], 
				found: actual_token,
			} )
		}
	}

	fn parse_next_token(&mut self) -> Option<Result<Token, TokenErr>> {		
		for ch in self.iter.by_ref() {			
			let token_result = match ch.kind() {
				CharKind::Digit (first_digit) => self.parse_number(ch.pos(), first_digit, false),
				CharKind::Dot => match self.iter.peek() {
					Some(ch2) => match ch2.kind() {
						CharKind::Digit (_) => self.parse_number(ch2.pos(), 0_u32, true),
						_ => Err( TokenErr::Construct (ch2) ),
					},
					None => Err( TokenErr::Construct (ch) ),
				},
				CharKind::Plus => Ok( Token::new(ch.pos(), ch.pos(), TokenContent::Operator ( Operator::Plus )) ),
				CharKind::Minus => Ok( Token::new(ch.pos(), ch.pos(), TokenContent::Operator ( Operator::Minus )) ),
				CharKind::Asterisk => Ok( Token::new(ch.pos(), ch.pos(), TokenContent::Operator ( Operator::Mul )) ),
				CharKind::Circumflex => Ok( Token::new(ch.pos(), ch.pos(), TokenContent::Operator ( Operator::Pow )) ),
				CharKind::DoubleQuote => self.parse_string_literal(ch.pos()),
				CharKind::LeftSlash => Ok( Token::new(ch.pos(), ch.pos(), TokenContent::Operator ( Operator::Div )) ),
				CharKind::LeftBracket => Ok( Token::new(ch.pos(), ch.pos(), TokenContent::Bracket ( Bracket::Left )) ),
				CharKind::RightBracket => Ok( Token::new(ch.pos(), ch.pos(), TokenContent::Bracket ( Bracket::Right )) ),
				CharKind::Eq => self.parse_assignment_or_equality(ch.pos()),
				CharKind::Letter => self.parse_name_or_keyword(ch.pos(), ch.ch()),
				CharKind::Dog => self.parse_name_or_keyword(ch.pos(), ch.ch()),
				CharKind::Whitespace | CharKind::Control | CharKind::NewLine => continue,
				CharKind::Punctuation (p) => match p {
					Punctuation::Colon => Ok( Token::new(ch.pos(), ch.pos(), TokenContent::StatementOp (StatementOp::Colon)) ),
					Punctuation::Semicolon => Ok( Token::new(ch.pos(), ch.pos(), TokenContent::StatementOp (StatementOp::Semicolon)) ),
					Punctuation::Comma => Ok( Token::new(ch.pos(), ch.pos(), TokenContent::StatementOp (StatementOp::Comma)) ),
				},
				CharKind::Invalid => Err(TokenErr::Construct (ch) ),
			};
			
			return Some(token_result);
		}
		
		None
	}
}

impl Iterator for TokensIter {
	type Item = Result<Token, TokenErr>;
	
	fn next(&mut self) -> Option<Self::Item> {
		if self.peeked_token.is_some() {
			return Some( Ok( self.peeked_token.take().unwrap() ) );
		}
		
		if let Some(token) = self.cached_queue.pop_front() {
			return Some(Ok(token));
		}
		
		Some( Err( TokenErr::EndReached { pos: self.iter.last_pos() } ) )
	}
}

//---------------------------- Token --------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
	pub pos_begin: CharPos,
	pub pos_end: CharPos,
	pub content: TokenContent,
}

impl Token {
	fn new(pos_begin: CharPos, pos_end: CharPos, content: TokenContent) -> Self {
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

//---------------------------- TokenContent --------------------------------

#[derive(Debug, Clone)]
pub enum TokenContent {
	Number (f32),
	StringLiteral (String),
	Name (String),
	BuiltinName (String),
	Operator (Operator),
	Bracket (Bracket),
	StatementOp (StatementOp),
	Keyword (Keyword),
}

impl std::fmt::Display for TokenContent {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			TokenContent::Number (val) => write!(f, "'{}'", val),
			TokenContent::StringLiteral (val) => write!(f, "\"{}\"", val),
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
			TokenContent::StringLiteral (s1) => match other {
				TokenContent::StringLiteral (s2) => s1 == s2,
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

//---------------------------- Extra... --------------------------------

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
	Construct (ParsedChar),
	ExpectedButFound { expected: Vec<TokenContent>, found: Token },
	EndReached { pos: CharPos },
}

impl std::fmt::Display for TokenErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			TokenErr::Construct (parsed_char) => {
				super::display_error_pos(f, parsed_char.pos(), parsed_char.pos())?;
				write!(f, "Unexpected character '{}'", parsed_char.ch())
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

#[cfg(test)]
mod tests {
	use super::*;
	
	#[test]
	pub fn can_parse_tokens() {
		let test_token_content_detection = |code: &str, tc: TokenContent| {
			let mut tokens_iter = TokensIter::new();
			tokens_iter.push_string(code.to_string());
						
			assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), tc);
			
			match tokens_iter.next_or_err().unwrap_err() {
				TokenErr::EndReached { .. } => {},
				err @ _ => panic!("Unexpected err: {:?}", err),
			}
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
		test_token_content_detection("\"vasya\"", TokenContent::StringLiteral (String::from("vasya")));
	}

	#[test]
	pub fn can_parse_multiple_tokens() {
		let mut tokens_iter = TokensIter::new();
		tokens_iter.push_string("1+23.4-45.6*7.8/9 var1var\"vasya\"".to_string());
		
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::Number (1_f32));
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::Operator (Operator::Plus));
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::Number (23.4_f32));
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::Operator (Operator::Minus));
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::Number (45.6_f32));
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::Operator (Operator::Mul));
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::Number (7.8_f32));
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::Operator (Operator::Div));
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::Number (9_f32));
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::Name (String::from("var1var")));
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::StringLiteral (String::from("vasya")));
		assert_eq!(tokens_iter.parse_next_token(), None);
	}

	#[test]
	pub fn can_parse_multiple_tokens_with_whitespaces() {
		let mut tokens_iter = TokensIter::new();
		tokens_iter.push_string("1\t+  23.4 \n-  45.6\n\n *7.8  / \t\t9 \n var1 var \"vasya\"".to_string());
		
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::Number (1_f32));
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::Operator (Operator::Plus));
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::Number (23.4_f32));
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::Operator (Operator::Minus));
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::Number (45.6_f32));
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::Operator (Operator::Mul));
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::Number (7.8_f32));
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::Operator (Operator::Div));
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::Number (9_f32));
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::Name (String::from("var1")));
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::Keyword ( Keyword::Var ));
		assert_eq!(*tokens_iter.parse_next_token().unwrap().unwrap().content(), TokenContent::StringLiteral (String::from("vasya")));
		assert_eq!(tokens_iter.parse_next_token(), None);
	}

	#[test]
	pub fn cannot_cache_if_no_semicolon() {
		let mut tokens_iter = TokensIter::new();
		let code = "1\t+  23.4 \n-  45.6\n\n *7.8  / \t\t9 \n var1 var";
		tokens_iter.push_string(code.to_string());
		
		assert_eq!(tokens_iter.cached_queue_is_empty(), true);
		
		assert_eq!(tokens_iter.cache_until_semicolon().unwrap(), false);
		
		assert_eq!(tokens_iter.cached_queue_is_empty(), false);
		
		assert_eq!(tokens_iter.parse_next_token(), None);
		
		assert_eq!(*tokens_iter.next_or_err().unwrap().content(), TokenContent::Number (1_f32));
		assert_eq!(
			*tokens_iter.next_or_err().unwrap().content(), 
			TokenContent::Operator (Operator::Plus));
		assert_eq!(*tokens_iter.next_or_err().unwrap().content(), TokenContent::Number (23.4_f32));
		assert_eq!(
			*tokens_iter.next_or_err().unwrap().content(), 
			TokenContent::Operator (Operator::Minus));
		assert_eq!(*tokens_iter.next_or_err().unwrap().content(), TokenContent::Number (45.6_f32));
		assert_eq!(
			*tokens_iter.next_or_err().unwrap().content(), 
			TokenContent::Operator (Operator::Mul));
		assert_eq!(*tokens_iter.next_or_err().unwrap().content(), TokenContent::Number (7.8_f32));
		assert_eq!(
			*tokens_iter.next_or_err().unwrap().content(), 
			TokenContent::Operator (Operator::Div));
		assert_eq!(*tokens_iter.next_or_err().unwrap().content(), TokenContent::Number (9_f32));
		assert_eq!(
			*tokens_iter.next_or_err().unwrap().content(), 
			TokenContent::Name (String::from("var1")));
		assert_eq!(
			*tokens_iter.next_or_err().unwrap().content(), 
			TokenContent::Keyword ( Keyword::Var ));
		
		match tokens_iter.next_or_err().unwrap_err() {
			TokenErr::EndReached { .. } => {},
			err @ _ => panic!("Unexpected err: {:?}", err),
		}
	}

	#[test]
	pub fn caches_if_has_semicolon() {
		let mut tokens_iter = TokensIter::new();
		tokens_iter.push_string("1\t+  23.4 \n-  45.6\n\n *7.8;  / \t\t9 \n var1 var".to_string());
		
		assert_eq!(tokens_iter.cached_queue_is_empty(), true);
		
		assert_eq!(tokens_iter.cache_until_semicolon().unwrap(), true);
		
		assert_eq!(tokens_iter.cached_queue_is_empty(), false);
		
		assert_eq!(*tokens_iter.next_or_err().unwrap().content(), TokenContent::Number (1_f32));
		assert_eq!(
			*tokens_iter.next_or_err().unwrap().content(), 
			TokenContent::Operator (Operator::Plus));
		assert_eq!(*tokens_iter.next_or_err().unwrap().content(), TokenContent::Number (23.4_f32));
		assert_eq!(
			*tokens_iter.next_or_err().unwrap().content(), 
			TokenContent::Operator (Operator::Minus));
		assert_eq!(*tokens_iter.next_or_err().unwrap().content(), TokenContent::Number (45.6_f32));
		assert_eq!(
			*tokens_iter.next_or_err().unwrap().content(), 
			TokenContent::Operator (Operator::Mul));
		assert_eq!(*tokens_iter.next_or_err().unwrap().content(), TokenContent::Number (7.8_f32));
		assert_eq!(
			*tokens_iter.next_or_err().unwrap().content(), 
			TokenContent::StatementOp (StatementOp::Semicolon));
	}
}