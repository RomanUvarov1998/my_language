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
	
	pub fn pos(&self) -> CharPos {
		self.iter.last_pos()
	}
	
	pub fn peek(&mut self) -> Result<Option<&Token>, TokenErr> {
		if self.peeked_token.is_none() {
			if let Some(token_result) = self.next() {
				self.peeked_token = Some(token_result?);
			}
		}
		Ok(self.peeked_token.as_ref())
	}
	
	pub fn peek_or_end_reached_err(&mut self) -> Result<&Token, TokenErr> {
		if self.peeked_token.is_none() {
			self.peeked_token = Some(self.next_or_end_reached_err()?);
		}
		Ok(self.peeked_token.as_ref().unwrap())
	}
	
	pub fn next_or_end_reached_err(&mut self) -> Result<Token, TokenErr> {
		match self.next() {
			Some(token_result) => token_result,
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
	
	pub fn next_expect_left_curly_bracket(&mut self) -> Result<(), TokenErr> {
		match self.next() {
			Some(token_result) => Self::expect(
				token_result?, 
				TokenContent::Bracket (Bracket::LeftCurly)),
			None => Err( TokenErr::EndReached { pos: self.iter.last_pos() } ),
		}
	}
	
	fn parse_number(&mut self, first_char: ParsedChar) -> Result<Token, TokenErr> {
		let mut has_dot = false;
		let mut value: f32 = match first_char.kind() {
			CharKind::Digit (first_digit) => first_digit as f32,
			CharKind::Dot => {
				has_dot = true;
				0_f32
			},
			_ => panic!("Wrong input: {:?}", first_char),
		};
		let mut frac_multiplier = 0.1_f32;
		let mut pos_end: CharPos = first_char.pos();
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
						_ => break Ok( Token::new(first_char.pos(), pos_end, TokenContent::Number (value) )),
					};
					pos_end = parsed_char.pos();
				},
				None => break Ok( Token::new(first_char.pos(), pos_end, TokenContent::Number (value) )),
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
					CharKind::Greater |
					CharKind::Less |
					CharKind::Asterisk |
					CharKind::Exclamation |
					CharKind::Circumflex |
					CharKind::LeftSlash |
					CharKind::LeftBracket |
					CharKind::RightBracket |
					CharKind::LeftCurlyBracket |
					CharKind::RightCurlyBracket |
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

	fn parse_name_or_keyword_or_operator(&mut self, first_char: ParsedChar) -> Result<Token, TokenErr> {
		let is_builtin: bool;
		let mut name = if first_char.kind() == CharKind::Dog {
			is_builtin = true;
			String::new()
		} else {
			is_builtin = false;
			String::from(first_char.ch())
		};
		
		let mut pos_end: CharPos = first_char.pos();
		
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
			"var" => Token::new(first_char.pos(), pos_end, TokenContent::Keyword ( Keyword::Var )),
			"if" => Token::new(first_char.pos(), pos_end, TokenContent::Keyword ( Keyword::If )),
			"else" => Token::new(first_char.pos(), pos_end, TokenContent::Keyword ( Keyword::Else )),
			"True" => Token::new(first_char.pos(), pos_end, TokenContent::Keyword ( Keyword::True )),
			"False" => Token::new(first_char.pos(), pos_end, TokenContent::Keyword ( Keyword::False )),
			"land" => Token::new(first_char.pos(), pos_end, TokenContent::Operator ( Operator::LogicalAnd )),
			"lor" => Token::new(first_char.pos(), pos_end, TokenContent::Operator ( Operator::LogicalOr )),
			"lxor" => Token::new(first_char.pos(), pos_end, TokenContent::Operator ( Operator::LogicalXor )),
			_ => if is_builtin {
					Token::new(first_char.pos(), pos_end, TokenContent::BuiltinName (name))
				} else {
					Token::new(first_char.pos(), pos_end, TokenContent::Name (name))
				},
		};
		
		Ok( token )
	}

	fn parse_operator_or_comment(&mut self, first_char: ParsedChar) -> Result<Token, TokenErr> {
		match first_char.kind() {
			CharKind::Plus => Ok( Token::new(first_char.pos(), first_char.pos(), TokenContent::Operator ( Operator::Plus )) ),
			CharKind::Minus => Ok( Token::new(first_char.pos(), first_char.pos(), TokenContent::Operator ( Operator::Minus )) ),
			CharKind::Asterisk => Ok( Token::new(first_char.pos(), first_char.pos(), TokenContent::Operator ( Operator::Mul )) ),
			CharKind::Circumflex => Ok( Token::new(first_char.pos(), first_char.pos(), TokenContent::Operator ( Operator::Pow )) ),

			CharKind::LeftSlash => match self.iter.peek() {
				Some(second_char) => match second_char.kind() {
					CharKind::LeftSlash => {
						let second_char: ParsedChar = self.iter.next().unwrap();
						
						let mut content = String::new();
						
						let mut last_char: ParsedChar = second_char;
						while let Some(ch) = self.iter.next() {
							match ch.kind() {
								CharKind::NewLine => break,
								//CharKind::Invalid => return Err( TokenErr::Construct (ch) ),
								_ => {
									content.push(ch.ch());
									last_char = ch;
								},
							}
						};
						
						Ok( Token::new(first_char.pos(), last_char.pos(), TokenContent::StatementOp ( StatementOp::Comment (content) )) )
					},
					_ => Ok( Token::new(first_char.pos(), first_char.pos(), TokenContent::Operator ( Operator::Div )) ),
				},
				None => Ok( Token::new(first_char.pos(), first_char.pos(), TokenContent::Operator ( Operator::Div )) ),
			},
			
			CharKind::Exclamation => match self.iter.peek() {
				Some(second_char) => match second_char.kind() {
					CharKind::Eq => {
						let second_char: ParsedChar = self.iter.next().unwrap();
						Ok( Token::new(first_char.pos(), second_char.pos(), TokenContent::Operator ( Operator::NotEqual )) )
					},
					_ => Ok( Token::new(first_char.pos(), first_char.pos(), TokenContent::Operator ( Operator::Not )) ),
				},
				None => Ok( Token::new(first_char.pos(), first_char.pos(), TokenContent::Operator ( Operator::Not )) ),
			},
			
			CharKind::Greater => match self.iter.peek() {
				Some(second_char) => match second_char.kind() {
					CharKind::Eq => {
						self.iter.next().unwrap();
						Ok( Token::new(
							first_char.pos(), 
							second_char.pos(), 
							TokenContent::Operator (Operator::GreaterEqual)) )
					},
					_ => Ok( Token::new(
						first_char.pos(), 
						second_char.pos(), 
						TokenContent::Operator (Operator::Greater)) ),
				},
				None => Ok( Token::new(
					first_char.pos(), 
					first_char.pos(), 
					TokenContent::Operator (Operator::Greater)) ),
			},
			
			CharKind::Less => match self.iter.peek() {
				Some(second_char) => match second_char.kind() {
					CharKind::Eq => {
						self.iter.next().unwrap();
						Ok( Token::new(
							first_char.pos(), 
							second_char.pos(), 
							TokenContent::Operator (Operator::LessEqual)) )
					},
					_ => Ok( Token::new(
						first_char.pos(), 
						second_char.pos(), 
						TokenContent::Operator (Operator::Less)) ),
				},
				None => Ok( Token::new(
					first_char.pos(), 
					first_char.pos(), 
					TokenContent::Operator (Operator::Less)) ),
			},
			
			CharKind::Eq => match self.iter.peek() {
				Some(parsed_char) => {					
					match parsed_char.kind() {
						CharKind::Eq => {
							self.iter.next().unwrap(); 
							Ok( Token::new(first_char.pos(), parsed_char.pos(), TokenContent::Operator (Operator::Equal)) )
						},
						_ => Ok( Token::new(first_char.pos(), first_char.pos(), TokenContent::Operator (Operator::Assign)) ),
					}
				},
				None => Ok( Token::new(first_char.pos(), first_char.pos(), TokenContent::Operator (Operator::Assign)) ),
			}
			
			_ => panic!("Unexpected input: {:?}", first_char),
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
}

impl Iterator for TokensIter {
	type Item = Result<Token, TokenErr>;
	
	fn next(&mut self) -> Option<Self::Item> {
		if self.peeked_token.is_some() {
			return Some( Ok( self.peeked_token.take().unwrap() ) );
		}
		
		for ch in self.iter.by_ref() {			
			let token_result = match ch.kind() {
				CharKind::Digit (_) | CharKind::Dot => self.parse_number(ch),
				
				CharKind::Plus | CharKind::Minus 
					| CharKind::Greater | CharKind::Less
					| CharKind::Asterisk
					| CharKind::Circumflex 
					| CharKind::LeftSlash
					| CharKind::Eq
					| CharKind::Exclamation
						=> self.parse_operator_or_comment(ch),
						
				CharKind::DoubleQuote => self.parse_string_literal(ch.pos()),
				
				CharKind::LeftBracket => Ok( Token::new(ch.pos(), ch.pos(), TokenContent::Bracket ( Bracket::Left )) ),
				CharKind::RightBracket => Ok( Token::new(ch.pos(), ch.pos(), TokenContent::Bracket ( Bracket::Right )) ),
				
				CharKind::LeftCurlyBracket => Ok( Token::new(ch.pos(), ch.pos(), TokenContent::Bracket ( Bracket::LeftCurly )) ),
				CharKind::RightCurlyBracket => Ok( Token::new(ch.pos(), ch.pos(), TokenContent::Bracket ( Bracket::RightCurly )) ),
				
				CharKind::Letter 
					| CharKind::Dog 
						=> self.parse_name_or_keyword_or_operator(ch),
				
				CharKind::Whitespace 
					| CharKind::Control 
					| CharKind::NewLine 
						=> continue,
				
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

//---------------------------- Token --------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
	pub pos_begin: CharPos,
	pub pos_end: CharPos,
	pub content: TokenContent,
}

impl Token {
	pub fn new(pos_begin: CharPos, pos_end: CharPos, content: TokenContent) -> Self {
		Self { pos_begin, pos_end, content }
	}
	
	pub fn pos_begin(&self) -> CharPos { self.pos_begin }
	pub fn pos_end(&self) -> CharPos { self.pos_end }
	
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
				Operator::Equal => write!(f, "'{}'", "=="),
				Operator::Assign => write!(f, "'{}'", "="),
				Operator::NotEqual => write!(f, "'{}'", "!="),
				Operator::Not => write!(f, "'{}'", "!"),
				Operator::Greater => write!(f, "'{}'", ">"),
				Operator::GreaterEqual => write!(f, "'{}'", ">="),
				Operator::Less => write!(f, "'{}'", "<"),
				Operator::LessEqual => write!(f, "'{}'", "<="),
				Operator::LogicalAnd => write!(f, "'{}'", "LogicalAnd"),
				Operator::LogicalOr => write!(f, "'{}'", "LogicalOr"),
				Operator::LogicalXor => write!(f, "'{}'", "LogicalXor"),
			},
			TokenContent::Bracket (br) => match br {
				Bracket::Left => write!(f, "'{}'", "("),
				Bracket::Right => write!(f, "'{}'", ")"),
				Bracket::LeftCurly => write!(f, "'{}'", "{{"),
				Bracket::RightCurly => write!(f, "'{}'", "}}"),
			},
			TokenContent::Name (name) => write!(f, "'{}'", name),
			TokenContent::BuiltinName (name) => write!(f, "'{}'", name),
			TokenContent::StatementOp (op) => match op {
				StatementOp::Colon => write!(f, "'{}'", ":"),
				StatementOp::Semicolon => write!(f, "'{}'", ";"),
				StatementOp::Comma => write!(f, "'{}'", ","),
				StatementOp::Comment (content) => write!(f, "Comment //'{}'", content),
			},
			TokenContent::Keyword (kw) => match kw {
				Keyword::Var => write!(f, "'{}'", "var"),
				Keyword::If => write!(f, "'{}'", "if"),
				Keyword::Else => write!(f, "'{}'", "else"),
				Keyword::True => write!(f, "'{}'", "True"),
				Keyword::False => write!(f, "'{}'", "False"),
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
	Equal,
	Assign,
	Not,
	NotEqual,
	Greater,
	GreaterEqual,
	Less,
	LessEqual,
	LogicalAnd,
	LogicalOr,
	LogicalXor,
}

//---------------------------- Extra... --------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Bracket {
	Left,
	Right,
	LeftCurly,
	RightCurly,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementOp {
	Colon,
	Semicolon,
	Comma,
	Comment (String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
	Var,
	If,
	Else,
	True,
	False,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenErr {
	Construct (ParsedChar),
	ExpectedButFound { 
		expected: Vec<TokenContent>, 
		found: Token 
	},
	EndReached { pos: CharPos },
}

impl std::fmt::Display for TokenErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			TokenErr::Construct (parsed_char) => write!(f, "Unexpected character '{}'", parsed_char.ch()),
			TokenErr::ExpectedButFound { expected, found } => {				
				writeln!(f, "Expected: ")?;
				for tc in expected {
					writeln!(f, "\t{}", tc)?;
				}
				write!(f, "But found {}", found.content())
			},
			TokenErr::EndReached { .. } => write!(f, "Unexpected end"),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	
	#[test]
	pub fn can_parse_single_tokens() {
		let test_token_content_detection = |code: &str, tc: TokenContent| {
			let mut tokens_iter = TokensIter::new();
			tokens_iter.push_string(code.to_string());
						
			assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), tc);
			
			match tokens_iter.next_or_end_reached_err().unwrap_err() {
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
		test_token_content_detection("==", TokenContent::Operator (Operator::Equal));
		test_token_content_detection("(", TokenContent::Bracket (Bracket::Left));
		test_token_content_detection(")", TokenContent::Bracket (Bracket::Right));
		test_token_content_detection("{", TokenContent::Bracket (Bracket::LeftCurly));
		test_token_content_detection("}", TokenContent::Bracket (Bracket::RightCurly));
		test_token_content_detection("=", TokenContent::Operator (Operator::Assign));
		test_token_content_detection("var1", TokenContent::Name (String::from("var1")));
		test_token_content_detection(":", TokenContent::StatementOp (StatementOp::Colon));
		test_token_content_detection(";", TokenContent::StatementOp (StatementOp::Semicolon));
		test_token_content_detection(",", TokenContent::StatementOp (StatementOp::Comma));
		test_token_content_detection("//23rwrer2", TokenContent::StatementOp (StatementOp::Comment (String::from("23rwrer2"))));
		test_token_content_detection("var", TokenContent::Keyword ( Keyword::Var ));
		test_token_content_detection("if", TokenContent::Keyword ( Keyword::If ));
		test_token_content_detection("else", TokenContent::Keyword ( Keyword::Else ));
		test_token_content_detection("@print", TokenContent::BuiltinName (String::from("print")));
		test_token_content_detection("\"vasya\"", TokenContent::StringLiteral (String::from("vasya")));
		test_token_content_detection(">", TokenContent::Operator (Operator::Greater));
		test_token_content_detection(">=", TokenContent::Operator (Operator::GreaterEqual));
		test_token_content_detection("<", TokenContent::Operator (Operator::Less));
		test_token_content_detection("<=", TokenContent::Operator (Operator::LessEqual));
		test_token_content_detection("!=", TokenContent::Operator (Operator::NotEqual));
		test_token_content_detection("!", TokenContent::Operator (Operator::Not));
		test_token_content_detection("land", TokenContent::Operator (Operator::LogicalAnd));
		test_token_content_detection("lor", TokenContent::Operator (Operator::LogicalOr));
		test_token_content_detection("lxor", TokenContent::Operator (Operator::LogicalXor));
	}

	#[test]
	pub fn can_parse_multiple_tokens() {
		let mut tokens_iter = TokensIter::new();
		tokens_iter.push_string("1+23.4-45.6*7.8/9 var1var\"vasya\">>=<<===!=!land lor lxor:;,(){}if else//sdsdfd".to_string());
		
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
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::StringLiteral (String::from("vasya")));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::Greater));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::GreaterEqual));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::Less));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::LessEqual));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::Equal));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::NotEqual));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::Not));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::LogicalAnd));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::LogicalOr));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::LogicalXor));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::StatementOp (StatementOp::Colon));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::StatementOp (StatementOp::Semicolon));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::StatementOp (StatementOp::Comma));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Bracket (Bracket::Left));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Bracket (Bracket::Right));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Bracket (Bracket::LeftCurly));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Bracket (Bracket::RightCurly));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Keyword ( Keyword::If ));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Keyword ( Keyword::Else ));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::StatementOp (StatementOp::Comment (String::from("sdsdfd"))));
		assert_eq!(tokens_iter.next(), None);
	}

	#[test]
	pub fn can_parse_multiple_tokens_with_whitespaces() {
		let mut tokens_iter = TokensIter::new();
		tokens_iter.push_string("1 \n\t + \n\t 23.4 \n\t - \n\t 45.6 \n\t *7.8 \n\t / \n\t 9 \n\t var1 \n\t var \n\t \"vasya\" \n\t > \n\t >= < \n\t <= \n\t == \n\t != \n\t ! \n\t land \n\t lor \n\t lxor \n\t : \n\t ; \n\t , \n\t (  \n\t ) \n\t { \n\t } \n\t if \n\t else \n\t // sdfsdfs \n\t ".to_string());
		
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
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::StringLiteral (String::from("vasya")));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::Greater));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::GreaterEqual));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::Less));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::LessEqual));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::Equal));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::NotEqual));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::Not));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::LogicalAnd));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::LogicalOr));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Operator (Operator::LogicalXor));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::StatementOp (StatementOp::Colon));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::StatementOp (StatementOp::Semicolon));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::StatementOp (StatementOp::Comma));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Bracket (Bracket::Left));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Bracket (Bracket::Right));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Bracket (Bracket::LeftCurly));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Bracket (Bracket::RightCurly));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Keyword ( Keyword::If ));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::Keyword ( Keyword::Else ));
		assert_eq!(*tokens_iter.next().unwrap().unwrap().content(), TokenContent::StatementOp (StatementOp::Comment (String::from(" sdfsdfs "))));
		assert_eq!(tokens_iter.next(), None);
	}
}