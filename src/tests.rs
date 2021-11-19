use super::{ parser::* };

#[test]
pub fn can_parse_int() {
	let parser = Parser::new("123").unwrap();
	let mut tokens_iter = parser.tokens().iter();
	assert_eq!(*tokens_iter.next().unwrap(), Token::Int (123_u32));
	assert_eq!(tokens_iter.next(), None);
}

#[test]
pub fn can_parse_float() {
	let parser = Parser::new("123.456").unwrap();
	let mut tokens_iter = parser.tokens().iter();
	assert_eq!(*tokens_iter.next().unwrap(), Token::Float (123.456_f32));
	assert_eq!(tokens_iter.next(), None);
}

#[test]
pub fn can_parse_plus() {
	let parser = Parser::new("+").unwrap();
	let mut tokens_iter = parser.tokens().iter();
	assert_eq!(*tokens_iter.next().unwrap(), Token::Plus);
	assert_eq!(tokens_iter.next(), None);
}

#[test]
pub fn can_parse_minus() {
	let parser = Parser::new("-").unwrap();
	let mut tokens_iter = parser.tokens().iter();
	assert_eq!(*tokens_iter.next().unwrap(), Token::Minus);
	assert_eq!(tokens_iter.next(), None);
}

#[test]
pub fn can_parse_mul() {
	let parser = Parser::new("*").unwrap();
	let mut tokens_iter = parser.tokens().iter();
	assert_eq!(*tokens_iter.next().unwrap(), Token::Mul);
	assert_eq!(tokens_iter.next(), None);
}

#[test]
pub fn can_parse_div() {
	let parser = Parser::new("/").unwrap();
	let mut tokens_iter = parser.tokens().iter();
	assert_eq!(*tokens_iter.next().unwrap(), Token::Div);
	assert_eq!(tokens_iter.next(), None);
}

#[test]
pub fn can_parse_simple_expr() {
	let parser = Parser::new("1+23.4-45.6*7.8/9").unwrap();
	let mut tokens_iter = parser.tokens().iter();
	assert_eq!(*tokens_iter.next().unwrap(), Token::Int (1_u32));
	assert_eq!(*tokens_iter.next().unwrap(), Token::Plus);
	assert_eq!(*tokens_iter.next().unwrap(), Token::Float (23.4_f32));
	assert_eq!(*tokens_iter.next().unwrap(), Token::Minus);
	assert_eq!(*tokens_iter.next().unwrap(), Token::Float (45.6_f32));
	assert_eq!(*tokens_iter.next().unwrap(), Token::Mul);
	assert_eq!(*tokens_iter.next().unwrap(), Token::Float (7.8_f32));
	assert_eq!(*tokens_iter.next().unwrap(), Token::Div);
	assert_eq!(*tokens_iter.next().unwrap(), Token::Int (9_u32));
	assert_eq!(tokens_iter.next(), None);
}

#[test]
pub fn can_parse_simple_expr_with_whitespaces() {
	let parser = Parser::new("1\t+  23.4 \n-  45.6\n\n *7.8  / \t\t9").unwrap();
	let mut tokens_iter = parser.tokens().iter();
	assert_eq!(*tokens_iter.next().unwrap(), Token::Int (1_u32));
	assert_eq!(*tokens_iter.next().unwrap(), Token::Plus);
	assert_eq!(*tokens_iter.next().unwrap(), Token::Float (23.4_f32));
	assert_eq!(*tokens_iter.next().unwrap(), Token::Minus);
	assert_eq!(*tokens_iter.next().unwrap(), Token::Float (45.6_f32));
	assert_eq!(*tokens_iter.next().unwrap(), Token::Mul);
	assert_eq!(*tokens_iter.next().unwrap(), Token::Float (7.8_f32));
	assert_eq!(*tokens_iter.next().unwrap(), Token::Div);
	assert_eq!(*tokens_iter.next().unwrap(), Token::Int (9_u32));
	assert_eq!(tokens_iter.next(), None);
}