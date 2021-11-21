use super::chars_iter::*;
use super::tokens_iter::*;

#[test]
pub fn can_parse_int() {
	let mut tokens_iter = TokensIter::new(CharsIter::new("123"));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::Number (123_f32));
	assert!(tokens_iter.next().unwrap().is_none());
}

#[test]
pub fn can_parse_float() {
	let mut tokens_iter = TokensIter::new(CharsIter::new("123.456"));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::Number (123.456_f32));
	assert!(tokens_iter.next().unwrap().is_none());
}

#[test]
pub fn can_parse_plus() {
	let mut tokens_iter = TokensIter::new(CharsIter::new("+"));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Plus));
	assert!(tokens_iter.next().unwrap().is_none());
}

#[test]
pub fn can_parse_minus() {
	let mut tokens_iter = TokensIter::new(CharsIter::new("-"));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Minus));
	assert!(tokens_iter.next().unwrap().is_none());
}

#[test]
pub fn can_parse_mul() {
	let mut tokens_iter = TokensIter::new(CharsIter::new("*"));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Mul));
	assert!(tokens_iter.next().unwrap().is_none());
}

#[test]
pub fn can_parse_div() {
	let mut tokens_iter = TokensIter::new(CharsIter::new("/"));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Div));
	assert!(tokens_iter.next().unwrap().is_none());
}

#[test]
pub fn can_parse_name() {
	let mut tokens_iter = TokensIter::new(CharsIter::new("var1"));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::Name (String::from("var1")));
	assert!(tokens_iter.next().unwrap().is_none());
}

#[test]
pub fn can_parse_keyword() {
	let mut tokens_iter = TokensIter::new(CharsIter::new("var"));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::Keyword ( Keyword::Var ));
	assert!(tokens_iter.next().unwrap().is_none());
}

#[test]
pub fn can_parse_simple_expr() {
	let mut tokens_iter = TokensIter::new(CharsIter::new("1+23.4-45.6*7.8/9 var1var"));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::Number (1_f32));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Plus));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::Number (23.4_f32));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Minus));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::Number (45.6_f32));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Mul));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::Number (7.8_f32));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Div));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::Number (9_f32));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::Name (String::from("var1var")));
	assert!(tokens_iter.next().unwrap().is_none());
}

#[test]
pub fn can_parse_simple_expr_with_whitespaces() {
	let mut tokens_iter = TokensIter::new(CharsIter::new("1\t+  23.4 \n-  45.6\n\n *7.8  / \t\t9 \n var1 var"));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::Number (1_f32));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Plus));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::Number (23.4_f32));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Minus));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::Number (45.6_f32));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Mul));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::Number (7.8_f32));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::ArithmeticalOp (ArithmeticalOp::Div));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::Number (9_f32));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::Name (String::from("var1")));
	assert_eq!(tokens_iter.next().unwrap().unwrap(), Token::Keyword ( Keyword::Var ));
	assert!(tokens_iter.next().unwrap().is_none());
}

#[test]
pub fn can_calc_expression() {
	let mut tokens_iter = TokensIter::new(CharsIter::new("33 + (1 + 2 * (3 + 4) + 5) / 10 - 30"));
	
	let expr = super::expr::Expr::new(&mut tokens_iter).unwrap();
	
	assert!((expr.calc().unwrap() - 5.0_f32).abs() <= std::f32::EPSILON);
}

#[test]
pub fn can_make_variable_declare_init_statement() {
	let tokens_iter = TokensIter::new(CharsIter::new("var a: f32 = 0.3 + 0.5;"));	
	let mut statements_iter = StatementsIter::new(tokens_iter);
	
	use super::statements_iter::*;
	let st = statements_iter.next().unwrap().unwrap();
	assert_eq!(st, Statement::WithVariable ( 
			WithVariable::DeclareInitialize {
				var_name: String::from("a"), 
				var_type: VarType::Float32, 
				value: VarValue::Float32(0.8),
			} 
		) 
	);
	
	assert!(statements_iter.next().unwrap().is_none());
}