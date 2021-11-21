use super::*;
use super::super::chars_iter::*;
use super::super::tokens_iter::*;
use super::super::statements_iter::*;
use super::Expr;

#[test]
pub fn can_make_variable_declare_init_statement() {
	let tokens_iter = TokensIter::new(CharsIter::new("var a: f32 = 0.3 + 0.5;"));	
	let mut statements_iter = StatementsIter::new(tokens_iter);
	
	let st = statements_iter.next().unwrap().unwrap();
	assert_eq!(st, Statement::WithVariable ( 
			WithVariable::DeclareSet {
				var_name: String::from("a"), 
				var_type: VarType::Float32, 
				value_expr: Expr {
					node_stack: NodeStack {
						inner: vec![
							Node::Number (0.3),
							Node::Number (0.5),
							Node::ArithmeticalOp (RankedArithmeticalOp { op: ArithmeticalOp::Plus, rank: 1_u32 }),
						]
					}
				},
			} 
		) 
	);
	
	assert!(statements_iter.next().unwrap().is_none());
}

#[test]
pub fn can_make_variable_declare_statement() {
	let tokens_iter = TokensIter::new(CharsIter::new("var a: f32;"));	
	let mut statements_iter = StatementsIter::new(tokens_iter);
	
	let st = statements_iter.next().unwrap().unwrap();
	assert_eq!(st, Statement::WithVariable ( 
			WithVariable::Declare {
				var_name: String::from("a"), 
				var_type: VarType::Float32, 
			} 
		) 
	);
	
	assert!(statements_iter.next().unwrap().is_none());
}

#[test]
pub fn can_make_variable_set_statement() {
	let tokens_iter = TokensIter::new(CharsIter::new("a = 0.3 + 0.5;"));	
	let mut statements_iter = StatementsIter::new(tokens_iter);
	
	let st = statements_iter.next().unwrap().unwrap();
	assert_eq!(st, Statement::WithVariable ( 
			WithVariable::Set {
				var_name: String::from("a"), 
				value_expr: Expr {
					node_stack: NodeStack {
						inner: vec![
							Node::Number (0.3),
							Node::Number (0.5),
							Node::ArithmeticalOp (RankedArithmeticalOp { op: ArithmeticalOp::Plus, rank: 1_u32 }),
						]
					}
				},
			} 
		) 
	);
	
	assert!(statements_iter.next().unwrap().is_none());
}

#[test]
pub fn can_calc_expression_without_variables() {
	let mut tokens_iter = TokensIter::new(CharsIter::new("33 + (1 + 2 * (3 + 4) + 5) / 10 - 30"));
	
	let expr = Expr::new(&mut tokens_iter).unwrap();
	
	assert!((expr.calc().unwrap() - 5.0_f32).abs() <= std::f32::EPSILON);
}

#[test]
pub fn can_parse_expression_with_variables() {
	let mut tokens_iter = TokensIter::new(CharsIter::new("a + 2 + b / c"));
	
	let expr = Expr::new(&mut tokens_iter).unwrap();
	assert_eq!(
		expr, 
		Expr {
			node_stack: NodeStack {
				inner: vec![
					Node::Variable { name: String::from("a") },
					Node::Number (2_f32),
					Node::ArithmeticalOp (RankedArithmeticalOp { op: ArithmeticalOp::Plus, rank: 1_u32 }),
					Node::Variable { name: String::from("b") },
					Node::Variable { name: String::from("c") },
					Node::ArithmeticalOp (RankedArithmeticalOp { op: ArithmeticalOp::Div, rank: 2_u32 }),
					Node::ArithmeticalOp (RankedArithmeticalOp { op: ArithmeticalOp::Plus, rank: 1_u32 }),
				]
			}
		},
	);
}