use super::memory::*;
use super::string_char::*;
use super::token::*;
use super::statement::*;
use super::arithmetic_expr::*;
use super::var_data::*;
use super::func_data::*;

#[test]
pub fn can_make_variable_declare_statement() {
	let tokens_iter = TokensIter::new(CharsIter::new("var a: f32;"));	
	let mut statements_iter = StatementsIter::new(tokens_iter);
	
	let st = statements_iter.next().unwrap().unwrap();
	assert_eq!(st, Statement::WithVariable ( 
			WithVariable::Declare {
				var_name: String::from("a"), 
				data_type: DataType::Float32, 
			} 
		) 
	);
	
	assert!(statements_iter.next().is_none());
}

#[test]
pub fn can_calc_expression_without_variables() {
	let mut tokens_iter = TokensIter::new(CharsIter::new("33 + (1 + 2 * (3 + 4) + 5) / 10 - 30;"));
	
	let expr = ArithmeticExpr::new(
		&mut tokens_iter,
		ExprContextKind::FunctionArg).unwrap();
	
	let memory = Memory::new();
	
	let value_res = expr.calc(&memory).unwrap();
	
	match value_res {
		VarValue::Float32(res) => assert!((res - 5.0_f32).abs() <= std::f32::EPSILON),
		_ => panic!("Test failed: {:?}", value_res),
	}
}
