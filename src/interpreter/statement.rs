use super::token::*;
use super::arithmetic_expr::{ArithmeticExpr, ExprContextKind};
use super::{ InterpErr, var_data::DataType };

pub struct StatementsIter {
	tokens_iter: TokensIter,
}

impl StatementsIter {
	pub fn new() -> Self {
		StatementsIter { 
			tokens_iter: TokensIter::new(),
		}
	}
	
	pub fn push_string(&mut self, text: String) {
		self.tokens_iter.push_string(text);
	}	
	
	fn parse_variable_set_or_func_call(&mut self, name: String, is_builtin: bool) -> Result<Statement, InterpErr> {
		let second = self.tokens_iter.next_or_err()?;
		let statement = match second {
			Token { content: TokenContent::Bracket(Bracket::Left), .. } => 
				self.parse_func_call(name, is_builtin)?,
				
			Token { content: TokenContent::Operator (Operator::Assign), .. } => 
				self.parse_variable_set(name, is_builtin)?,
			_ => return Err( InterpErr::Token ( TokenErr::ExpectedButFound { 
					expected: vec![
						TokenContent::Bracket(Bracket::Left),
						TokenContent::Operator (Operator::Assign),
					], 
					found: second 
				} ) ),
		};
		Ok(statement)
	}
	
	fn parse_varable_declaration(&mut self) -> Result<Statement, InterpErr> {	
		let var_name = self.tokens_iter.next_name_or_err()?;
		
		self.tokens_iter.next_expect_colon()?;
		
		let type_name = self.tokens_iter.next_name_or_err()?;
		let data_type = DataType::parse(&type_name)?;
		
		let tok_assign = self.tokens_iter.next_or_err()?;
		match tok_assign {
			Token { content: TokenContent::Operator (Operator::Assign), .. } => {},
			Token { content: TokenContent::StatementOp ( StatementOp::Semicolon ), .. } => return 
				Ok ( Statement::WithVariable ( WithVariable::Declare {
					var_name, 
					data_type, 
				}
			) ),
			found @ _ => return Err( InterpErr::Token( TokenErr::ExpectedButFound { 
							expected: vec![
								TokenContent::Operator (Operator::Assign),
								TokenContent::StatementOp ( StatementOp::Semicolon ),
							], 
							found
						} ) ),
		};
		
		let value_expr = ArithmeticExpr::new(
			&mut self.tokens_iter, 
			ExprContextKind::ValueToAssign)?;

		self.tokens_iter.next_expect_semicolon()?;
		
		Ok ( Statement::WithVariable ( WithVariable::DeclareSet {
				var_name, 
				data_type, 
				value_expr,
			}
		) )
	}
	
	fn parse_variable_set(&mut self, var_name: String, _is_builtin: bool) -> Result<Statement, InterpErr> {		
		let value_expr = ArithmeticExpr::new(
			&mut self.tokens_iter,
			ExprContextKind::ValueToAssign)?;
		
		self.tokens_iter.next_expect_semicolon()?;
		
		Ok ( Statement::WithVariable ( WithVariable::Set {
				var_name, 
				value_expr,
			}
		) )
	}

	fn parse_func_call(&mut self, func_name: String, is_builtin: bool) -> Result<Statement, InterpErr> {
		let mut arg_exprs = Vec::<ArithmeticExpr>::new();
		
		loop {
			if let TokenContent::Bracket (Bracket::Right) = self.tokens_iter.peek_or_err()?.content() {
				break;
			}
			
			arg_exprs.push(ArithmeticExpr::new(
				&mut self.tokens_iter,
				ExprContextKind::FunctionArg)?);
				
			match self.tokens_iter.next_or_err()? {
				Token { content: TokenContent::StatementOp ( StatementOp::Comma ), .. } => {},
				
				Token { content: TokenContent::Bracket ( Bracket::Right ), .. } => break,
				
				found @ _ => 
					return Err( InterpErr::Token( TokenErr::ExpectedButFound { 
						expected: vec![
							TokenContent::StatementOp(StatementOp::Comma),
							TokenContent::Bracket ( Bracket::Right ),
						], 
						found
					} ) ),
			}
		}
		
		self.tokens_iter.next_expect_semicolon()?;
		
		Ok ( Statement::FuncCall {
			kind: if is_builtin { FuncKind::Builtin } else { FuncKind::UserDefined },
			name: func_name, 
			arg_exprs,
		} )
	}
}

impl Iterator for StatementsIter {
	type Item = Result<Statement, InterpErr>;
	
	fn next(&mut self) -> Option<Self::Item> {
		match self.tokens_iter.cache_until_semicolon() {
			Ok(true) => {},
			Ok(false) => return None,
			Err(err) => return Some(Err(InterpErr::from(err))),
		}
		
		if self.tokens_iter.cached_queue_is_empty() { 
			return None;
		}
		
		let first = self.tokens_iter.next_or_err().unwrap();
		
		let statement_result = match first {
			Token { content: TokenContent::Keyword ( Keyword::Var ), .. } => 
				self.parse_varable_declaration(),	
			
			Token { content: TokenContent::Name( name ), .. } => 
				self.parse_variable_set_or_func_call(name, false),
			
			Token { content: TokenContent::BuiltinName( name ), .. } =>
				self.parse_variable_set_or_func_call(name, true),
				
			found @ _ => 
				Err( InterpErr::Token( TokenErr::ExpectedButFound { 
					expected: vec![
						TokenContent::Keyword ( Keyword::Var ),
						TokenContent::Name( String::from("<name>") ),
					], 
					found
				} ) ),
		};
		
		Some(statement_result)
	}	
}

//------------------- Statement --------------------

#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
	WithVariable (WithVariable),
	FuncCall { 
		kind: FuncKind,
		name: String, 
		arg_exprs: Vec<ArithmeticExpr> 
	},
}

#[derive(Debug, Eq, PartialEq)]
pub enum WithVariable {
	Declare { var_name: String, data_type: DataType },
	DeclareSet { var_name: String, data_type: DataType, value_expr: ArithmeticExpr },
	Set { var_name: String, value_expr: ArithmeticExpr },
}

#[derive(Debug, Eq, PartialEq)]
pub enum FuncKind {
	UserDefined,
	Builtin,
}

//------------------- StatementErr --------------------

#[derive(Debug, PartialEq, Eq)]
pub enum StatementErr {
	#[allow(unused)]
	EndReached,
}

impl std::fmt::Display for StatementErr {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			StatementErr::EndReached => writeln!(f, "End reached"),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use super::super::var_data::VarValue;
	use super::super::memory::Memory;
	
	#[test]
	pub fn can_make_variable_declare_statement() {
		let mut statements_iter = StatementsIter::new();
		statements_iter.push_string("var a: f32;".to_string());
		
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
	pub fn can_make_variable_declare_set_statement() {
		let mut statements_iter = StatementsIter::new();
		statements_iter.push_string("var a: f32 = 3;".to_string());
		
		let mem = Memory::new();
		
		let st = statements_iter.next().unwrap().unwrap();
		match st {
			Statement::WithVariable ( 
				WithVariable::DeclareSet {
					var_name, 
					data_type: DataType::Float32, 
					value_expr
				} 
			) 
			if 
				(value_expr.calc(&mem).unwrap() == VarValue::Float32(3_f32) &&
				var_name == String::from("a"))
				=> {},
			_ => panic!("wrong statement: {:?}", st),
		};
		
		assert!(statements_iter.next().is_none());
	}
	
	#[test]
	fn can_parse_builtin_funcs_call() {
		let mut st_iter = StatementsIter::new();
		st_iter.push_string("@print(1.2 + 3.45);".to_string());
		
		match st_iter.next().unwrap().unwrap() {
			Statement::FuncCall {
				kind: FuncKind::Builtin,
				name, 
				arg_exprs
			} => {
				assert_eq!("print", &name);
				
				assert_eq!(arg_exprs.len(), 1);
				
				let mem = Memory::new();
				
				match arg_exprs[0].calc(&mem).unwrap() {
					VarValue::Float32 (val) => if (val - (1.2_f32 + 3.45_f32)).abs() > std::f32::EPSILON * 2.0 {
						panic!("{} != {}", (1.2_f32 + 3.45_f32), val);
					},
					ans @ _ => panic!("Wrong answer {:?}", ans),
				}
			},
			st @ _ => panic!("Wrong pattern {:?}", st),
		};
		
		assert_eq!(
			st_iter.next(),
			None
		);
	}

	#[test]
	pub fn can_make_statement_in_multiple_steps() {
		let mut statements_iter = StatementsIter::new();
		
		let mem = Memory::new();
		
		statements_iter.push_string("var".to_string());		
		assert_eq!(statements_iter.next(), None);
		
		statements_iter.push_string("a: ".to_string());
		assert_eq!(statements_iter.next(), None);
		
		statements_iter.push_string("f32 ".to_string());
		assert_eq!(statements_iter.next(), None);
		
		statements_iter.push_string(" = 3 ".to_string());
		assert_eq!(statements_iter.next(), None);
		
		statements_iter.push_string(";".to_string());
		let st = statements_iter.next().unwrap().unwrap();
		match st {
			Statement::WithVariable ( 
				WithVariable::DeclareSet {
					var_name, 
					data_type: DataType::Float32, 
					value_expr
				} 
			) 
			if 
				(value_expr.calc(&mem).unwrap() == VarValue::Float32(3_f32) &&
				var_name == String::from("a"))
				=> {},
			_ => panic!("wrong statement: {:?}", st),
		};
		
		statements_iter.push_string("a".to_string());		
		assert_eq!(statements_iter.next(), None);
		
		statements_iter.push_string(" = 4".to_string());		
		assert_eq!(statements_iter.next(), None);
		
		statements_iter.push_string(";".to_string());				
		let st = statements_iter.next().unwrap().unwrap();
		match st {
			Statement::WithVariable ( 
				WithVariable::Set {
					var_name, 
					value_expr
				} 
			) 
			if 
				(value_expr.calc(&mem).unwrap() == VarValue::Float32(4_f32) &&
				var_name == String::from("a"))
				=> {},
			_ => panic!("wrong statement: {:?}", st),
		};
		
		statements_iter.push_string("@print(".to_string());		
		assert_eq!(statements_iter.next(), None);
		
		statements_iter.push_string("2 + 3 *(".to_string());		
		assert_eq!(statements_iter.next(), None);
		
		statements_iter.push_string("4 + 3".to_string());		
		assert_eq!(statements_iter.next(), None);
		
		statements_iter.push_string(")".to_string());		
		assert_eq!(statements_iter.next(), None);
		
		statements_iter.push_string(");".to_string());				
		match statements_iter.next().unwrap().unwrap() {
			Statement::FuncCall {
				kind: FuncKind::Builtin,
				name, 
				arg_exprs
			} => {
				assert_eq!("print", &name);
				
				assert_eq!(arg_exprs.len(), 1);
				
				let mem = Memory::new();
				
				let right_ans = 2_f32 + 3_f32 * (4_f32 + 3_f32);
				
				match arg_exprs[0].calc(&mem).unwrap() {
					VarValue::Float32 (val) => if (val - right_ans).abs() > std::f32::EPSILON * 2.0 {
						panic!("{} != {}", right_ans, val);
					},
					ans @ _ => panic!("Wrong answer {:?}", ans),
				}
			},
			st @ _ => panic!("Wrong pattern {:?}", st),
		};
		
		assert_eq!(statements_iter.next(), None);
	}

}