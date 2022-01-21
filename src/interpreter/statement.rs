use super::token::*;
use super::expr::{Expr, ExprContextKind};
use super::{InterpErr};
use super::var_data::{DataType, VarErr};
use super::func_data::{FuncsDefList, FuncDef};
use super::memory::Memory;

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
		let second = self.tokens_iter.next_or_end_reached_err()?;
		let statement = match second {
			Token { content: TokenContent::Bracket(Bracket::Left), .. } => 
				self.parse_func_call(name, is_builtin)?,
				
			Token { content: TokenContent::Operator (Operator::Assign), .. } => 
				self.parse_variable_set(name, is_builtin)?,
			_ => return Err( InterpErr::from ( TokenErr::ExpectedButFound { 
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
		
		let tok_assign = self.tokens_iter.next_or_end_reached_err()?;
		match tok_assign {
			Token { content: TokenContent::Operator (Operator::Assign), .. } => {},
			Token { content: TokenContent::StatementOp ( StatementOp::Semicolon ), .. } => return 
				Ok ( Statement::WithVariable ( WithVariable::Declare {
					var_name, 
					data_type, 
				}
			) ),
			found @ _ => return Err( InterpErr::from( TokenErr::ExpectedButFound { 
							expected: vec![
								TokenContent::Operator (Operator::Assign),
								TokenContent::StatementOp ( StatementOp::Semicolon ),
							], 
							found
						} ) ),
		};
		
		let value_expr = Expr::new(
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
		let value_expr = Expr::new(
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
		let mut arg_exprs = Vec::<Expr>::new();
		
		if let TokenContent::Bracket (Bracket::Right) = self.tokens_iter.peek_or_end_reached_err()?.content() {
			self.tokens_iter.next_or_end_reached_err().unwrap();
		} else {
			loop {			
				arg_exprs.push(Expr::new(
					&mut self.tokens_iter,
					ExprContextKind::FunctionArg)?);
					
				match self.tokens_iter.next_or_end_reached_err()? {
					Token { content: TokenContent::StatementOp ( StatementOp::Comma ), .. } => {},
					
					Token { content: TokenContent::Bracket ( Bracket::Right ), .. } => break,
					
					found @ _ => 
						return Err( InterpErr::from( TokenErr::ExpectedButFound { 
							expected: vec![
								TokenContent::StatementOp(StatementOp::Comma),
								TokenContent::Bracket ( Bracket::Right ),
							], 
							found
						} ) ),
				}
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
		let first: Token = match self.tokens_iter.next()? {
			Ok(tok) => tok,
			Err(err) => return Some(Err(InterpErr::from(err))),
		};
		
		let statement_result = match first {
			Token { content: TokenContent::Keyword ( Keyword::Var ), .. } => 
				self.parse_varable_declaration(),	
			
			Token { content: TokenContent::Name( name ), .. } => 
				self.parse_variable_set_or_func_call(name, false),
			
			Token { content: TokenContent::BuiltinName( name ), .. } =>
				self.parse_variable_set_or_func_call(name, true),
				
			found @ _ => 
				Err( InterpErr::from( TokenErr::ExpectedButFound { 
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
		arg_exprs: Vec<Expr> 
	},
}

impl Statement {
	pub fn check_types(
		&self, 
		types_memory: &mut Memory, 
		builtin_func_defs: &FuncsDefList, 
		vars_memory: &Memory
	) -> Result<(), InterpErr> 
	{
		match self {
			Statement::WithVariable (st) => match st {
				WithVariable::Declare { var_name, data_type } => {
					if let Ok(_) = vars_memory.get_variable_type(var_name) {
						return Err(InterpErr::from(VarErr::AlreadyExists { name: var_name.clone() }));
					}
					types_memory.add_variable(var_name.clone(), *data_type,  None)?;
				},
					
				WithVariable::DeclareSet { var_name, data_type, value_expr } => {
					if let Ok(_) = vars_memory.get_variable_type(var_name) {
						return Err(InterpErr::from(VarErr::AlreadyExists { name: var_name.clone() }));
					}
					types_memory.add_variable(var_name.clone(), *data_type, None)?;
					
					let expr_data_type: DataType = value_expr.calc_data_type(types_memory, vars_memory)?;
					if *data_type != expr_data_type {
						return Err(InterpErr::from(VarErr::WrongType { 
							value_data_type: expr_data_type, 
							var_data_type: *data_type,
						}));
					}
				},
				
				WithVariable::Set { var_name, value_expr } => {
					let var_data_type: DataType = match types_memory.get_variable_type(var_name) {
						Err(_) => vars_memory.get_variable_type(var_name)?,
						Ok(dt) => dt,
					};
					let expr_data_type: DataType = value_expr.calc_data_type(types_memory, vars_memory)?;
					if var_data_type != expr_data_type {
						return Err(InterpErr::from(VarErr::WrongType { 
							value_data_type: expr_data_type, 
							var_data_type,
						}));
					}
				},				
			},
			
			Statement::FuncCall { kind, name, arg_exprs } => {
				match kind {
					FuncKind::UserDefined => todo!(),
					FuncKind::Builtin => {						
						let fd: &FuncDef = builtin_func_defs.try_find(name)?;
						
						let args_data_types: Result<Vec<DataType>, InterpErr> = arg_exprs.iter()
							.map(|expr| expr.calc_data_type(types_memory, vars_memory))
							.collect();
						
						fd.check_args(args_data_types?)?;
					},
				}
			}
		}
		Ok(())
	}
}

#[derive(Debug, Eq, PartialEq)]
pub enum WithVariable {
	Declare { var_name: String, data_type: DataType },
	DeclareSet { var_name: String, data_type: DataType, value_expr: Expr },
	Set { var_name: String, value_expr: Expr },
}

#[derive(Debug, Eq, PartialEq)]
pub enum FuncKind {
	UserDefined,
	Builtin,
}

#[cfg(test)]
mod tests {
	use super::*;
	use super::super::*;
	use super::super::var_data::Value;
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
		
		statements_iter.push_string("var a: str;".to_string());
		let st = statements_iter.next().unwrap().unwrap();
		assert_eq!(st, Statement::WithVariable ( 
				WithVariable::Declare {
					var_name: String::from("a"), 
					data_type: DataType::String, 
				} 
			) 
		);		
		assert!(statements_iter.next().is_none());
		
		statements_iter.push_string("var a: bool;".to_string());
		let st = statements_iter.next().unwrap().unwrap();
		assert_eq!(st, Statement::WithVariable ( 
				WithVariable::Declare {
					var_name: String::from("a"), 
					data_type: DataType::Bool, 
				} 
			) 
		);		
		assert!(statements_iter.next().is_none());
	}
	
	#[test]
	pub fn can_make_variable_declare_set_statement() {
		let mut statements_iter = StatementsIter::new();
		let mem = Memory::new();
		
		statements_iter.push_string("var a: f32 = 3;".to_string());
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
				(value_expr.calc(&mem).unwrap() == Value::Float32(3_f32) &&
				var_name == String::from("a"))
				=> {},
			_ => panic!("wrong statement: {:?}", st),
		};		
		assert!(statements_iter.next().is_none());
		
		statements_iter.push_string("var a: str = \"hello\";".to_string());
		let st = statements_iter.next().unwrap().unwrap();
		match st {
			Statement::WithVariable ( 
				WithVariable::DeclareSet {
					var_name, 
					data_type: DataType::String, 
					value_expr
				} 
			) 
			if 
				(value_expr.calc(&mem).unwrap() == Value::String(String::from("hello")) &&
				var_name == String::from("a"))
				=> {},
			_ => panic!("wrong statement: {:?}", st),
		};		
		assert!(statements_iter.next().is_none());
		
		statements_iter.push_string("var a: bool = True;".to_string());
		let st = statements_iter.next().unwrap().unwrap();
		match st {
			Statement::WithVariable ( 
				WithVariable::DeclareSet {
					var_name, 
					data_type: DataType::Bool, 
					value_expr
				} 
			) 
			if 
				(value_expr.calc(&mem).unwrap() == Value::Bool(true) &&
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
					Value::Float32 (val) => if (val - (1.2_f32 + 3.45_f32)).abs() > std::f32::EPSILON * 2.0 {
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
	fn can_parse_builtin_parameterless_funcs_call() {
		let mut st_iter = StatementsIter::new();
		st_iter.push_string("@print();".to_string());
		
		match st_iter.next().unwrap().unwrap() {
			Statement::FuncCall {
				kind: FuncKind::Builtin,
				name, 
				arg_exprs
			} => {
				assert_eq!("print", &name);
				
				assert_eq!(arg_exprs.len(), 0);
			},
			st @ _ => panic!("Wrong pattern {:?}", st),
		};
		
		assert_eq!(
			st_iter.next(),
			None
		);
	}
	
	#[test]
	pub fn cannot_make_unfinished_statement() {		
		let check_end_reached = |code: &str| {
			let mut statements_iter = StatementsIter::new();
			statements_iter.push_string(code.to_string());				
			match statements_iter.next() {
				Some(Err(InterpErr { inner: InnerErr::Token( TokenErr::EndReached { .. }), .. } )) => {},
				err @ _ => panic!("Wrong result for code {}: {:?}, expected EndReached error", code, err),
			}
		};
		
		check_end_reached("var");
		check_end_reached("var a");
		check_end_reached("var a:");
		check_end_reached("var a: f32");
		check_end_reached("var a: f32 = ");
		check_end_reached("var a: f32 = 3");
		
		check_end_reached("a");
		check_end_reached("a = ");
		check_end_reached("a = 4");
		
		check_end_reached("@");
		check_end_reached("@f");
		check_end_reached("@f(");
		check_end_reached("@f()");
	}
}