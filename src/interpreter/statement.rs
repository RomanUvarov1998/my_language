use super::token::*;
use super::expr::{Expr, ExprContextKind};
use super::{InterpErr};
use super::string_char::CharPos;
use super::var_data::{DataType, VarErr};
use super::func_data::{FuncsDefList, FuncDef, FuncErr};
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
	
	fn parse_variable_set_or_func_call(&mut self, name: NameToken, is_builtin: bool) -> Result<Statement, InterpErr> {
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
		let var_name: NameToken = NameToken::from(self.tokens_iter.next_or_end_reached_err()?)?;
		
		self.tokens_iter.next_expect_colon()?;
		
		let type_name: NameToken = NameToken::from(self.tokens_iter.next_or_end_reached_err()?)?;
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
	
	fn parse_if_statement(&mut self) -> Result<Statement, InterpErr> {
		let condition_expr = Expr::new(
			&mut self.tokens_iter,
			ExprContextKind::IfCondition)?;
		
		self.tokens_iter.next_expect_left_curly_bracket()?;
		
		let mut body = Vec::<Statement>::new();
		
		loop {
			match self.tokens_iter.peek_or_end_reached_err()? {
				Token { content: TokenContent::Bracket (Bracket::RightCurly), .. } => {
					self.tokens_iter.next_or_end_reached_err().unwrap();
					break;
				},
				_ => match self.parse_next_statement() {
					Some(statement_result) => body.push(statement_result?),
					None => return Err( InterpErr::from( StatementErr::UnfinishedBody (self.tokens_iter.pos()) ) ),
				},
			}
		}
		
		Ok( Statement::Branching (Branching::If { condition_expr, body }) )
	}
	
	fn parse_variable_set(&mut self, var_name: NameToken, _is_builtin: bool) -> Result<Statement, InterpErr> {		
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

	fn parse_func_call(&mut self, func_name: NameToken, is_builtin: bool) -> Result<Statement, InterpErr> {
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
			func_name, 
			arg_exprs,
		} )
	}

	fn parse_next_statement(&mut self) -> Option<Result<Statement, InterpErr>> {
		let first: Token = match self.tokens_iter.next()? {
			Ok(tok) => tok,
			Err(err) => return Some(Err(InterpErr::from(err))),
		};
		
		let statement_result = match first {
			Token { content: TokenContent::StatementOp ( StatementOp::Comment (content) ), .. } => 
				Ok( Statement::Comment (content) ),
				
			Token { content: TokenContent::Keyword ( Keyword::Var ), .. } => 
				self.parse_varable_declaration(),	
				
			Token { content: TokenContent::Keyword ( Keyword::If ), .. } => 
				self.parse_if_statement(),	
			
			Token { content: TokenContent::Name(_), .. } => 
				self.parse_variable_set_or_func_call(NameToken::from(first).unwrap(), false),
			
			Token { content: TokenContent::BuiltinName(_), .. } =>
				self.parse_variable_set_or_func_call(NameToken::from(first).unwrap(), true),
				
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

impl Iterator for StatementsIter {
	type Item = Result<Statement, InterpErr>;
	
	fn next(&mut self) -> Option<Self::Item> {
		self.parse_next_statement()
	}	
}

//--------------------- NameToken --------------------------

#[derive(Debug, Clone)]
pub struct NameToken {
	name: String,
	tok: Token, // // TODO: use more lightweigt struct to keep Name position
}

impl NameToken {
	pub fn from(tok: Token) -> Result<Self, InterpErr> {
		match tok {
			Token { content: TokenContent::Name (ref name), .. }
				| Token { content: TokenContent::BuiltinName (ref name), .. } => Ok( NameToken {
				name: name.clone(),
				tok,
			} ),
			found @ _ => return Err( InterpErr::from( TokenErr::ExpectedButFound {
							expected: vec![TokenContent::Name ("<name>".to_string())], 
							found,
						} ) ),
		}
	}
	
	pub fn value(&self) -> &str { &self.name }
	pub fn tok(&self) -> &Token { &self.tok }
}

impl std::fmt::Display for NameToken {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.value())
	}
}

impl PartialEq for NameToken {
	fn eq(&self, other: &Self) -> bool {
		self.value() == other.value()
	}
}
impl Eq for NameToken {}

//------------------- Statement --------------------

#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
	Comment (String),
	WithVariable (WithVariable),
	FuncCall { 
		kind: FuncKind,
		func_name: NameToken, 
		arg_exprs: Vec<Expr>,
	},
	Branching (Branching),
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
			Statement::Comment (_) => {},
				
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
							var_name: var_name.clone(),
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
							var_name: var_name.clone(),
						}));
					}
				},				
			},
			
			Statement::FuncCall { kind, func_name, arg_exprs } => {
				match kind {
					FuncKind::UserDefined => todo!(),
					FuncKind::Builtin => {					
						let func_def: &FuncDef = match builtin_func_defs.try_find(func_name.value()) {
							Err(err) => match err {
								FuncErr::NotDefined =>
									return Err(InterpErr::from(StatementErr::Func { err, name: func_name.clone() })),
								_ => unreachable!(),
							},
							Ok(fd) => fd,
						};
						
						let args_data_types: Result<Vec<DataType>, InterpErr> = arg_exprs.iter()
							.map(|expr| expr.calc_data_type(types_memory, vars_memory))
							.collect();
						
						match func_def.check_args(args_data_types?) {
							Err(err) => match err {
								FuncErr::ArgsCnt { .. } => 
									return Err(InterpErr::from(StatementErr::Func { err, name: func_name.clone() })),
								FuncErr::ArgType { .. } => {
									return Err(InterpErr::from(StatementErr::Func { err, name: func_name.clone() }))
								},
								_ => unreachable!(),
							},
							Ok(()) => {},
						}
					},
				}
			}
		
			Statement::Branching (br) => match br {
				Branching::If { condition_expr, body } => {
					match condition_expr.calc_data_type(types_memory, vars_memory)? {
						DataType::Bool => {},
						_ => return Err( InterpErr::from( StatementErr::IfConditionType { 
													cond_expr_begin: condition_expr.pos_begin(),
													cond_expr_end: condition_expr.pos_end(),
												} ) ),
					}
					
					for st_ref in body {
						st_ref.check_types(types_memory, builtin_func_defs, vars_memory)?;
					}
				},
			}
		}
		Ok(())
	}
}

#[derive(Debug, Eq, PartialEq)]
pub enum WithVariable {
	Declare { var_name: NameToken, data_type: DataType },
	DeclareSet { var_name: NameToken, data_type: DataType, value_expr: Expr },
	Set { var_name: NameToken, value_expr: Expr },
}

#[derive(Debug, Eq, PartialEq)]
pub enum FuncKind {
	UserDefined,
	Builtin,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Branching {
	If {
		condition_expr: Expr,
		body: Vec<Statement>,
	},
}

//------------------- StatementErr --------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementErr {
	Func { err: FuncErr, name: NameToken },
	UnfinishedBody (CharPos),
	IfConditionType { 
		cond_expr_begin: CharPos,
		cond_expr_end: CharPos,
	},
}

impl std::fmt::Display for StatementErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			StatementErr::Func { err, name } => match err {
				FuncErr::ArgsCnt { actual_cnt, given_cnt } => 
					write!(f, "Builtin function '{}' has {} argument(-s) but {} were given", &name, actual_cnt, given_cnt),
				FuncErr::ArgType { actual_type, given_type } => 
					write!(f, "Argument '{}' must have {:?} type but {:?} were given", &name, actual_type, given_type),
				FuncErr::NotDefined => 
					write!(f, "Builtin function '{}' is not defined", &name),
				FuncErr::AlreadyDefined => 
					write!(f, "Builtin function '{}' already defined", &name),
			},
			StatementErr::UnfinishedBody (_) => 
				write!(f, "Unfinished body"),
			StatementErr::IfConditionType { .. } => 
				write!(f, "'If' condition expression data type must be bool"),
		}
	}
}

//------------------- Tests --------------------

#[cfg(test)]
mod tests {
	use super::*;
	use super::super::*;
	use super::super::var_data::Value;
	use super::super::memory::Memory;
	use super::super::token::{Token, TokenContent};
	use super::super::string_char::CharPos;
	use super::super::statement::NameToken;
	
	#[test]
	fn can_parse_comment() {
		let mut statements_iter = StatementsIter::new();
		
		statements_iter.push_string("//var a: f32;\nvar a: f32;//d".to_string());	
		
		assert_eq!(
			statements_iter.next().unwrap().unwrap(), 
			Statement::Comment (String::from("var a: f32;")) 
		);
		
		let nt = NameToken::from(Token::new(CharPos::new(), CharPos::new(), TokenContent::Name(String::from("a")))).unwrap();
		
		assert_eq!(
			statements_iter.next().unwrap().unwrap(), 
			Statement::WithVariable ( 
				WithVariable::Declare {
					var_name: nt, 
					data_type: DataType::Float32, 
				} 
			) 
		);
		
		assert_eq!(
			statements_iter.next().unwrap().unwrap(), 
			Statement::Comment (String::from("d")) 
		);
		
		assert!(statements_iter.next().is_none());
	}
	
	#[test]
	fn can_make_variable_declare_statement() {
		let mut statements_iter = StatementsIter::new();
		
		let nt = NameToken::from(Token::new(CharPos::new(), CharPos::new(), TokenContent::Name(String::from("a")))).unwrap();
		
		statements_iter.push_string("var a: f32;".to_string());		
		let st = statements_iter.next().unwrap().unwrap();
		assert_eq!(st, Statement::WithVariable ( 
				WithVariable::Declare {
					var_name: nt.clone(), 
					data_type: DataType::Float32, 
				} 
			) 
		);		
		assert!(statements_iter.next().is_none());
		
		statements_iter.push_string("var a: str;".to_string());
		let st = statements_iter.next().unwrap().unwrap();
		assert_eq!(st, Statement::WithVariable ( 
				WithVariable::Declare {
					var_name: nt.clone(), 
					data_type: DataType::String, 
				} 
			) 
		);		
		assert!(statements_iter.next().is_none());
		
		statements_iter.push_string("var a: bool;".to_string());
		let st = statements_iter.next().unwrap().unwrap();
		assert_eq!(st, Statement::WithVariable ( 
				WithVariable::Declare {
					var_name: nt.clone(), 
					data_type: DataType::Bool, 
				} 
			) 
		);		
		assert!(statements_iter.next().is_none());
	}
	
	#[test]
	fn can_make_variable_declare_set_statement() {
		let mut statements_iter = StatementsIter::new();
		let mem = Memory::new();
		
		let nt = NameToken::from(Token::new(CharPos::new(), CharPos::new(), TokenContent::Name(String::from("a")))).unwrap();
		
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
				var_name == nt)
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
				var_name == nt)
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
				var_name == nt)
				=> {},
			_ => panic!("wrong statement: {:?}", st),
		};		
		assert!(statements_iter.next().is_none());
	}
	
	#[test]
	fn can_parse_builtin_funcs_call() {
		let mut st_iter = StatementsIter::new();
		st_iter.push_string("@print(1.2 + 3.45);".to_string());
		
		let nt = NameToken::from(Token::new(CharPos::new(), CharPos::new(), TokenContent::BuiltinName(String::from("print")))).unwrap();
		
		match st_iter.next().unwrap().unwrap() {
			Statement::FuncCall {
				kind: FuncKind::Builtin,
				func_name, 
				arg_exprs
			} => {
				assert_eq!(nt, func_name);
				
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
		
		let nt = NameToken::from(Token::new(CharPos::new(), CharPos::new(), TokenContent::BuiltinName(String::from("print")))).unwrap();
		
		match st_iter.next().unwrap().unwrap() {
			Statement::FuncCall {
				kind: FuncKind::Builtin,
				func_name, 
				arg_exprs
			} => {
				assert_eq!(nt, func_name);
				
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
	fn can_parse_branching_if_statement() {
		let mut statements_iter = StatementsIter::new();
		
		statements_iter.push_string("if 2 == 2 { @print(\"2 == 2\"); @print(\"Cool!\"); } @exit();".to_string());	
		let st = statements_iter.next().unwrap().unwrap();
		
		let nt_print = NameToken::from(Token::new(CharPos::new(), CharPos::new(), TokenContent::Name(String::from("print")))).unwrap();
		
		let types_memory = Memory::new();
		let vars_memory = Memory::new();
		
		match st {
			Statement::Branching (
				Branching::If {
					condition_expr,
					body,
				} 
			) => {
				assert_eq!(
					condition_expr.calc_data_type(&types_memory, &vars_memory).unwrap(), 
					DataType::Bool);
					
				assert_eq!(
					condition_expr.calc(&vars_memory).unwrap(), 
					Value::Bool(true));
					
				match body.as_slice() {
					[st1, st2] => {
						// st1
						match st1 {
							Statement::FuncCall { 
								kind: FuncKind::Builtin,
								func_name, 
								arg_exprs,
							} => {
								assert_eq!(*func_name, nt_print);
								
								assert_eq!(arg_exprs.len(), 1);
								
								assert_eq!(
									arg_exprs[0].calc_data_type(&types_memory, &vars_memory).unwrap(), 
									DataType::String);
									
								assert_eq!(
									arg_exprs[0].calc(&vars_memory).unwrap(), 
									Value::String(String::from("2 == 2")));
							},
							st @ _ => panic!("Wrong statement: {:?}", st),
						}
							
						// st2
						match st2 {
							Statement::FuncCall { 
								kind: FuncKind::Builtin,
								func_name, 
								arg_exprs,
							} => {
								assert_eq!(*func_name, nt_print);
						
						assert_eq!(arg_exprs.len(), 1);
								
								assert_eq!(
									arg_exprs[0].calc_data_type(&types_memory, &vars_memory).unwrap(), 
									DataType::String);
									
								assert_eq!(
									arg_exprs[0].calc(&vars_memory).unwrap(), 
									Value::String(String::from("Cool!")));
							},
							st @ _ => panic!("Wrong statement: {:?}", st),
						}
					},
					_ => panic!("Wrong body: {:?}", body),
				}
			},
			_ => panic!("Wrong statement: {:?}", st),
		}	
		
		let st = statements_iter.next().unwrap().unwrap();
		let nt_exit = NameToken::from(Token::new(CharPos::new(), CharPos::new(), TokenContent::Name(String::from("exit")))).unwrap();
		match st {
			Statement::FuncCall { 
				kind: FuncKind::Builtin,
				func_name, 
				arg_exprs,
			} => {
				assert_eq!(func_name, nt_exit);
						
				assert_eq!(arg_exprs.len(), 0);
			},
			_ => panic!("Wrong statement: {:?}", st),
		}
		
		assert!(statements_iter.next().is_none());
	}
	
	#[test]
	fn cannot_make_unfinished_statement() {		
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