use super::token::*;
use super::expr::{Expr, ExprContextKind};
use super::{InterpErr};
use super::string_char::CharPos;
use super::var_data::{DataType, VarErr};
use super::func_data::{BuiltinFuncsDefList, BuiltinFuncDef, BuiltinFuncErr};
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
	
	fn parse_if_else_statement(&mut self) -> Result<Statement, InterpErr> {
		let mut if_bodies = Vec::<ConditionalBody>::new();
		let mut else_body = UnconditionalBody {
			statements: Vec::new(),
		};
		
		loop {
			let condition_expr = Expr::new(
				&mut self.tokens_iter,
				ExprContextKind::IfCondition)?;
			
			let mut statements = Vec::<Statement>::new();
			self.parse_body(&mut statements)?;
			
			if_bodies.push(ConditionalBody {
				condition_expr,
				statements,
			});
			
			if let Some(Token { content: TokenContent::Keyword (Keyword::Else), .. }) = self.tokens_iter.peek()? {
				self.tokens_iter.next_or_end_reached_err().unwrap();
				
				if let Some(Token { content: TokenContent::Keyword (Keyword::If), .. }) = self.tokens_iter.peek()? {
					self.tokens_iter.next_or_end_reached_err().unwrap();
					continue; // parse next if () {}
				}
				
				// parse final else
				self.parse_body(&mut else_body.statements)?;
				break;
			}
			
			break; // another statement starts here, if-else chain is finished
		}
		
		Ok( Statement::Branching (Branching::IfElse { if_bodies, else_body }) )
	}
	
	fn parse_while_statement(&mut self) -> Result<Statement, InterpErr> {
		let condition_expr = Expr::new(
			&mut self.tokens_iter,
			ExprContextKind::IfCondition)?;
		
		let mut statements = Vec::<Statement>::new();
		self.parse_body(&mut statements)?;
			
		let body = ConditionalBody { condition_expr, statements };

		Ok( Statement::Branching (Branching::While { body }) )
	}
	
	fn parse_body(&mut self, body: &mut Vec<Statement>) -> Result<(), InterpErr> {
			self.tokens_iter.next_expect_left_curly_bracket()?;
			
			loop {
				match self.tokens_iter.peek_or_end_reached_err()? {
					Token { content: TokenContent::Bracket (Bracket::RightCurly), .. } => {
						self.tokens_iter.next_or_end_reached_err().unwrap();
						break Ok(());
					},
					_ => match self.parse_next_statement() {
						Some(statement_result) => body.push(statement_result?),
						None => break Err( InterpErr::from( StatementErr::UnfinishedBody (self.tokens_iter.pos()) ) ),
					},
				}
			}
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
				self.parse_if_else_statement(),	
				
			Token { content: TokenContent::Keyword ( Keyword::While ), .. } =>
				self.parse_while_statement(),	
			
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
	pub fn check(
		&self, 
		check_memory: &mut Memory, 
		builtin_func_defs: &BuiltinFuncsDefList
	) -> Result<(), InterpErr> 
	{
		match self {
			Statement::Comment (_) => {},
				
			Statement::WithVariable (st) => match st {
				WithVariable::Declare { var_name, data_type } => {
					check_memory.add_variable(var_name.clone(), *data_type,  None)?;
				},
					
				WithVariable::DeclareSet { var_name, data_type, value_expr } => {
					check_memory.add_variable(var_name.clone(), *data_type, None)?;
					
					let expr_data_type: DataType = value_expr.check_and_calc_data_type(check_memory)?;
					if *data_type != expr_data_type {
						return Err(InterpErr::from(VarErr::WrongType { 
							value_data_type: expr_data_type, 
							var_data_type: *data_type,
							var_name: var_name.clone(),
						}));
					}
				},
				
				WithVariable::Set { var_name, value_expr } => {
					let var_data_type: DataType = check_memory.get_variable_type(var_name)?;
					let expr_data_type: DataType = value_expr.check_and_calc_data_type(check_memory)?;
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
						let func_def: &BuiltinFuncDef = match builtin_func_defs.try_find(func_name.value()) {
							Err(err) => match err {
								BuiltinFuncErr::NotDefined =>
									return Err(InterpErr::from(StatementErr::Func { err, name: func_name.clone() })),
								_ => unreachable!(),
							},
							Ok(fd) => fd,
						};
						
						let args_data_types: Result<Vec<DataType>, InterpErr> = arg_exprs.iter()
							.map(|expr| expr.check_and_calc_data_type(check_memory))
							.collect();
						
						match func_def.check_args(args_data_types?) {
							Err(err) => match err {
								BuiltinFuncErr::ArgsCnt { .. } => 
									return Err(InterpErr::from(StatementErr::Func { err, name: func_name.clone() })),
								BuiltinFuncErr::ArgType { .. } => {
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
				Branching::IfElse { if_bodies, else_body } => {
					for body in if_bodies.iter() {
						match body.condition_expr.check_and_calc_data_type(check_memory)? {
							DataType::Bool => {},
							_ => return Err( InterpErr::from( StatementErr::IfConditionType { 
														cond_expr_begin: body.condition_expr.pos_begin(),
														cond_expr_end: body.condition_expr.pos_end(),
													} ) ),
						}
						for st_ref in body.statements.iter() {
							st_ref.check(check_memory, builtin_func_defs)?;
						}
					}
					for st_ref in else_body.statements.iter() {
						st_ref.check(check_memory, builtin_func_defs)?;
					}
				},
				Branching::While { body } => {
					match body.condition_expr().check_and_calc_data_type(check_memory)? {
						DataType::Bool => {},
						_ => return Err( InterpErr::from( StatementErr::IfConditionType { 
													cond_expr_begin: body.condition_expr.pos_begin(),
													cond_expr_end: body.condition_expr.pos_end(),
												} ) ),
					}
					for st_ref in body.statements().iter() {
						st_ref.check(check_memory, builtin_func_defs)?;
					}
				},
			}
		}
		Ok(())
	}
}

impl std::fmt::Display for Statement {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Statement::Comment (c) => writeln!(f, "Comment({})", c),
			Statement::WithVariable (st) => match st {
				WithVariable::Declare { var_name, .. } => writeln!(f, "Declare '{}'", var_name.value()),
				WithVariable::DeclareSet { var_name, .. } => writeln!(f, "Declare and set '{}'", var_name.value()),
				WithVariable::Set { var_name, .. } => writeln!(f, "Set '{}'", var_name.value()),
			},
			Statement::FuncCall { func_name, .. } => writeln!(f, "FuncCall '{}'", func_name.value()),
			Statement::Branching (br_st) => match br_st {
				Branching::IfElse { .. } => {
					writeln!(f, "If () {{")
					/*for part in parts {
						writeln!(f, "If () {{")?;
						for st in part.statements.iter() {
							writeln!(f, "{}", st)?;
						}
						writeln!(f, "}} else ")?;
					}
					writeln!(f, "{{")?;
					for st in else_body.iter() {
						writeln!(f, "{}", st)?;
					}
					writeln!(f, "}}")*/
				},
				Branching::While { .. } => writeln!(f, "While () {{"),
			}
		}
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
	IfElse {
		if_bodies: Vec<ConditionalBody>,
		else_body: UnconditionalBody,
	},
	While {
		body: ConditionalBody,
	},
}

#[derive(Debug, Eq, PartialEq)]
pub struct ConditionalBody {
	condition_expr: Expr,
	statements: Vec<Statement>,
}

impl ConditionalBody {
	pub fn condition_expr(&self) -> &Expr {
		&self.condition_expr
	}
	pub fn statements(&self) -> &Vec<Statement> {
		&self.statements
	}
}

#[derive(Debug, Eq, PartialEq)]
pub struct UnconditionalBody {
	statements: Vec<Statement>,
}

impl UnconditionalBody {
	pub fn statements(&self) -> &Vec<Statement> {
		&self.statements
	}
}

//------------------- StatementErr --------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementErr {
	Func { err: BuiltinFuncErr, name: NameToken },
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
				BuiltinFuncErr::ArgsCnt { actual_cnt, given_cnt } => 
					write!(f, "Builtin function '{}' has {} argument(-s) but {} were given", &name, actual_cnt, given_cnt),
				BuiltinFuncErr::ArgType { actual_type, given_type } => 
					write!(f, "Argument '{}' must have {:?} type but {:?} were given", &name, actual_type, given_type),
				BuiltinFuncErr::NotDefined => 
					write!(f, "Builtin function '{}' is not defined", &name),
				BuiltinFuncErr::AlreadyDefined => 
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
				(value_expr.calc(&mem).unwrap() == Value::from(3_f32) &&
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
				(value_expr.calc(&mem).unwrap() == Value::from("hello") &&
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
				(value_expr.calc(&mem).unwrap() == Value::from(true) &&
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
	fn can_parse_branching_if_else_statement() {
		let mut statements_iter = StatementsIter::new();
		
		//--------------------- if ---------------------
		statements_iter.push_string(r#"
		if 2 == 2 { 
			@print("2 == 2"); 
			@print("Cool!"); 
		} 
		@exit();
		"#.to_string());	
		
		let st = statements_iter.next().unwrap().unwrap();
		let (if_bodies, else_body) = if let Statement::Branching (Branching::IfElse { if_bodies, else_body }) = st {
			(if_bodies, else_body) 
		} else {
			panic!("Not IfElse: {:?}", st);
		};
		match if_bodies.as_slice() {
			[cb1] => {
				check_conditional_body(cb1, true, &Value::from("2 == 2"), &Value::from("Cool!"));
			},
			found @ _ => panic!("Wrong if_bodies: {:?}", found),
		}
		assert_eq!(else_body.statements().len(), 0);
		
		let st = statements_iter.next().unwrap().unwrap();
		check_is_exit_call(&st);
		
		//--------------------- if-else --------------------- 
		statements_iter.push_string(r#"
		if 3 == 1 + 2 { 
			@print("3 == 1 + 2"); 
			@print("Cool!"); 
		} else { 
			@print("3 != 1 + 2"); 
			@print("Ooops!"); 
		} 
		@exit();"#.to_string());	
		let st = statements_iter.next().unwrap().unwrap();	
		let (if_bodies, else_body) = if let Statement::Branching (Branching::IfElse { if_bodies, else_body }) = st {
			(if_bodies, else_body)
		} else {
			dbg!(st); panic!("Not IfElse: ");
		};
		match if_bodies.as_slice() {
			[cb1] => {
				check_conditional_body(cb1, true, &Value::from("3 == 1 + 2"), &Value::from("Cool!"));
			},
			found @ _ => { dbg!(found); panic!("Wrong if_bodies:"); },
		}
		check_unconditional_body(&else_body, &Value::from("3 != 1 + 2"), &Value::from("Ooops!"));
		
		let st = statements_iter.next().unwrap().unwrap();
		check_is_exit_call(&st);
		
		//--------------------- if-elseif ---------------------
		statements_iter.push_string(r#"
		if 3 == 1 + 2 { 
			@print("3 == 1 + 2"); 
			@print("Cool!"); 
		} else if 45 * 6 == 6 * 45 { 
			@print("45 * 6 == 6 * 45"); 
			@print("Nice!"); 
		}
		@exit();"#.to_string());	
		let st = statements_iter.next().unwrap().unwrap();
		let (if_bodies, else_body) = if let Statement::Branching (Branching::IfElse { if_bodies, else_body }) = st {
			(if_bodies, else_body)
		} else {
			dbg!(st); panic!("Not IfElse: ");
		};
		match if_bodies.as_slice() {
			[cb1, cb2] => {
				check_conditional_body(cb1, true, &Value::from("3 == 1 + 2"), &Value::from("Cool!"));
				check_conditional_body(cb2, true, &Value::from("45 * 6 == 6 * 45"), &Value::from("Nice!"));
			},
			found @ _ => { dbg!(found); panic!("Wrong if_bodies:"); },
		}
		assert_eq!(else_body.statements().len(), 0);
		
		let st = statements_iter.next().unwrap().unwrap();
		check_is_exit_call(&st);
		
		//--------------------- if-elseif-else ---------------------
		statements_iter.push_string(r#"
		if 3 == 1 + 2 { 
			@print("3 == 1 + 2"); 
			@print("Cool!"); 
		} else if (45 * 6 == 6 * 45) land False { 
			@print("45 * 6 == 6 * 45"); 
			@print("Nice!"); 
		} else { 
			@print("Not cool((("); 
			@print("Math broken"); 
		}
		@exit();"#.to_string());	
		let st = statements_iter.next().unwrap().unwrap();
		let (if_bodies, else_body) = if let Statement::Branching (Branching::IfElse { if_bodies, else_body }) = st {
			(if_bodies, else_body)
		} else {
			dbg!(st); panic!("Not IfElse: ");
		};
		match if_bodies.as_slice() {
			[cb1, cb2] => {
				check_conditional_body(cb1, true, &Value::from("3 == 1 + 2"), &Value::from("Cool!"));
				check_conditional_body(cb2, false, &Value::from("45 * 6 == 6 * 45"), &Value::from("Nice!"));
			},
			found @ _ => { dbg!(found); panic!("Wrong if_bodies:"); },
		}
		check_unconditional_body(&else_body, &Value::from("Not cool((("), &Value::from("Math broken"));
		
		let st = statements_iter.next().unwrap().unwrap();
		check_is_exit_call(&st);
		
		//--------------------- if-elseif-elseif-else ---------------------
		statements_iter.push_string(r#"
		if 3 == 1 + 2 { 
			@print("3 == 1 + 2"); 
			@print("Cool!"); 
		} else if (45 * 6 == 6 * 45) land False { 
			@print("45 * 6 == 6 * 45"); 
			@print("Nice!"); 
		} else if (10 == 7) lor True { 
			@print("(10 == 7) lor True"); 
			@print("Ok!"); 
		} else { 
			@print("Not cool((("); 
			@print("Math broken"); 
		}
		@exit();"#.to_string());	
		let st = statements_iter.next().unwrap().unwrap();
		let (if_bodies, else_body) = if let Statement::Branching (Branching::IfElse { if_bodies, else_body }) = st {
			(if_bodies, else_body)
		} else {
			dbg!(st); panic!("Not IfElse: ");
		};
		match if_bodies.as_slice() {
			[cb1, cb2, cb3] => {
				check_conditional_body(cb1, true, &Value::from("3 == 1 + 2"), &Value::from("Cool!"));
				check_conditional_body(cb2, false, &Value::from("45 * 6 == 6 * 45"), &Value::from("Nice!"));
				check_conditional_body(cb3, true, &Value::from("(10 == 7) lor True"), &Value::from("Ok!"));
			},
			found @ _ => { dbg!(found); panic!("Wrong if_bodies:"); },
		}
		check_unconditional_body(&else_body, &Value::from("Not cool((("), &Value::from("Math broken"));
		
		let st = statements_iter.next().unwrap().unwrap();
		check_is_exit_call(&st);
		
		assert!(statements_iter.next().is_none());
	}
	
	#[test]
	fn can_parse_branching_while_statement() {
		let mut statements_iter = StatementsIter::new();
		
		statements_iter.push_string(r#"
		var a: f32 = 3;
		while 3 > 0 { 
			@print("a is"); 
			@print("a"); 
		} 
		@exit();
		"#.to_string());	
			
		let types_memory = Memory::new();
		let vars_memory = Memory::new();
		
		let st = statements_iter.next().unwrap().unwrap();
		if let Statement::WithVariable (WithVariable::DeclareSet {
			var_name,
			data_type,
			value_expr,
		}) = st {
			assert_eq!(var_name.value(), "a");
			assert_eq!(data_type, DataType::Float32);
		
			assert_eq!(
				value_expr.check_and_calc_data_type(&types_memory).unwrap(), 
				DataType::Float32);
						
			assert_eq!(
				value_expr.calc(&vars_memory).unwrap(), 
				Value::from(3_f32));
		} else {
			panic!("Wrong statement: {:?}", st);
		};
		
		let st = statements_iter.next().unwrap().unwrap();
		let body = if let Statement::Branching (Branching::While { body }) = st { 
			body
		} else {
			panic!("Wrong statement: {:?}", st);
		};
		check_conditional_body(&body, true, &Value::from("a is"), &Value::from("a"));
		
		let st = statements_iter.next().unwrap().unwrap();
		check_is_exit_call(&st);
	}
	
	fn check_is_exit_call(st: &Statement) {		
		let nt_exit = NameToken::from(Token::new(CharPos::new(), CharPos::new(), TokenContent::Name(String::from("exit")))).unwrap();
		
		match st {
			Statement::FuncCall { 
				kind: FuncKind::Builtin,
				func_name, 
				arg_exprs,
			} => {
				assert_eq!(*func_name, nt_exit);
						
				assert_eq!(arg_exprs.len(), 0);
			},
			_ => { dbg!(st); panic!("Not IfElse: "); },
		}
	}
	
	fn check_conditional_body(body: &ConditionalBody, conditional_result: bool, text1: &Value, text2: &Value) {
		let ConditionalBody { condition_expr, statements } = body;
		
		let types_memory = Memory::new();
		let vars_memory = Memory::new();
		
		assert_eq!(
			condition_expr.check_and_calc_data_type(&types_memory).unwrap(), 
			DataType::Bool);
					
		assert_eq!(
			condition_expr.calc(&vars_memory).unwrap(), 
			Value::Bool(conditional_result));
		
		assert_eq!(statements.len(), 2);
		
		check_statement(&statements[0], text1);
		check_statement(&statements[1], text2);
	}
	
	fn check_unconditional_body(body: &UnconditionalBody, value1: &Value, value2: &Value) {
		let UnconditionalBody { statements } = body;
		
		assert_eq!(statements.len(), 2);
		
		check_statement(&statements[0], value1);
		check_statement(&statements[1], value2);
	}
	
	fn check_statement(statement: &Statement, value: &Value) {
		let nt_print = NameToken::from(Token::new(CharPos::new(), CharPos::new(), TokenContent::Name(String::from("print")))).unwrap();
		
		let types_memory = Memory::new();
		let vars_memory = Memory::new();
		
		match statement {
			Statement::FuncCall { kind: FuncKind::Builtin, func_name, arg_exprs } => {
				assert_eq!(*func_name, nt_print);
				
				assert_eq!(arg_exprs.len(), 1);
				
				assert_eq!(
					arg_exprs[0].check_and_calc_data_type(&types_memory).unwrap(), 
					DataType::String);
					
				assert_eq!(
					arg_exprs[0].calc(&vars_memory).unwrap(), 
					*value);
			},
			_ => panic!("Wrong statement: {:?}", statement),
		}
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