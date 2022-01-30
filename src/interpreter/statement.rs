use super::token::*;
use super::expr::{Expr, ExprContextKind};
use super::{InterpErr};
use super::var_data::{DataType, VarErr, Value};
use super::func_data::{BuiltinFuncsDefList, BuiltinFuncDef, UserFuncArg, UserFuncDef};
use super::memory::Memory;
use super::utils::{CharPos, CodePos, NameToken};

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
	
	fn parse_varable_declaration(&mut self, begin_pos: CharPos) -> Result<Statement, InterpErr> {	
		let var_name: NameToken = NameToken::from_or_err(self.tokens_iter.next_or_end_reached_err()?)?;
		
		self.tokens_iter.next_expect_colon()?;
		
		let type_name: NameToken = NameToken::from_or_err(self.tokens_iter.next_or_end_reached_err()?)?;
		let data_type = DataType::parse(&type_name)?;
		
		let tok_assign = self.tokens_iter.next_or_end_reached_err()?;
		match tok_assign {
			Token { content: TokenContent::Operator (Operator::Assign), .. } => {},
			Token { content: TokenContent::StatementOp ( StatementOp::Semicolon ), .. } => return 
				Ok ( Statement {
					kind: StatementKind::VariableDeclare {
						var_name, 
						data_type, 
					},
					pos: CodePos::new(begin_pos, type_name.pos().end()),
				} ),
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
		
		let pos = CodePos::new(begin_pos, value_expr.pos().end());
		Ok ( Statement {
			kind: StatementKind::VariableDeclareSet {
				var_name, 
				data_type, 
				value_expr,
			},
			pos,
		}	)
	}
	
	fn parse_if_else_statement(&mut self, begin_pos: CharPos) -> Result<Statement, InterpErr> {
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
		
		let pos = if let Some(st) = else_body.statements().last() {
			CodePos::new(begin_pos, st.pos().end())
		} else if let Some(body) = if_bodies.last() {
			if let Some(st) = body.statements().last() {
				CodePos::new(begin_pos, st.pos().end())
			} else {
				unreachable!()
			}
		} else {
			CodePos::new(begin_pos, begin_pos)
		};
		
		Ok( Statement {
			kind: StatementKind::BranchingIfElse { if_bodies, else_body },
			pos,
		} )
	}
	
	fn parse_while_statement(&mut self, begin_pos: CharPos) -> Result<Statement, InterpErr> {
		let condition_expr = Expr::new(
			&mut self.tokens_iter,
			ExprContextKind::IfCondition)?;
		
		let mut statements = Vec::<Statement>::new();
		self.parse_body(&mut statements)?;
			
		let body = ConditionalBody { condition_expr, statements };

		let pos = if let Some(st) = body.statements().last() {
			CodePos::new(begin_pos, st.pos().end())
		} else {
			CodePos::new(begin_pos, begin_pos)
		};
		Ok( Statement {
			kind: StatementKind::BranchingWhile { body },
			pos,
		} )
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
		
		let pos = CodePos::new(var_name.pos().begin(), value_expr.pos().end());
		Ok ( Statement {
			kind: StatementKind::VariableSet {
				var_name, 
				value_expr,
			},
			pos,
		} )
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
		
		let pos = if let Some(expr) = arg_exprs.last() {
			CodePos::new(func_name.pos().begin(), expr.pos().end())
		} else {
			CodePos::new(func_name.pos().begin(), func_name.pos().begin())
		};
		Ok ( Statement {
			kind: StatementKind::FuncCall {
				kind: if is_builtin { FuncKind::Builtin } else { FuncKind::UserDefined },
				func_name, 
				arg_exprs,
			},
			pos,
		} )
	}

	fn parse_user_func_def(&mut self, begin_pos: CharPos) -> Result<Statement, InterpErr> {
		let name: NameToken = NameToken::from_or_err(self.tokens_iter.next_or_end_reached_err()?)?;
		
		self.tokens_iter.next_expect_left_bracket()?;
		
		let mut args = Vec::<UserFuncArg>::new();
		loop {
			let arg_name: NameToken = NameToken::from_or_err(self.tokens_iter.next_or_end_reached_err()?)?;
			
			self.tokens_iter.next_expect_colon()?;
			
			let type_name: NameToken = NameToken::from_or_err(self.tokens_iter.next_or_end_reached_err()?)?;
			let data_type: DataType = DataType::parse(&type_name)?;
			
			args.push(UserFuncArg::new(arg_name, data_type));
			
			match self.tokens_iter.next_or_end_reached_err()? {
				Token { content: TokenContent::StatementOp (StatementOp::Comma), .. } => continue,
				Token { content: TokenContent::Bracket (Bracket::Right), .. } => break,
				found @ _ => return Err( InterpErr::from( TokenErr::ExpectedButFound {
					expected: vec![
						TokenContent::StatementOp (StatementOp::Comma),
						TokenContent::Bracket (Bracket::Right),
					],
					found,
				} ) ),
			}
		}
		
		self.tokens_iter.next_expect_thin_arrow()?;
		let return_type_name: NameToken = NameToken::from_or_err(self.tokens_iter.next_or_end_reached_err()?)?;
		let return_type: DataType = DataType::parse(&return_type_name)?;
		
		let mut statements = Vec::<Statement>::new();
		self.parse_body(&mut statements)?;
		
		let pos = if let Some(st) = statements.last() {
			CodePos::new(begin_pos, st.pos().end())
		} else {
			CodePos::new(begin_pos, begin_pos)
		};
		Ok ( Statement {
			kind: StatementKind::UserDefinedFuncDeclare {
				name,
				args, 
				return_type,
				body: ReturningBody::new(statements),
			},
			pos,
		} )
	}

	fn parse_return_statement(&mut self, begin_pos: CharPos) -> Result<Statement, InterpErr> {
		let return_expr: Expr = Expr::new(
			&mut self.tokens_iter, 
			ExprContextKind::ToReturn)?;
		
		self.tokens_iter.next_expect_semicolon()?;
		
		let pos = CodePos::new(begin_pos, return_expr.pos().end());
		Ok ( Statement {
			kind: StatementKind::UserDefinedFuncReturn { return_expr },
			pos,
		} )
	}

	fn parse_next_statement(&mut self) -> Option<Result<Statement, InterpErr>> {
		let first: Token = match self.tokens_iter.next()? {
			Ok(tok) => tok,
			Err(err) => return Some(Err(InterpErr::from(err))),
		};
		
		let begin_pos: CharPos = first.pos().begin();
		
		let statement_result: Result<Statement, InterpErr> = match first {
			Token { content: TokenContent::StatementOp ( StatementOp::Comment (content) ), pos } => 
				Ok( Statement { kind: StatementKind::Comment (content), pos } ),
				
			Token { content: TokenContent::Keyword ( Keyword::Var ), .. } => 
				self.parse_varable_declaration(begin_pos),	
				
			Token { content: TokenContent::Keyword ( Keyword::If ), .. } =>
				self.parse_if_else_statement(begin_pos),	
				
			Token { content: TokenContent::Keyword ( Keyword::While ), .. } =>
				self.parse_while_statement(begin_pos),	
				
			Token { content: TokenContent::Keyword ( Keyword::F ), .. } =>
				self.parse_user_func_def(begin_pos),	
				
			Token { content: TokenContent::Keyword ( Keyword::Return ), .. } =>
				self.parse_return_statement(begin_pos),	
			
			Token { content: TokenContent::Name(_), .. } => 
				self.parse_variable_set_or_func_call(NameToken::from_or_err(first).unwrap(), false),
			
			Token { content: TokenContent::BuiltinName(_), .. } =>
				self.parse_variable_set_or_func_call(NameToken::from_or_err(first).unwrap(), true),
				
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
		let st = self.parse_next_statement();
		match st {
			Some(Ok(ref st)) => println!("{}", st),
			_ => {},
		}
		st
	}	
}

//------------------- Statement --------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Statement {
	pos: CodePos,
	kind: StatementKind,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StatementKind {
	Comment (String),
	VariableDeclare { var_name: NameToken, data_type: DataType },
	VariableDeclareSet { var_name: NameToken, data_type: DataType, value_expr: Expr },
	VariableSet { var_name: NameToken, value_expr: Expr },
	UserDefinedFuncReturn { 
		return_expr: Expr,
	},
	UserDefinedFuncDeclare {
		name: NameToken,
		args: Vec<UserFuncArg>,
		return_type: DataType,
		body: ReturningBody,
	},
	FuncCall {
		kind: FuncKind,
		func_name: NameToken, 
		arg_exprs: Vec<Expr>,
	},
	BranchingIfElse {
		if_bodies: Vec<ConditionalBody>,
		else_body: UnconditionalBody,
	},
	BranchingWhile {
		body: ConditionalBody,
	},
}

impl Statement {
	pub fn check(
		&self, 
		check_memory: &mut Memory, 
		builtin_func_defs: &BuiltinFuncsDefList
	) -> Result<(), InterpErr> 
	{
		match &self.kind {
			StatementKind::Comment (_) => {},
				
			StatementKind::VariableDeclare { var_name, data_type } => {
					check_memory.add_variable(var_name.clone(), *data_type,  None)?;
				},
					
			StatementKind::VariableDeclareSet { var_name, data_type, value_expr } => {
				check_memory.add_variable(var_name.clone(), *data_type, None)?;
				
				let expr_data_type: DataType = value_expr.check_and_calc_data_type(check_memory, builtin_func_defs)?;
				if *data_type != expr_data_type {
					return Err(InterpErr::from(VarErr::WrongType { 
						value_data_type: expr_data_type, 
						var_data_type: *data_type,
						var_name: var_name.clone(),
					}));
				}
			},
			
			StatementKind::VariableSet { var_name, value_expr } => {
				let var_data_type: DataType = check_memory.get_variable_type(var_name)?;
				let expr_data_type: DataType = value_expr.check_and_calc_data_type(check_memory, builtin_func_defs)?;
				if var_data_type != expr_data_type {
					return Err(InterpErr::from(VarErr::WrongType { 
						value_data_type: expr_data_type, 
						var_data_type,
						var_name: var_name.clone(),
					}));
				}
			},
			
			StatementKind::FuncCall { kind, func_name, arg_exprs } => {
				match kind {
					FuncKind::UserDefined => {
						let func_def: UserFuncDef = check_memory.find_func_def(func_name)?.clone();
						
						check_memory.push_frame();
						let func_args: &Vec<UserFuncArg> = func_def.args();
						for i in 0..arg_exprs.len() {
							check_memory.add_variable(
								func_args[i].name().clone(),
								func_args[i].data_type(),
								None)?;
						}
						func_def.check_args(arg_exprs, check_memory, builtin_func_defs)?;
						check_memory.pop_frame();
					},
					
					FuncKind::Builtin => {					
						let func_def: &BuiltinFuncDef = builtin_func_defs.find(func_name)?;
						
						func_def.check_args(func_name, arg_exprs, check_memory, builtin_func_defs)?;
					},
				}
			},
			
			StatementKind::UserDefinedFuncDeclare { name, args, return_type, body } => {
				check_memory.add_user_func(name.clone(), args.clone(), *return_type, body.clone())?;
				
				check_memory.push_frame();
				
				for arg_ref in args.iter() {
					check_memory.add_variable(arg_ref.name().clone(), arg_ref.data_type(), None)?;
				}
				
				body.check(*return_type, check_memory, builtin_func_defs)?;
				
				check_memory.pop_frame();
			},
			
			StatementKind::UserDefinedFuncReturn { return_expr } => {
				return_expr.check_and_calc_data_type(check_memory, builtin_func_defs)?;
			},
		
			StatementKind::BranchingIfElse { if_bodies, else_body } => {
				for body in if_bodies.iter() {
					body.check(check_memory, builtin_func_defs)?;
				}
				
				else_body.check(check_memory, builtin_func_defs)?;
			},
			StatementKind::BranchingWhile { body } => body.check(check_memory, builtin_func_defs)?,
		}
		Ok(())
	}
	
	pub fn run(&self, memory: &mut Memory, builtin_func_defs: &BuiltinFuncsDefList) -> Option<Value> {
		match &self.kind {
			StatementKind::Comment (_) => {},
			
			StatementKind::VariableDeclare { var_name, data_type } => 
				memory.add_variable(var_name.clone(), *data_type, None).unwrap(),
					
			StatementKind::VariableDeclareSet { var_name, data_type, value_expr } => {
				let value: Value = value_expr.calc(memory, builtin_func_defs);
				memory.add_variable(
					var_name.clone(), 
					*data_type,
					Some(value)).unwrap();
			},
				
			StatementKind::VariableSet { var_name, value_expr } => {
				let value: Value = value_expr.calc(memory, builtin_func_defs);
				memory.set_variable(&var_name, value).unwrap();
			},
			
			StatementKind::FuncCall { kind, func_name, arg_exprs } => {
					match kind {
						FuncKind::Builtin => {
							let f = builtin_func_defs.find(func_name).unwrap();
							
							let mut args_values = Vec::<Value>::with_capacity(arg_exprs.len());
							for expr in arg_exprs {
								let value: Value = expr.calc(memory, builtin_func_defs);
								args_values.push(value);
							}
							
							f.call(args_values);
						},
						
						FuncKind::UserDefined => {							
							let mut args_values = Vec::<Value>::with_capacity(arg_exprs.len());
							for expr in arg_exprs {
								let value: Value = expr.calc(memory, builtin_func_defs);
								args_values.push(value);
							}
							
							// TODO: Do not clone function definition
							let func_def: UserFuncDef = memory.find_func_def(func_name).unwrap().clone();
							assert_eq!(args_values.len(), func_def.args().len());
							
							memory.push_frame();
							
							let func_args: &Vec<UserFuncArg> = func_def.args();
							for i in 0..args_values.len() {
								memory.add_variable(
									func_args[i].name().clone(),
									func_args[i].data_type(),
									Some(args_values[i].clone())).unwrap();
							}
							
							func_def.call(memory, builtin_func_defs);
							
							memory.pop_frame();
						},
					}
				},
				
			StatementKind::UserDefinedFuncDeclare { name, args, return_type, body } => {
				memory.add_user_func(name.clone(), args.clone(), *return_type, body.clone()).unwrap();
			},
				
			StatementKind::UserDefinedFuncReturn { return_expr } => {
				let value: Value = return_expr.calc(memory, builtin_func_defs);
				return Some(value)
			},
			
			StatementKind::BranchingIfElse { if_bodies, else_body } => {
				for body in if_bodies {
					if let (true, value_op) = body.run_if_true(memory, builtin_func_defs) {
						return value_op;
					}
				}
				
				return else_body.run(memory, builtin_func_defs);
			},
			
			StatementKind::BranchingWhile { body } => {
				return loop {
					match body.run_if_true(memory, builtin_func_defs) {
						(false, _) => break None,
						(true, Some(value)) => break Some(value),
						(true, None) => {},
					}
				}
			},
		}
		
		None
	}

	pub fn pos(&self) -> CodePos {
		self.pos
	}
}

impl std::fmt::Display for Statement {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "[{}, {}] ", self.pos().begin(), self.pos().end())?;
		match &self.kind {
			StatementKind::Comment (comment) => writeln!(f, "//{}", comment),
				
			StatementKind::VariableDeclare { var_name, data_type } => writeln!(f, "declare {}: {}", var_name, data_type),
					
			StatementKind::VariableDeclareSet { var_name, data_type, .. } => writeln!(f, "declare {}: {} = expr", var_name, data_type),
				
			StatementKind::VariableSet { var_name, .. } => writeln!(f, "set {} = expr", var_name),
			
			StatementKind::FuncCall { kind, func_name, .. } => writeln!(f, "call {} func {}(...)", kind, func_name),
				
			StatementKind::UserDefinedFuncDeclare { name, return_type, body, .. } => {
				writeln!(f, "define user func {}(...) -> {}", name, return_type)?;
				for st_ref in body.statements().iter() {
					writeln!(f, "\t{}", st_ref)?;
				}
				writeln!(f, "------------------")
			},
			
			StatementKind::UserDefinedFuncReturn { .. } => writeln!(f, "return ..."),
		
			StatementKind::BranchingIfElse { if_bodies, else_body } => {
				let mut is_not_first = false;
				for body in if_bodies.iter() {
					if is_not_first {
						writeln!(f, "else if (...):")?;
					} else {
						writeln!(f, "if (...):")?;
					}
					is_not_first = true;
					for st_ref in body.statements().iter() {
						writeln!(f, "\t{}", st_ref)?;
					}
					writeln!(f, "------------------")?;
				}
				
				writeln!(f, "else (...):")?;
				for st_ref in else_body.statements().iter() {
					writeln!(f, "\t{}", st_ref)?;
				}
				writeln!(f, "------------------")
			},
			
			StatementKind::BranchingWhile { body } => {
				writeln!(f, "while (...):")?;
				for st_ref in body.statements().iter() {
					writeln!(f, "\t{}", st_ref)?;
				}
				writeln!(f, "------------------")
			},
		}
	}
}

//------------------------- FuncKind -----------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum FuncKind {
	UserDefined,
	Builtin,
}

impl std::fmt::Display for FuncKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			FuncKind::UserDefined => writeln!(f, "UserDefined"),
			FuncKind::Builtin => writeln!(f, "Builtin"),
		}
	}
}

//------------------------- ConditionalBody -----------------------

#[derive(Debug, Clone, Eq, PartialEq)]
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
	
	pub fn check(&self, check_memory: &mut Memory, builtin_func_defs: &BuiltinFuncsDefList) -> Result<(), InterpErr> {
		match self.condition_expr.check_and_calc_data_type(check_memory, builtin_func_defs)? {
			DataType::Bool => {},
			_ => return Err( InterpErr::from( StatementErr::IfConditionType { 
										pos: self.condition_expr().pos(),
									} ) ),
		}
		
		check_memory.push_scope();
		for st_ref in self.statements.iter() {
			st_ref.check(check_memory, builtin_func_defs)?;
		}
		check_memory.pop_scope();
		
		Ok(())
	}
	
	pub fn run_if_true(&self, memory: &mut Memory, builtin_func_defs: &BuiltinFuncsDefList) -> (bool, Option<Value>) {
		match self.condition_expr().calc(memory, builtin_func_defs) {
			Value::Bool(true) => {
				memory.push_scope();
				
				for st_ref in self.statements() {
					match st_ref.run(memory, builtin_func_defs) {
						res @ Some(_) => return (true, res),
						None => {},
					}
				}
				
				memory.pop_scope();
				
				(true, None)
			},
			Value::Bool(false) => (false, None),
			result @ _ => panic!("Wrong condition expr data type: {:?}", result.get_type()),
		}
	}
}

//------------------------- UnconditionalBody -----------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct UnconditionalBody {
	statements: Vec<Statement>,
}

impl UnconditionalBody {
	pub fn statements(&self) -> &Vec<Statement> {
		&self.statements
	}
	
	pub fn check(&self, check_memory: &mut Memory, builtin_func_defs: &BuiltinFuncsDefList) -> Result<(), InterpErr> {
		check_memory.push_scope();
		for st_ref in self.statements.iter() {
			st_ref.check(check_memory, builtin_func_defs)?;
		}
		check_memory.pop_scope();
		
		Ok(())
	}
	
	pub fn run(&self, memory: &mut Memory, builtin_func_defs: &BuiltinFuncsDefList) -> Option<Value> {
		memory.push_scope();
		
		for st_ref in self.statements() {
			match st_ref.run(memory, builtin_func_defs) {
				res @ Some(_) => return res,
				None => {},
			}
		}
		
		memory.pop_scope();
		
		None
	}
}

//------------------------- ReturningBody -----------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ReturningBody {
	statements: Vec<Statement>,
}

impl ReturningBody {
	pub fn new(statements: Vec<Statement>) -> Self {
		Self {
			statements,
		}
	}
	
	pub fn check(&self, return_type: DataType, check_memory: &mut Memory, builtin_func_defs: &BuiltinFuncsDefList) -> Result<(), InterpErr> {
		let mut has_return = false; // at least one statement on 1st level returns
		for st_ref in self.statements().iter() {
			if Self::returns(st_ref, return_type, check_memory, builtin_func_defs)? {
				has_return = true;
			}
		}
		
		if !has_return {
			return Err( InterpErr::from(StatementErr::UserFuncNotAllFuncPathsReturn {
				last_statement_pos: CodePos::from(CharPos::new()),
			}) );
		}
		
		for st_ref in self.statements.iter() {
			st_ref.check(check_memory, builtin_func_defs)?;
		}
		
		Ok(())
	}
	
	fn returns(statement: &Statement, declared_return_type: DataType, check_memory: &Memory, builtin_func_defs: &BuiltinFuncsDefList) -> Result<bool, InterpErr> {
		match &statement.kind {
			StatementKind::Comment (_) 
				| StatementKind::VariableDeclare { .. }
				| StatementKind::VariableDeclareSet { .. }
				| StatementKind::VariableSet { .. }
				| StatementKind::UserDefinedFuncDeclare { .. }
				| StatementKind::FuncCall { .. }
			=> Ok(false),
			
			StatementKind::UserDefinedFuncReturn { return_expr } => {
				let returned_type: DataType = return_expr.check_and_calc_data_type(check_memory, builtin_func_defs)?;
				if returned_type != declared_return_type {
					Err( InterpErr::from(StatementErr::UserFuncReturnType {
						return_expr_pos: statement.pos(),
						declared_return_type,
						returned_type,
					}) )
				} else {
					Ok(true)
				}
			},
			
			StatementKind::BranchingIfElse { if_bodies, else_body } => {
				let mut all_bodies_return = true;
				
				for body_ref in if_bodies.iter() {
					let mut body_returns = false;
					for st_ref in body_ref.statements() {
						if Self::returns(st_ref, declared_return_type, check_memory, builtin_func_defs)? {
							body_returns = true;
						}
					}
					if !body_returns {
						all_bodies_return = false;
					}
				}
				
				let mut body_returns = false;
				for st_ref in else_body.statements() {
					if Self::returns(st_ref, declared_return_type, check_memory, builtin_func_defs)? {
						body_returns = true;
					}
				}
				if !body_returns {
					all_bodies_return = false;
				}
				
				return Ok(all_bodies_return);
			},
				
			StatementKind::BranchingWhile { body } => {
				let mut body_returns = false;
				for st_ref in body.statements().iter() {
					if Self::returns(st_ref, declared_return_type, check_memory, builtin_func_defs)? {
						body_returns = true;
					}
				}
				
				return Ok(body_returns);
			},
		}
	}
	
	pub fn run(&self, memory: &mut Memory, builtin_func_defs: &BuiltinFuncsDefList) -> Value {
		for st_ref in self.statements.iter() {
			match st_ref.run(memory, builtin_func_defs) {
				Some(value) => return value,
				None => {},
			}
		}
		
		unreachable!();
	}
	
	pub fn statements(&self) -> &Vec<Statement> {
		&self.statements
	}
}

//------------------- StatementErr --------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementErr {
	UnfinishedBody (CharPos),
	IfConditionType { 
		pos: CodePos,
	},
	UserFuncReturnType {
		return_expr_pos: CodePos,
		declared_return_type: DataType,
		returned_type: DataType,
	},
	UserFuncNotAllFuncPathsReturn {
		last_statement_pos: CodePos,
	},
}

impl std::fmt::Display for StatementErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			StatementErr::UnfinishedBody (_) => 
				write!(f, "Unfinished body"),
			StatementErr::IfConditionType { .. } => 
				write!(f, "'If' condition expression data type must be bool"),
			StatementErr::UserFuncReturnType { declared_return_type, returned_type, .. } => 
				write!(f, "User-defined function return type must be '{}', but found '{}'", declared_return_type, returned_type),
			StatementErr::UserFuncNotAllFuncPathsReturn { .. } =>
				write!(f, "Not all user-defined function paths return"),
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
	use super::super::utils::NameToken;
	
	#[test]
	fn can_parse_comment() {
		let mut statements_iter = StatementsIter::new();
		
		statements_iter.push_string("//var a: f32;\nvar a: f32;//d".to_string());	
		
		assert_eq!(
			statements_iter.next().unwrap().unwrap().kind, 
			StatementKind::Comment (String::from("var a: f32;")) 
		);
		
		let nt = new_name_token("a");
		
		assert_eq!(
			statements_iter.next().unwrap().unwrap().kind, 
			StatementKind::VariableDeclare {
					var_name: nt, 
					data_type: DataType::Float32, 
			} 
		);
		
		assert_eq!(
			statements_iter.next().unwrap().unwrap().kind, 
			StatementKind::Comment (String::from("d")) 
		);
		
		assert!(statements_iter.next().is_none());
	}
	
	#[test]
	fn can_make_variable_declare_statement() {
		let mut statements_iter = StatementsIter::new();
		
		let nt = new_name_token("a");
		
		statements_iter.push_string("var a: f32;".to_string());		
		let st = statements_iter.next().unwrap().unwrap();
		assert_eq!(st.kind, StatementKind::VariableDeclare {
			var_name: nt.clone(), 
			data_type: DataType::Float32, 
		});		
		assert!(statements_iter.next().is_none());
		
		statements_iter.push_string("var a: str;".to_string());
		let st = statements_iter.next().unwrap().unwrap();
		assert_eq!(st.kind, StatementKind::VariableDeclare {
			var_name: nt.clone(), 
			data_type: DataType::String, 
		});		
		assert!(statements_iter.next().is_none());
		
		statements_iter.push_string("var a: bool;".to_string());
		let st = statements_iter.next().unwrap().unwrap();
		assert_eq!(st.kind, StatementKind::VariableDeclare {
			var_name: nt.clone(), 
			data_type: DataType::Bool, 
		});		
		assert!(statements_iter.next().is_none());
	}
	
	#[test]
	fn can_make_variable_declare_set_statement() {
		let mut statements_iter = StatementsIter::new();
		let mut mem = Memory::new();
		
		let nt = new_name_token("a");
		let builtin_func_defs = BuiltinFuncsDefList::new();
		
		statements_iter.push_string("var a: f32 = 3;".to_string());
		let st = statements_iter.next().unwrap().unwrap();
		match st.kind {
			StatementKind::VariableDeclareSet {
					var_name, 
					data_type: DataType::Float32, 
					value_expr
				} 
			if 
				(value_expr.calc(&mut mem, &builtin_func_defs) == Value::from(3_f32) &&
				var_name == nt)
				=> {},
			_ => panic!("wrong statement: {:?}", st),
		};		
		assert!(statements_iter.next().is_none());
		
		statements_iter.push_string("var a: str = \"hello\";".to_string());
		let st = statements_iter.next().unwrap().unwrap();
		match st.kind {
			StatementKind::VariableDeclareSet {
					var_name, 
					data_type: DataType::String, 
					value_expr
				}
			if 
				(value_expr.calc(&mut mem, &builtin_func_defs) == Value::from("hello") &&
				var_name == nt)
				=> {},
			_ => panic!("wrong statement: {:?}", st),
		};		
		assert!(statements_iter.next().is_none());
		
		statements_iter.push_string("var a: bool = True;".to_string());
		let st = statements_iter.next().unwrap().unwrap();
		match st.kind {
			StatementKind::VariableDeclareSet {
					var_name, 
					data_type: DataType::Bool, 
					value_expr
				} 
			if 
				(value_expr.calc(&mut mem, &builtin_func_defs) == Value::from(true) &&
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
		
		let nt = new_name_token("print");
		let builtin_func_defs = BuiltinFuncsDefList::new();
		
		match st_iter.next().unwrap().unwrap().kind {
			StatementKind::FuncCall {
				kind: FuncKind::Builtin,
				func_name, 
				arg_exprs
			} => {
				assert_eq!(nt, func_name);
				
				assert_eq!(arg_exprs.len(), 1);
				
				let mut mem = Memory::new();
				
				match arg_exprs[0].calc(&mut mem, &builtin_func_defs) {
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
		
		let nt = new_name_token("print");
		
		match st_iter.next().unwrap().unwrap().kind {
			StatementKind::FuncCall {
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
		@print("End!");
		"#.to_string());	
		
		let st = statements_iter.next().unwrap().unwrap();
		let (if_bodies, else_body) = if let StatementKind::BranchingIfElse { if_bodies, else_body } = &st.kind {
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
		@print("End!");"#.to_string());	
		let st = statements_iter.next().unwrap().unwrap();	
		let (if_bodies, else_body) = if let StatementKind::BranchingIfElse { if_bodies, else_body } = &st.kind {
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
		@print("End!");"#.to_string());	
		let st = statements_iter.next().unwrap().unwrap();
		let (if_bodies, else_body) = if let StatementKind::BranchingIfElse { if_bodies, else_body } = &st.kind {
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
		@print("End!");"#.to_string());	
		let st = statements_iter.next().unwrap().unwrap();
		let (if_bodies, else_body) = if let StatementKind::BranchingIfElse { if_bodies, else_body } = &st.kind {
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
		@print("End!");"#.to_string());	
		let st = statements_iter.next().unwrap().unwrap();
		let (if_bodies, else_body) = if let StatementKind::BranchingIfElse { if_bodies, else_body } = &st.kind {
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
		@print("End!");
		"#.to_string());	
			
		let types_memory = Memory::new();
		let mut vars_memory = Memory::new();
		let builtin_func_defs = BuiltinFuncsDefList::new();
		
		let st = statements_iter.next().unwrap().unwrap();
		if let StatementKind::VariableDeclareSet {
			var_name,
			data_type,
			value_expr,
		} = st.kind {
			assert_eq!(var_name.value(), "a");
			assert_eq!(data_type, DataType::Float32);
		
			assert_eq!(
				value_expr.check_and_calc_data_type(&types_memory, &builtin_func_defs).unwrap(), 
				DataType::Float32);
						
			assert_eq!(
				value_expr.calc(&mut vars_memory, &builtin_func_defs), 
				Value::from(3_f32));
		} else {
			panic!("Wrong statement: {:?}", st);
		};
		
		let st = statements_iter.next().unwrap().unwrap();
		let body = if let StatementKind::BranchingWhile { body } = &st.kind { 
			body
		} else {
			panic!("Wrong statement: {:?}", st);
		};
		check_conditional_body(&body, true, &Value::from("a is"), &Value::from("a"));
		
		let st = statements_iter.next().unwrap().unwrap();
		check_is_exit_call(&st);
	}
	
	fn check_is_exit_call(st: &Statement) {
		let nt_exit = new_name_token("print");
		
		match &st.kind {
			StatementKind::FuncCall { 
				kind: FuncKind::Builtin,
				func_name, 
				arg_exprs,
			} => {
				assert_eq!(*func_name, nt_exit);
						
				assert_eq!(arg_exprs.len(), 1);
		
				let types_memory = Memory::new();
				let mut vars_memory = Memory::new();
				let builtin_func_defs = BuiltinFuncsDefList::new();
				
				assert_eq!(
					arg_exprs[0].check_and_calc_data_type(&types_memory, &builtin_func_defs).unwrap(), 
					DataType::String);
					
				assert_eq!(
					arg_exprs[0].calc(&mut vars_memory, &builtin_func_defs), 
					Value::from("End!"));
			},
			_ => { dbg!(st); panic!("Not IfElse: "); },
		}
	}
	
	fn check_conditional_body(body: &ConditionalBody, conditional_result: bool, text1: &Value, text2: &Value) {
		let ConditionalBody { condition_expr, statements } = body;
		
		let types_memory = Memory::new();
		let mut vars_memory = Memory::new();
		let builtin_func_defs = BuiltinFuncsDefList::new();
		
		assert_eq!(
			condition_expr.check_and_calc_data_type(&types_memory, &builtin_func_defs).unwrap(), 
			DataType::Bool);
					
		assert_eq!(
			condition_expr.calc(&mut vars_memory, &builtin_func_defs), 
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
		let nt_print = new_name_token("print");
		
		let types_memory = Memory::new();
		let mut vars_memory = Memory::new();
		let builtin_func_defs = BuiltinFuncsDefList::new();
		
		match &statement.kind {
			StatementKind::FuncCall { kind: FuncKind::Builtin, func_name, arg_exprs } => {
				assert_eq!(*func_name, nt_print);
				
				assert_eq!(arg_exprs.len(), 1);
				
				assert_eq!(
					arg_exprs[0].check_and_calc_data_type(&types_memory, &builtin_func_defs).unwrap(), 
					DataType::String);
					
				assert_eq!(
					arg_exprs[0].calc(&mut vars_memory, &builtin_func_defs), 
					*value);
			},
			_ => panic!("Wrong statement: {:?}", statement),
		}
	}
	
	#[test]
	fn can_parse_user_func_def_statement() {
		let mut statements_iter = StatementsIter::new();
		let mut mem = Memory::new();
		
		let builtin_func_defs = BuiltinFuncsDefList::new();
		
		statements_iter.push_string(r#"
		f add_squared(a: f32, b: f32) -> f32 {
			var a2: f32 = a ^ 2;
			var b2: f32 = b ^ 2;
			return a2 + b2;
		}
		"#.to_string());
		
		let st = statements_iter.next().unwrap().unwrap();
		match st.kind {
			StatementKind::UserDefinedFuncDeclare { name, args, return_type, body } => {
				assert_eq!(name, new_name_token("add_squared"));
				assert_eq!(
					args,
					vec![
						UserFuncArg::new(new_name_token("a"), DataType::Float32),
						UserFuncArg::new(new_name_token("b"), DataType::Float32),
					]);
				assert_eq!(return_type, DataType::Float32);
				
				assert_eq!(body.statements().len(), 3);
				
				mem.push_frame();
				mem.add_variable(new_name_token("a"), DataType::Float32, None).unwrap();
				mem.add_variable(new_name_token("b"), DataType::Float32, None).unwrap();
				
				match &body.statements()[0].kind {
					StatementKind::VariableDeclareSet { var_name, data_type, value_expr } => {
						assert_eq!(*var_name, new_name_token("a2"));
						assert_eq!(*data_type, DataType::Float32);
						let dt: DataType = value_expr.check_and_calc_data_type(&mem, &builtin_func_defs).unwrap();
						assert_eq!(dt, DataType::Float32);
					},
					_ => {
						println!("Wrong statement:");
						dbg!(&body.statements()[0]);
						panic!("Err!");
					},
				}
				
				mem.add_variable(new_name_token("a2"), DataType::Float32, None).unwrap();
				
				match &body.statements()[1].kind {
					StatementKind::VariableDeclareSet { var_name, data_type, value_expr } => {
						assert_eq!(*var_name, new_name_token("b2"));
						assert_eq!(*data_type, DataType::Float32);
						let dt: DataType = value_expr.check_and_calc_data_type(&mem, &builtin_func_defs).unwrap();
						assert_eq!(dt, DataType::Float32);
					},
					_ => {
						println!("Wrong statement:");
						dbg!(&body.statements()[1]);
						panic!("Err!");
					},
				}
				
				mem.add_variable(new_name_token("b2"), DataType::Float32, None).unwrap();
				
				match &body.statements()[2].kind {
					StatementKind::UserDefinedFuncReturn { return_expr } => {
						let dt: DataType = return_expr.check_and_calc_data_type(&mem, &builtin_func_defs).unwrap();
						assert_eq!(dt, DataType::Float32);
					},
					_ => {
						println!("Wrong statement:");
						dbg!(&body.statements()[2]);
						panic!("Err!");
					},
				}
			},
			_ => {
				println!("Wrong statement:");
				dbg!(st);
				panic!("Err!");
			},
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
		check_end_reached("@foo");
		check_end_reached("@foo(");
		check_end_reached("@foo()");
	}

	fn new_name_token(name: &str) -> NameToken {
		NameToken::new_with_pos(name, CodePos::from(CharPos::new()))
	}
}