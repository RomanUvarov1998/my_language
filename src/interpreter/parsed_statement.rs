use super::token::*;
use super::parsed_expr::{ParsedExpr, ParsedExprContextKind};
use super::InterpErr;
use super::value::Value;
use super::data_type::{DataType, Primitive};
use super::var_data::{VarData, VarErr};
use super::builtin_func::BuiltinFuncDef;
use super::user_func::{UserFuncArg, UserFuncDef};
use super::utils::{CharPos, CodePos, NameToken};
use super::context::Context;

pub struct ParsedStatementsIter {
	tokens_iter: TokensIter,
}

impl ParsedStatementsIter {
	pub fn new() -> Self {
		ParsedStatementsIter { 
			tokens_iter: TokensIter::new(),
		}
	}
	
	pub fn push_string(&mut self, text: String) {
		self.tokens_iter.push_string(text);
	}	
	
	fn parse_variable_set_or_func_call(&mut self, name: NameToken, is_builtin: bool) -> Result<ParsedStatement, InterpErr> {
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
	
	fn parse_varable_declaration(&mut self, begin_pos: CharPos) -> Result<ParsedStatement, InterpErr> {	
		let var_name: NameToken = NameToken::from_or_err(self.tokens_iter.next_or_end_reached_err()?)?;
		
		self.tokens_iter.next_expect_colon()?;
		
		let type_name: NameToken = NameToken::from_or_err(self.tokens_iter.next_or_end_reached_err()?)?;
		let data_type = self.parse_data_type(&type_name)?;
		
		let tok_assign = self.tokens_iter.next_or_end_reached_err()?;
		match tok_assign {
			Token { content: TokenContent::Operator (Operator::Assign), .. } => {},
			Token { content: TokenContent::StatementOp ( StatementOp::Semicolon ), .. } => return 
				Ok ( ParsedStatement {
					kind: ParsedStatementKind::VariableDeclare {
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
		
		let value_expr = ParsedExpr::new(
			&mut self.tokens_iter, 
			ParsedExprContextKind::ValueToAssign)?;

		self.tokens_iter.next_expect_semicolon()?;
		
		let pos = CodePos::new(begin_pos, value_expr.pos().end());
		Ok ( ParsedStatement {
			kind: ParsedStatementKind::VariableDeclareSet {
				var_name, 
				data_type, 
				value_expr,
			},
			pos,
		}	)
	}
	
	fn parse_if_else_statement(&mut self, begin_pos: CharPos) -> Result<ParsedStatement, InterpErr> {
		let mut if_bodies = Vec::<ConditionalBody>::new();
		let mut else_body = UnconditionalBody {
			statements: Vec::new(),
		};
		
		loop {
			let condition_expr = ParsedExpr::new(
				&mut self.tokens_iter,
				ParsedExprContextKind::IfCondition)?;
			
			let mut statements = Vec::<ParsedStatement>::new();
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
		
		Ok( ParsedStatement {
			kind: ParsedStatementKind::BranchingIfElse { if_bodies, else_body },
			pos,
		} )
	}
	
	fn parse_while_statement(&mut self, begin_pos: CharPos) -> Result<ParsedStatement, InterpErr> {
		let condition_expr = ParsedExpr::new(
			&mut self.tokens_iter,
			ParsedExprContextKind::IfCondition)?;
		
		let mut statements = Vec::<ParsedStatement>::new();
		self.parse_body(&mut statements)?;
			
		let body = ConditionalBody { condition_expr, statements };

		let pos = if let Some(st) = body.statements().last() {
			CodePos::new(begin_pos, st.pos().end())
		} else {
			CodePos::new(begin_pos, begin_pos)
		};
		Ok( ParsedStatement {
			kind: ParsedStatementKind::BranchingWhile { body },
			pos,
		} )
	}
	
	fn parse_body(&mut self, body: &mut Vec<ParsedStatement>) -> Result<(), InterpErr> {
			self.tokens_iter.next_expect_left_curly_bracket()?;
			
			loop {
				match self.tokens_iter.peek_or_end_reached_err()? {
					Token { content: TokenContent::Bracket (Bracket::RightCurly), .. } => {
						self.tokens_iter.next_or_end_reached_err().unwrap();
						break Ok(());
					},
					_ => match self.parse_next_statement() {
						Some(statement_result) => body.push(statement_result?),
						None => break Err( InterpErr::from( ParsedStatementErr::UnfinishedBody (self.tokens_iter.pos()) ) ),
					},
				}
			}
		}
	
	fn parse_variable_set(&mut self, var_name: NameToken, _is_builtin: bool) -> Result<ParsedStatement, InterpErr> {		
		let value_expr = ParsedExpr::new(
			&mut self.tokens_iter,
			ParsedExprContextKind::ValueToAssign)?;
		
		self.tokens_iter.next_expect_semicolon()?;
		
		let pos = CodePos::new(var_name.pos().begin(), value_expr.pos().end());
		Ok ( ParsedStatement {
			kind: ParsedStatementKind::VariableSet {
				var_name, 
				value_expr,
			},
			pos,
		} )
	}

	fn parse_func_call(&mut self, func_name: NameToken, is_builtin: bool) -> Result<ParsedStatement, InterpErr> {
		let mut arg_exprs = Vec::<ParsedExpr>::new();
		
		if let TokenContent::Bracket (Bracket::Right) = self.tokens_iter.peek_or_end_reached_err()?.content() {
			self.tokens_iter.next_or_end_reached_err().unwrap();
		} else {
			loop {			
				arg_exprs.push(ParsedExpr::new(
					&mut self.tokens_iter,
					ParsedExprContextKind::FunctionArg)?);
					
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
		Ok ( ParsedStatement {
			kind: ParsedStatementKind::FuncCall {
				kind: if is_builtin { FuncKind::Builtin } else { FuncKind::UserDefined },
				func_name, 
				arg_exprs,
			},
			pos,
		} )
	}

	fn parse_user_func_def(&mut self, begin_pos: CharPos) -> Result<ParsedStatement, InterpErr> {
		let name: NameToken = NameToken::from_or_err(self.tokens_iter.next_or_end_reached_err()?)?;
		
		self.tokens_iter.next_expect_left_bracket()?;
		
		let mut args = Vec::<UserFuncArg>::new();
		if let TokenContent::Bracket (Bracket::Right) = self.tokens_iter.peek_or_end_reached_err()?.content() {
			self.tokens_iter.next_or_end_reached_err().unwrap();
		} else {
			loop {
				let arg_name: NameToken = NameToken::from_or_err(self.tokens_iter.next_or_end_reached_err()?)?;
				
				self.tokens_iter.next_expect_colon()?;
				
				let type_name: NameToken = NameToken::from_or_err(self.tokens_iter.next_or_end_reached_err()?)?;
				let data_type: DataType = self.parse_data_type(&type_name)?;
				
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
		}
		
		let return_type: DataType = match self.tokens_iter.peek_or_end_reached_err()? {
			Token { content: TokenContent::StatementOp (StatementOp::ThinArrow), .. } => { // function returns value
				self.tokens_iter.next_or_end_reached_err().unwrap(); // skip ThinArrow
				let return_type_name: NameToken = NameToken::from_or_err(self.tokens_iter.next_or_end_reached_err()?)?;
				self.parse_data_type(&return_type_name)?
			},
			Token { content: TokenContent::Bracket (Bracket::LeftCurly), .. } => { // function doesn't return value
				DataType::Primitive (Primitive::None)
			},
			found @ _ => return Err( InterpErr::from(TokenErr::ExpectedButFound {
				expected: vec![
					TokenContent::StatementOp (StatementOp::ThinArrow),
					TokenContent::Bracket (Bracket::LeftCurly),
				], 
				found: found.clone(),
			} ) ),
		};
		
		let mut statements = Vec::<ParsedStatement>::new();
		self.parse_body(&mut statements)?;
		
		let pos = if let Some(st) = statements.last() {
			CodePos::new(begin_pos, st.pos().end())
		} else {
			CodePos::new(begin_pos, begin_pos)
		};
		Ok ( ParsedStatement {
			kind: ParsedStatementKind::UserDefinedFuncDeclare {
				name,
				args, 
				return_type,
				body: ReturningBody::new(statements),
			},
			pos,
		} )
	}

	fn parse_return_statement(&mut self, return_keyword_pos: CodePos) -> Result<ParsedStatement, InterpErr> {
		let (return_expr, pos): (Option<ParsedExpr>, CodePos) = 
			if let TokenContent::StatementOp (StatementOp::Semicolon) = 
				self.tokens_iter.peek_or_end_reached_err()?.content() 
			{
				self.tokens_iter.next_or_end_reached_err().unwrap(); // skip semicolon
				(None, return_keyword_pos)
			} else {
				let expr: ParsedExpr = ParsedExpr::new(
					&mut self.tokens_iter, 
					ParsedExprContextKind::ToReturn)?;
				self.tokens_iter.next_expect_semicolon()?;
				let pos_end: CharPos = expr.pos().end();
				(Some(expr), CodePos::new(return_keyword_pos.begin(), pos_end))
			};
		
		Ok ( ParsedStatement {
			kind: ParsedStatementKind::UserDefinedFuncReturn { return_expr },
			pos,
		} )
	}

	fn parse_data_type(&mut self, data_type_name: &NameToken) -> Result<DataType, VarErr> {
		match data_type_name.value() {
			"f32" => Ok( DataType::Primitive (Primitive::Float32) ),
			"str" => Ok( DataType::Primitive (Primitive::String) ),
			"bool" => Ok( DataType::Primitive (Primitive::Bool) ),
			_ => Err( VarErr::UnknownType { name: data_type_name.clone() } ),
		}
	}

	fn parse_next_statement(&mut self) -> Option<Result<ParsedStatement, InterpErr>> {
		let first: Token = match self.tokens_iter.next()? {
			Ok(tok) => tok,
			Err(err) => return Some(Err(InterpErr::from(err))),
		};
		
		let begin_pos: CharPos = first.pos().begin();
		
		let statement_result: Result<ParsedStatement, InterpErr> = match first {
			Token { content: TokenContent::StatementOp ( StatementOp::Comment (content) ), pos } => 
				Ok( ParsedStatement { kind: ParsedStatementKind::Comment (content), pos } ),
				
			Token { content: TokenContent::Keyword ( Keyword::Var ), .. } => 
				self.parse_varable_declaration(begin_pos),	
				
			Token { content: TokenContent::Keyword ( Keyword::If ), .. } =>
				self.parse_if_else_statement(begin_pos),	
				
			Token { content: TokenContent::Keyword ( Keyword::While ), .. } =>
				self.parse_while_statement(begin_pos),	
				
			Token { content: TokenContent::Keyword ( Keyword::F ), .. } =>
				self.parse_user_func_def(begin_pos),	
				
			Token { content: TokenContent::Keyword ( Keyword::Return ), pos } =>
				self.parse_return_statement(pos),	
			
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

impl Iterator for ParsedStatementsIter {
	type Item = Result<ParsedStatement, InterpErr>;
	
	fn next(&mut self) -> Option<Self::Item> {
		self.parse_next_statement()
	}	
}

//------------------- ParsedStatement --------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ParsedStatement {
	pos: CodePos,
	kind: ParsedStatementKind,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ParsedStatementKind {
	Comment (String),
	VariableDeclare {
		var_name: NameToken,
		data_type: DataType
	},
	VariableDeclareSet {
		var_name: NameToken,
		data_type: DataType,
		value_expr: ParsedExpr
	},
	VariableSet {
		var_name: NameToken,
		value_expr: ParsedExpr
	},
	UserDefinedFuncReturn { 
		return_expr: Option<ParsedExpr>,
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
		arg_exprs: Vec<ParsedExpr>,
	},
	BranchingIfElse {
		if_bodies: Vec<ConditionalBody>,
		else_body: UnconditionalBody,
	},
	BranchingWhile {
		body: ConditionalBody,
	},
}

impl ParsedStatement {
	pub fn check(
		&self, 
		check_context: &mut Context
	) -> Result<(), InterpErr> 
	{
		match &self.kind {
			ParsedStatementKind::Comment (_) => {},
				
			ParsedStatementKind::VariableDeclare { var_name, data_type } => {
					check_context.add_variable(var_name.clone(), data_type.clone(),  None)?;
				},
					
			ParsedStatementKind::VariableDeclareSet { var_name, data_type, value_expr } => {
				check_context.add_variable(var_name.clone(), data_type.clone(), Some(data_type.default_value()))?;
				
				let expr_data_type: DataType = value_expr.check_and_calc_data_type(check_context)?;
				if data_type.clone() != expr_data_type {
					return Err(InterpErr::from(VarErr::WrongType { 
						value_data_type: expr_data_type, 
						variable_type: data_type.clone(),
						var_name: var_name.clone(),
					}));
				}
			},
			
			ParsedStatementKind::VariableSet { var_name, value_expr } => {
				let expr_data_type: DataType = value_expr.check_and_calc_data_type(check_context)?;
				let var_def: &mut VarData = check_context.get_variable_mut(var_name)?;
				var_def.set(expr_data_type.default_value())?;
				
				let variable_type: &DataType = var_def.get_type();
				
				if variable_type.ne(&expr_data_type) {
					return Err(InterpErr::from(VarErr::WrongType { 
						value_data_type: expr_data_type, 
						variable_type: variable_type.clone(),
						var_name: var_name.clone(),
					}));
				}
			},
			
			ParsedStatementKind::FuncCall { kind, func_name, arg_exprs } => {
				match kind {
					FuncKind::UserDefined => {
						let func_def: &UserFuncDef = check_context.find_func_def(func_name)?;
						
						let mut next_check_context = check_context.new_stack_frame_context();
						
						let func_args: &Vec<UserFuncArg> = func_def.args();
						for i in 0..arg_exprs.len() {
							next_check_context.add_variable(
								func_args[i].name().clone(),
								func_args[i].data_type().clone(),
								Some(func_args[i].data_type().default_value()))?;
						}
						func_def.check_args(arg_exprs, &next_check_context)?;
					},
					
					FuncKind::Builtin => {
						let func_def: &BuiltinFuncDef = check_context.find_builtin_func_def(func_name)?;
						
						func_def.check_args(func_name, arg_exprs, &check_context)?;
					},
				}
			},
			
			ParsedStatementKind::UserDefinedFuncDeclare { name, args, return_type, body } => {
				check_context.add_user_func(name.clone(), args.clone(), return_type.clone(), body.clone())?;
				
				let mut next_check_context = check_context.new_stack_frame_context();
				
				for arg_ref in args.iter() {
					next_check_context.add_variable(
						arg_ref.name().clone(), 
						arg_ref.data_type().clone(),
						Some(arg_ref.data_type().default_value()))?;
				}
				
				body.check(return_type, &mut next_check_context)?;
			},
			
			ParsedStatementKind::UserDefinedFuncReturn { return_expr } => {
				if let Some(ref expr) = return_expr {
					expr.check_and_calc_data_type(check_context)?;
				}
			},
		
			ParsedStatementKind::BranchingIfElse { if_bodies, else_body } => {
				for body in if_bodies.iter() {
					body.check(check_context)?;
				}
				
				else_body.check(check_context)?;
			},
			ParsedStatementKind::BranchingWhile { body } => body.check(check_context)?,
		}
		Ok(())
	}
	
	pub fn run(&self, context: &mut Context) -> Option<Value> {
		match &self.kind {
			ParsedStatementKind::Comment (_) => {},
			
			ParsedStatementKind::VariableDeclare { var_name, data_type } => 
				context.add_variable(var_name.clone(), data_type.clone(), None).unwrap(),
					
			ParsedStatementKind::VariableDeclareSet { var_name, data_type, value_expr } => {
				let value: Value = value_expr.calc(context);
				context.add_variable(
					var_name.clone(), 
					data_type.clone(),
					Some(value)).unwrap();
			},
				
			ParsedStatementKind::VariableSet { var_name, value_expr } => {
				let value: Value = value_expr.calc(context);
				context.set_variable(&var_name, value).unwrap();
			},
			
			ParsedStatementKind::FuncCall { kind, func_name, arg_exprs } => {
					match kind {
						FuncKind::Builtin => {
							let f: &BuiltinFuncDef = context.find_builtin_func_def(func_name).unwrap();
							
							let mut args_values = Vec::<Value>::with_capacity(arg_exprs.len());
							for expr in arg_exprs {
								let value: Value = expr.calc(context);
								args_values.push(value);
							}
							
							f.call(args_values);
						},
						
						FuncKind::UserDefined => {							
							let mut args_values = Vec::<Value>::with_capacity(arg_exprs.len());
							for expr in arg_exprs {
								let value: Value = expr.calc(context);
								args_values.push(value);
							}
							
							let func_def: &UserFuncDef = context.find_func_def(func_name).unwrap();
							assert_eq!(args_values.len(), func_def.args().len());
							
							let mut next_context = context.new_stack_frame_context();
							
							let func_args: &Vec<UserFuncArg> = func_def.args();
							for i in 0..args_values.len() {
								next_context.add_variable(
									func_args[i].name().clone(),
									func_args[i].data_type().clone(),
									Some(args_values[i].clone())).unwrap();
							}
							
							func_def.call(&mut next_context);
						},
					}
				},
				
			ParsedStatementKind::UserDefinedFuncDeclare { name, args, return_type, body } => {
				context.add_user_func(name.clone(), args.clone(), return_type.clone(), body.clone()).unwrap();
			},
				
			ParsedStatementKind::UserDefinedFuncReturn { return_expr } => {
				return match return_expr {
					Some(expr) => {
						let value: Value = expr.calc(context);
						Some(value)
					},
					None => Some(Value::None),
				}
			},
			
			ParsedStatementKind::BranchingIfElse { if_bodies, else_body } => {
				for body in if_bodies {
					if let (true, value_op) = body.run_if_true(context) {
						return value_op;
					}
				}
				
				return else_body.run(context);
			},
			
			ParsedStatementKind::BranchingWhile { body } => {
				return loop {
					match body.run_if_true(context) {
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

impl std::fmt::Display for ParsedStatement {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "[{}, {}] ", self.pos().begin(), self.pos().end())?;
		match &self.kind {
			ParsedStatementKind::Comment (comment) => writeln!(f, "//{}", comment),
				
			ParsedStatementKind::VariableDeclare { var_name, data_type } => writeln!(f, "declare {}: {}", var_name, data_type),
					
			ParsedStatementKind::VariableDeclareSet { var_name, data_type, .. } => writeln!(f, "declare {}: {} = expr", var_name, data_type),
				
			ParsedStatementKind::VariableSet { var_name, .. } => writeln!(f, "set {} = expr", var_name),
			
			ParsedStatementKind::FuncCall { kind, func_name, .. } => writeln!(f, "call {} func {}(...)", kind, func_name),
				
			ParsedStatementKind::UserDefinedFuncDeclare { name, return_type, body, .. } => {
				write!(f, "define user func {}(...)", name)?;
				
				match return_type {
					DataType::Primitive (Primitive::None) => writeln!(f, " ")?,
					dt @ _ => writeln!(f, " -> {}", dt)?,
				}
				
				for st_ref in body.statements().iter() {
					writeln!(f, "\t{}", st_ref)?;
				}
				
				writeln!(f, "------------------")
			},
			
			ParsedStatementKind::UserDefinedFuncReturn { .. } => writeln!(f, "return ..."),
		
			ParsedStatementKind::BranchingIfElse { if_bodies, else_body } => {
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
			
			ParsedStatementKind::BranchingWhile { body } => {
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
	condition_expr: ParsedExpr,
	statements: Vec<ParsedStatement>,
}

impl ConditionalBody {
	pub fn condition_expr(&self) -> &ParsedExpr {
		&self.condition_expr
	}
	
	pub fn statements(&self) -> &Vec<ParsedStatement> {
		&self.statements
	}
	
	pub fn check(&self, check_context: &mut Context) -> Result<(), InterpErr> {
		match self.condition_expr.check_and_calc_data_type(check_context)? {
			DataType::Primitive (Primitive::Bool) => {},
			_ => return Err( InterpErr::from( ParsedStatementErr::IfConditionType { 
										pos: self.condition_expr().pos(),
									} ) ),
		}
		
		check_context.push_scope();
		for st_ref in self.statements.iter() {
			st_ref.check(check_context)?;
		}
		check_context.pop_scope();
		
		Ok(())
	}
	
	pub fn run_if_true(&self, context: &mut Context) -> (bool, Option<Value>) {
		match self.condition_expr().calc(context) {
			Value::Bool(true) => {
				context.push_scope();
				
				for st_ref in self.statements() {
					match st_ref.run(context) {
						res @ Some(_) => return (true, res),
						None => {},
					}
				}
				
				context.pop_scope();
				
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
	statements: Vec<ParsedStatement>,
}

impl UnconditionalBody {
	pub fn statements(&self) -> &Vec<ParsedStatement> {
		&self.statements
	}
	
	pub fn check(&self, check_context: &mut Context) -> Result<(), InterpErr> {
		check_context.push_scope();
		for st_ref in self.statements.iter() {
			st_ref.check(check_context)?;
		}
		check_context.pop_scope();
		
		Ok(())
	}
	
	pub fn run(&self, context: &mut Context) -> Option<Value> {
		context.push_scope();
		
		for st_ref in self.statements() {
			match st_ref.run(context) {
				res @ Some(_) => return res,
				None => {},
			}
		}
		
		context.pop_scope();
		
		None
	}
}

//------------------------- ReturningBody -----------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ReturningBody {
	statements: Vec<ParsedStatement>,
}

impl ReturningBody {
	pub fn new(statements: Vec<ParsedStatement>) -> Self {
		Self {
			statements,
		}
	}
	
	pub fn check(&self, return_type: &DataType, check_context: &mut Context) -> Result<(), InterpErr> {
		let mut has_return = false; // at least one statement on 1st level returns
		for st_ref in self.statements().iter() {
			if Self::returns(st_ref, return_type, check_context)? {
				has_return = true;
			}
		}
		
		if !has_return && return_type.ne(&DataType::Primitive (Primitive::None)) {
			return Err( InterpErr::from(ParsedStatementErr::UserFuncNotAllFuncPathsReturn {
				last_statement_pos: CodePos::from(CharPos::new()),
			}) );
		}
		
		for st_ref in self.statements.iter() {
			st_ref.check(check_context)?;
		}
		
		Ok(())
	}
	
	fn returns(statement: &ParsedStatement, declared_return_type: &DataType, check_context: &Context) -> Result<bool, InterpErr> {
		match &statement.kind {
			ParsedStatementKind::Comment (_) 
				| ParsedStatementKind::VariableDeclare { .. }
				| ParsedStatementKind::VariableDeclareSet { .. }
				| ParsedStatementKind::VariableSet { .. }
				| ParsedStatementKind::UserDefinedFuncDeclare { .. }
				| ParsedStatementKind::FuncCall { .. }
			=> Ok(false),
			
			ParsedStatementKind::UserDefinedFuncReturn { return_expr } => {
				let returned_type: DataType = match return_expr {
					Some(expr) => {
						let dt: DataType = expr.check_and_calc_data_type(check_context)?;
						dt
					},
					None => DataType::Primitive (Primitive::None),
				};
				if returned_type.ne(declared_return_type) {
					Err( InterpErr::from(ParsedStatementErr::UserFuncReturnType {
						return_expr_pos: statement.pos(),
						declared_return_type: declared_return_type.clone(),
						returned_type,
					}) )
				} else {
					Ok(true)
				}
			},
			
			ParsedStatementKind::BranchingIfElse { if_bodies, else_body } => {
				let mut all_bodies_return = true;
				
				for body_ref in if_bodies.iter() {
					let mut body_returns = false;
					for st_ref in body_ref.statements() {
						if Self::returns(st_ref, declared_return_type, check_context)? {
							body_returns = true;
						}
					}
					if !body_returns {
						all_bodies_return = false;
					}
				}
				
				let mut body_returns = false;
				for st_ref in else_body.statements() {
					if Self::returns(st_ref, declared_return_type, check_context)? {
						body_returns = true;
					}
				}
				if !body_returns {
					all_bodies_return = false;
				}
				
				return if let DataType::Primitive (Primitive::None) = declared_return_type {
					Ok(true)
				} else {
					Ok(all_bodies_return)
				};
			},
				
			ParsedStatementKind::BranchingWhile { body } => {
				let mut body_returns = false;
				for st_ref in body.statements().iter() {
					if Self::returns(st_ref, declared_return_type, check_context)? {
						body_returns = true;
					}
				}
				
				return if let DataType::Primitive (Primitive::None) = declared_return_type {
					Ok(true)
				} else {
					Ok(body_returns)
				};
			},
		}
	}
	
	pub fn run(&self, context: &mut Context) -> Option<Value> {
		for st_ref in self.statements.iter() {
			match st_ref.run(context) {
				Some(value) => return Some(value),
				None => {},
			}
		}
		
		None
	}
	
	pub fn statements(&self) -> &Vec<ParsedStatement> {
		&self.statements
	}
}

//------------------- ParsedStatementErr --------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParsedStatementErr {
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

impl std::fmt::Display for ParsedStatementErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ParsedStatementErr::UnfinishedBody (_) => 
				write!(f, "Unfinished body"),
			ParsedStatementErr::IfConditionType { .. } => 
				write!(f, "'If' condition expression data type must be bool"),
			ParsedStatementErr::UserFuncReturnType { declared_return_type, returned_type, .. } =>
				write!(f, "User-defined function return type must be '{}', but found '{}'", declared_return_type, returned_type),
			ParsedStatementErr::UserFuncNotAllFuncPathsReturn { .. } =>
				write!(f, "Not all user-defined function paths return"),
		}
	}
}

//------------------- Tests --------------------

#[cfg(test)]
mod tests {
	use super::*;
	use super::super::*;
	use super::super::value::Value;
	use super::super::utils::NameToken;
	use super::super::context::Context;
	use super::super::builtin_func::BuiltinFuncDef;
	use super::super::primitive_type_member_funcs_list::PrimitiveTypeMemberFuncsList;
	
	#[test]
	fn can_parse_comment() {
		let mut statements_iter = ParsedStatementsIter::new();
		
		statements_iter.push_string("//var a: f32;\nvar a: f32;//d".to_string());	
		
		assert_eq!(
			statements_iter.next().unwrap().unwrap().kind, 
			ParsedStatementKind::Comment (String::from("var a: f32;")) 
		);
		
		let nt = new_name_token("a");
		
		assert_eq!(
			statements_iter.next().unwrap().unwrap().kind, 
			ParsedStatementKind::VariableDeclare {
					var_name: nt, 
					data_type: DataType::Primitive (Primitive::Float32), 
			} 
		);
		
		assert_eq!(
			statements_iter.next().unwrap().unwrap().kind, 
			ParsedStatementKind::Comment (String::from("d")) 
		);
		
		assert!(statements_iter.next().is_none());
	}
	
	#[test]
	fn can_make_variable_declare_statement() {
		let mut statements_iter = ParsedStatementsIter::new();
		
		let nt = new_name_token("a");
		
		statements_iter.push_string("var a: f32;".to_string());		
		let st = statements_iter.next().unwrap().unwrap();
		assert_eq!(st.kind, ParsedStatementKind::VariableDeclare {
			var_name: nt.clone(), 
			data_type: DataType::Primitive (Primitive::Float32), 
		});		
		assert!(statements_iter.next().is_none());
		
		statements_iter.push_string("var a: str;".to_string());
		let st = statements_iter.next().unwrap().unwrap();
		assert_eq!(st.kind, ParsedStatementKind::VariableDeclare {
			var_name: nt.clone(), 
			data_type: DataType::Primitive (Primitive::String), 
		});		
		assert!(statements_iter.next().is_none());
		
		statements_iter.push_string("var a: bool;".to_string());
		let st = statements_iter.next().unwrap().unwrap();
		assert_eq!(st.kind, ParsedStatementKind::VariableDeclare {
			var_name: nt.clone(), 
			data_type: DataType::Primitive (Primitive::Bool), 
		});		
		assert!(statements_iter.next().is_none());
	}
	
	#[test]
	fn can_make_variable_declare_set_statement() {
		let mut statements_iter = ParsedStatementsIter::new();
		
		let nt = new_name_token("a");
		let builtin_func_defs = Vec::<BuiltinFuncDef>::new();
		let primitive_type_member_funcs_list = PrimitiveTypeMemberFuncsList::new();
		let context = Context::new(
			&builtin_func_defs,
			&primitive_type_member_funcs_list);
		
		statements_iter.push_string("var a: f32 = 3;".to_string());
		let st = statements_iter.next().unwrap().unwrap();
		match st.kind {
			ParsedStatementKind::VariableDeclareSet {
					var_name, 
					data_type: DataType::Primitive (Primitive::Float32), 
					value_expr
				} 
			if 
				(value_expr.calc(&context) == Value::from(3_f32) &&
				var_name == nt)
				=> {},
			_ => panic!("wrong statement: {:?}", st),
		};		
		assert!(statements_iter.next().is_none());
		
		statements_iter.push_string("var a: str = \"hello\";".to_string());
		let st = statements_iter.next().unwrap().unwrap();
		match st.kind {
			ParsedStatementKind::VariableDeclareSet {
					var_name, 
					data_type: DataType::Primitive (Primitive::String), 
					value_expr
				}
			if 
				(value_expr.calc(&context) == Value::from("hello") &&
				var_name == nt)
				=> {},
			_ => panic!("wrong statement: {:?}", st),
		};		
		assert!(statements_iter.next().is_none());
		
		statements_iter.push_string("var a: bool = True;".to_string());
		let st = statements_iter.next().unwrap().unwrap();
		match st.kind {
			ParsedStatementKind::VariableDeclareSet {
					var_name, 
					data_type: DataType::Primitive (Primitive::Bool), 
					value_expr
				} 
			if 
				(value_expr.calc(&context) == Value::from(true) &&
				var_name == nt)
				=> {},
			_ => panic!("wrong statement: {:?}", st),
		};		
		assert!(statements_iter.next().is_none());
	}
	
	#[test]
	fn can_parse_builtin_funcs_call() {
		let mut st_iter = ParsedStatementsIter::new();
		st_iter.push_string("@print(1.2 + 3.45);".to_string());
		
		let nt = new_name_token("print");
		let builtin_func_defs = Vec::<BuiltinFuncDef>::new();
		let primitive_type_member_funcs_list = PrimitiveTypeMemberFuncsList::new();
		let context = Context::new(
			&builtin_func_defs,
			&primitive_type_member_funcs_list);
		
		match st_iter.next().unwrap().unwrap().kind {
			ParsedStatementKind::FuncCall {
				kind: FuncKind::Builtin,
				func_name, 
				arg_exprs
			} => {
				assert_eq!(nt, func_name);
				
				assert_eq!(arg_exprs.len(), 1);
				
				match arg_exprs[0].calc(&context) {
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
		let mut st_iter = ParsedStatementsIter::new();
		st_iter.push_string("@print();".to_string());
		
		let nt = new_name_token("print");
		
		match st_iter.next().unwrap().unwrap().kind {
			ParsedStatementKind::FuncCall {
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
		let mut statements_iter = ParsedStatementsIter::new();
		
		//--------------------- if ---------------------
		statements_iter.push_string(r#"
		if 2 == 2 { 
			@print("2 == 2"); 
			@print("Cool!"); 
		} 
		@print("End!");
		"#.to_string());	
		
		let st = statements_iter.next().unwrap().unwrap();
		let (if_bodies, else_body) = if let ParsedStatementKind::BranchingIfElse { if_bodies, else_body } = &st.kind {
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
		let (if_bodies, else_body) = if let ParsedStatementKind::BranchingIfElse { if_bodies, else_body } = &st.kind {
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
		let (if_bodies, else_body) = if let ParsedStatementKind::BranchingIfElse { if_bodies, else_body } = &st.kind {
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
		let (if_bodies, else_body) = if let ParsedStatementKind::BranchingIfElse { if_bodies, else_body } = &st.kind {
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
		let (if_bodies, else_body) = if let ParsedStatementKind::BranchingIfElse { if_bodies, else_body } = &st.kind {
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
		let mut statements_iter = ParsedStatementsIter::new();
		
		statements_iter.push_string(r#"
		var a: f32 = 3;
		while 3 > 0 { 
			@print("a is"); 
			@print("a"); 
		} 
		@print("End!");
		"#.to_string());	
			
		let builtin_func_defs = Vec::<BuiltinFuncDef>::new();
		let primitive_type_member_funcs_list = PrimitiveTypeMemberFuncsList::new();
		let context = Context::new(
			&builtin_func_defs,
			&primitive_type_member_funcs_list);
		
		let st = statements_iter.next().unwrap().unwrap();
		if let ParsedStatementKind::VariableDeclareSet {
			var_name,
			data_type,
			value_expr,
		} = st.kind {
			assert_eq!(var_name.value(), "a");
			assert_eq!(data_type, DataType::Primitive (Primitive::Float32));
		
			assert_eq!(
				value_expr.check_and_calc_data_type(&context).unwrap(), 
				DataType::Primitive (Primitive::Float32));
						
			assert_eq!(
				value_expr.calc(&context), 
				Value::from(3_f32));
		} else {
			panic!("Wrong statement: {:?}", st);
		};
		
		let st = statements_iter.next().unwrap().unwrap();
		let body = if let ParsedStatementKind::BranchingWhile { body } = &st.kind { 
			body
		} else {
			panic!("Wrong statement: {:?}", st);
		};
		check_conditional_body(&body, true, &Value::from("a is"), &Value::from("a"));
		
		let st = statements_iter.next().unwrap().unwrap();
		check_is_exit_call(&st);
	}
	
	fn check_is_exit_call(st: &ParsedStatement) {
		let nt_exit = new_name_token("print");
		
		match &st.kind {
			ParsedStatementKind::FuncCall { 
				kind: FuncKind::Builtin,
				func_name, 
				arg_exprs,
			} => {
				assert_eq!(*func_name, nt_exit);
						
				assert_eq!(arg_exprs.len(), 1);
		
				let builtin_func_defs = Vec::<BuiltinFuncDef>::new();
				let primitive_type_member_funcs_list = PrimitiveTypeMemberFuncsList::new();
				let context = Context::new(
					&builtin_func_defs,
					&primitive_type_member_funcs_list);
				
				assert_eq!(
					arg_exprs[0].check_and_calc_data_type(&context).unwrap(), 
					DataType::Primitive (Primitive::String));
					
				assert_eq!(
					arg_exprs[0].calc(&context), 
					Value::from("End!"));
			},
			_ => { dbg!(st); panic!("Not IfElse: "); },
		}
	}
	
	fn check_conditional_body(body: &ConditionalBody, conditional_result: bool, text1: &Value, text2: &Value) {
		let ConditionalBody { condition_expr, statements } = body;
		
		let builtin_func_defs = Vec::<BuiltinFuncDef>::new();
		let primitive_type_member_funcs_list = PrimitiveTypeMemberFuncsList::new();
		let context = Context::new(
			&builtin_func_defs,
			&primitive_type_member_funcs_list);
		
		assert_eq!(
			condition_expr.check_and_calc_data_type(&context).unwrap(), 
			DataType::Primitive (Primitive::Bool));
					
		assert_eq!(
			condition_expr.calc(&context), 
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
	
	fn check_statement(statement: &ParsedStatement, value: &Value) {
		let nt_print = new_name_token("print");
		
		let builtin_func_defs = Vec::<BuiltinFuncDef>::new();
		let primitive_type_member_funcs_list = PrimitiveTypeMemberFuncsList::new();
		let context = Context::new(
			&builtin_func_defs,
			&primitive_type_member_funcs_list);
		
		match &statement.kind {
			ParsedStatementKind::FuncCall { kind: FuncKind::Builtin, func_name, arg_exprs } => {
				assert_eq!(*func_name, nt_print);
				
				assert_eq!(arg_exprs.len(), 1);
				
				assert_eq!(
					arg_exprs[0].check_and_calc_data_type(&context).unwrap(), 
					DataType::Primitive (Primitive::String));
					
				assert_eq!(
					arg_exprs[0].calc(&context), 
					*value);
			},
			_ => panic!("Wrong statement: {:?}", statement),
		}
	}
	
	#[test]
	fn can_parse_user_func_def_statement() {
		let mut statements_iter = ParsedStatementsIter::new();
		
		let builtin_func_defs = Vec::<BuiltinFuncDef>::new();
		let primitive_type_member_funcs_list = PrimitiveTypeMemberFuncsList::new();
		let mut context = Context::new(
			&builtin_func_defs,
			&primitive_type_member_funcs_list);
		
		statements_iter.push_string(r#"
		f add_squared(a: f32, b: f32) -> f32 {
			var a2: f32 = a ^ 2;
			var b2: f32 = b ^ 2;
			return a2 + b2;
		}
		"#.to_string());
		
		let st = statements_iter.next().unwrap().unwrap();
		match st.kind {
			ParsedStatementKind::UserDefinedFuncDeclare { name, args, return_type, body } => {
				assert_eq!(name, new_name_token("add_squared"));
				assert_eq!(
					args,
					vec![
						UserFuncArg::new(new_name_token("a"), DataType::Primitive (Primitive::Float32)),
						UserFuncArg::new(new_name_token("b"), DataType::Primitive (Primitive::Float32)),
					]);
				assert_eq!(return_type, DataType::Primitive (Primitive::Float32));
				
				assert_eq!(body.statements().len(), 3);
				
				context.add_variable(new_name_token("a"), DataType::Primitive (Primitive::Float32), Some(DataType::Primitive (Primitive::Float32).default_value())).unwrap();
				context.add_variable(new_name_token("b"), DataType::Primitive (Primitive::Float32), Some(DataType::Primitive (Primitive::Float32).default_value())).unwrap();
				
				match &body.statements()[0].kind {
					ParsedStatementKind::VariableDeclareSet { var_name, data_type, value_expr } => {
						assert_eq!(*var_name, new_name_token("a2"));
						assert_eq!(data_type.clone(), DataType::Primitive (Primitive::Float32));
						let dt: DataType = value_expr.check_and_calc_data_type(&context).unwrap();
						assert_eq!(dt, DataType::Primitive (Primitive::Float32));
					},
					_ => {
						println!("Wrong statement:");
						dbg!(&body.statements()[0]);
						panic!("Err!");
					},
				}
				
				context.add_variable(new_name_token("a2"), DataType::Primitive (Primitive::Float32), Some(DataType::Primitive (Primitive::Float32).default_value())).unwrap();
				
				match &body.statements()[1].kind {
					ParsedStatementKind::VariableDeclareSet { var_name, data_type, value_expr } => {
						assert_eq!(*var_name, new_name_token("b2"));
						assert_eq!(data_type.clone(), DataType::Primitive (Primitive::Float32));
						let dt: DataType = value_expr.check_and_calc_data_type(&context).unwrap();
						assert_eq!(dt, DataType::Primitive (Primitive::Float32));
					},
					_ => {
						println!("Wrong statement:");
						dbg!(&body.statements()[1]);
						panic!("Err!");
					},
				}
				
				context.add_variable(new_name_token("b2"), DataType::Primitive (Primitive::Float32), Some(DataType::Primitive (Primitive::Float32).default_value())).unwrap();
				
				match &body.statements()[2].kind {
					ParsedStatementKind::UserDefinedFuncReturn { return_expr } => {
						let dt: Option<DataType> = return_expr.as_ref()
							.map(|expr| expr.check_and_calc_data_type(&context).unwrap());
							
						assert_eq!(dt, Some(DataType::Primitive (Primitive::Float32)));
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
	fn can_parse_user_func_def_statement_that_returns_nothing() {
		let mut statements_iter = ParsedStatementsIter::new();
		
		let builtin_func_defs = Vec::<BuiltinFuncDef>::new();
		let primitive_type_member_funcs_list = PrimitiveTypeMemberFuncsList::new();
		let mut context = Context::new(
			&builtin_func_defs,
			&primitive_type_member_funcs_list);
		
		statements_iter.push_string(r#"
		f add_squared(a: f32, b: f32) {
			var a2: f32 = a ^ 2;
			var b2: f32 = b ^ 2;
			@println(a2 + b2);
			return;
		}
		"#.to_string());
		
		let st = statements_iter.next().unwrap().unwrap();
		match st.kind {
			ParsedStatementKind::UserDefinedFuncDeclare { name, args, return_type, body } => {
				assert_eq!(name, new_name_token("add_squared"));
				assert_eq!(
					args,
					vec![
						UserFuncArg::new(new_name_token("a"), DataType::Primitive (Primitive::Float32)),
						UserFuncArg::new(new_name_token("b"), DataType::Primitive (Primitive::Float32)),
					]);
				assert_eq!(return_type, DataType::Primitive (Primitive::None));
				
				assert_eq!(body.statements().len(), 4);
				
				context.add_variable(new_name_token("a"), DataType::Primitive (Primitive::Float32), Some(DataType::Primitive (Primitive::Float32).default_value())).unwrap();
				context.add_variable(new_name_token("b"), DataType::Primitive (Primitive::Float32), Some(DataType::Primitive (Primitive::Float32).default_value())).unwrap();
				
				match &body.statements()[0].kind {
					ParsedStatementKind::VariableDeclareSet { var_name, data_type, value_expr } => {
						assert_eq!(*var_name, new_name_token("a2"));
						assert_eq!(data_type.clone(), DataType::Primitive (Primitive::Float32));
						let dt: DataType = value_expr.check_and_calc_data_type(&context).unwrap();
						assert_eq!(dt, DataType::Primitive (Primitive::Float32));
					},
					_ => {
						println!("Wrong statement:");
						dbg!(&body.statements()[0]);
						panic!("Err!");
					},
				}
				
				context.add_variable(new_name_token("a2"), DataType::Primitive (Primitive::Float32), Some(DataType::Primitive (Primitive::Float32).default_value())).unwrap();
				
				match &body.statements()[1].kind {
					ParsedStatementKind::VariableDeclareSet { var_name, data_type, value_expr } => {
						assert_eq!(*var_name, new_name_token("b2"));
						assert_eq!(data_type.clone(), DataType::Primitive (Primitive::Float32));
						let dt: DataType = value_expr.check_and_calc_data_type(&context).unwrap();
						assert_eq!(dt, DataType::Primitive (Primitive::Float32));
					},
					_ => {
						println!("Wrong statement:");
						dbg!(&body.statements()[1]);
						panic!("Err!");
					},
				}
				
				context.add_variable(new_name_token("b2"), DataType::Primitive (Primitive::Float32), Some(DataType::Primitive (Primitive::Float32).default_value())).unwrap();
				
				match &body.statements()[2].kind {
					ParsedStatementKind::FuncCall { kind, func_name, arg_exprs } => {
						assert_eq!(*kind, FuncKind::Builtin);
						assert_eq!(*func_name, new_name_token("println"));
						assert_eq!(arg_exprs.len(), 1);
						let dt: DataType = arg_exprs[0].check_and_calc_data_type(&context).unwrap();
						assert_eq!(dt, DataType::Primitive (Primitive::Float32));
					},
					_ => {
						println!("Wrong statement:");
						dbg!(&body.statements()[2]);
						panic!("Err!");
					},
				}
				
				match &body.statements()[3].kind {
					ParsedStatementKind::UserDefinedFuncReturn { return_expr } => {
						let dt: Option<DataType> = return_expr.as_ref()
							.map(|expr| expr.check_and_calc_data_type(&context).unwrap());
							
						assert_eq!(dt, None);
					},
					_ => {
						println!("Wrong statement:");
						dbg!(&body.statements()[3]);
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
			let mut statements_iter = ParsedStatementsIter::new();
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
		NameToken::new_with_pos(name.to_string(), CodePos::from(CharPos::new()))
	}
}