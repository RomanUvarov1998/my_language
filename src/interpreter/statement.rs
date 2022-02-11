use super::token::*;
use super::expr::{Expr, ExprContextKind};
use super::InterpErr;
use super::value::Value;
use super::data_type::{DataType, BuiltinType};
use super::var_data::VarErr;
use super::user_func::UserFuncArg;
use super::utils::{CharPos, CodePos, NameToken};
use super::context::Context;
use super::struct_def::StructFieldDef;
use std::rc::Rc;

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
	
	fn parse_variable_set_or_func_call(&mut self) -> Result<Statement, InterpErr> {
		let left_expr = Expr::new(
			&mut self.tokens_iter,
			ExprContextKind::LeftAssignOperand)?;
		
		match self.tokens_iter.peek_or_end_reached_err()? {
			Token { content: TokenContent::StatementOp (StatementOp::Semicolon), pos } => {
				let pos = *pos;
				self.tokens_iter.skip_or_end_reached_err()?;
				Ok( Statement {
					pos: CodePos::new(left_expr.pos().begin(), pos.end()),
					kind: StatementKind::FuncCall { left_expr },
				} )
			},
			
			Token { content: TokenContent::Operator (Operator::Assign), .. } => {
				self.tokens_iter.skip_or_end_reached_err()?;
				
				let right_expr = Expr::new(
					&mut self.tokens_iter,
					ExprContextKind::RightAssignOperand)?;
				
				self.tokens_iter.next_expect_semicolon()?;
				
				Ok( Statement {
					pos: CodePos::new(left_expr.pos().begin(), right_expr.pos().end()),
					kind: StatementKind::VariableSet { 
						left_expr,
						right_expr,
					},
				} )
			},
			
			found @ _ => return Err( InterpErr::from ( TokenErr::ExpectedButFound { 
										expected: vec![
											TokenContent::Bracket(Bracket::Left),
											TokenContent::Operator (Operator::Assign),
										], 
										found: found.clone(),
									} ) ),
		}
	}

	fn parse_varable_declaration(&mut self, begin_pos: CharPos) -> Result<Statement, InterpErr> {
		self.tokens_iter.expect_keyword(Keyword::Var)?;
		
		let var_name: NameToken = NameToken::from_or_err(self.tokens_iter.next_or_end_reached_err()?)?;
		
		self.tokens_iter.next_expect_colon()?;
		
		let data_type_name: NameToken = NameToken::from_or_err(self.tokens_iter.next_or_end_reached_err()?)?;
		
		self.tokens_iter.next_expect_assign_operator()?;
		
		let value_expr = Expr::new(
			&mut self.tokens_iter, 
			ExprContextKind::RightAssignOperand)?;

		self.tokens_iter.next_expect_semicolon()?;
		
		let pos = CodePos::new(begin_pos, value_expr.pos().end());
		Ok ( Statement {
			kind: StatementKind::VariableDeclareSet {
				var_name, 
				data_type_name, 
				value_expr,
			},
			pos,
		}	)
	}
	
	fn parse_if_else_statement(&mut self, begin_pos: CharPos) -> Result<Statement, InterpErr> {
		self.tokens_iter.expect_keyword(Keyword::If)?;
		
		let mut if_bodies = Vec::<ConditionalBody>::new();
		let mut else_body = UnconditionalBody {
			statements: Vec::new(),
		};
		
		loop {
			self.tokens_iter.next_expect_left_bracket()?;
			
			let condition_expr = Expr::new(
				&mut self.tokens_iter,
				ExprContextKind::IfCondition)?;
			
			self.tokens_iter.next_expect_right_bracket()?;
			
			let mut statements = Vec::<Statement>::new();
			self.parse_body(&mut statements)?;
			
			if_bodies.push(ConditionalBody {
				condition_expr,
				statements,
			});
			
			if let Some(Token { content: TokenContent::Keyword (Keyword::Else), .. }) = self.tokens_iter.peek()? {
				self.tokens_iter.skip_or_end_reached_err()?;
				
				if let Some(Token { content: TokenContent::Keyword (Keyword::If), .. }) = self.tokens_iter.peek()? {
					self.tokens_iter.skip_or_end_reached_err()?;
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
		self.tokens_iter.expect_keyword(Keyword::While)?;
		
		self.tokens_iter.next_expect_left_bracket()?;
		
		let condition_expr = Expr::new(
			&mut self.tokens_iter,
			ExprContextKind::IfCondition)?;
			
		self.tokens_iter.next_expect_right_bracket()?;
		
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
						self.tokens_iter.skip_or_end_reached_err()?;
						break Ok(());
					},
					_ => match self.parse_next_statement() {
						Some(statement_result) => body.push(statement_result?),
						None => break Err(StatementErr::UnfinishedBody (self.tokens_iter.pos()).into()),
					},
				}
			}
		}
	
	fn parse_user_func_def(&mut self, begin_pos: CharPos) -> Result<Statement, InterpErr> {
		self.tokens_iter.expect_keyword(Keyword::F)?;
		
		let name: NameToken = NameToken::from_or_err(self.tokens_iter.next_or_end_reached_err()?)?;
		
		self.tokens_iter.next_expect_left_bracket()?;
		
		let mut args = Vec::<ParsedFuncArgDef>::new();
		if let TokenContent::Bracket (Bracket::Right) = self.tokens_iter.peek_or_end_reached_err()?.content() {
			self.tokens_iter.skip_or_end_reached_err()?;
		} else {
			loop {
				let arg_name: NameToken = NameToken::from_or_err(self.tokens_iter.next_or_end_reached_err()?)?;
				
				self.tokens_iter.next_expect_colon()?;
				
				let data_type_name: NameToken = NameToken::from_or_err(self.tokens_iter.next_or_end_reached_err()?)?;
				
				args.push(ParsedFuncArgDef::new(arg_name, data_type_name));
				
				match self.tokens_iter.next_or_end_reached_err()? {
					Token { content: TokenContent::StatementOp (StatementOp::Comma), .. } => continue,
					Token { content: TokenContent::Bracket (Bracket::Right), .. } => break,
					found @ _ => return Err(TokenErr::ExpectedButFound {
						expected: vec![
							TokenContent::StatementOp (StatementOp::Comma),
							TokenContent::Bracket (Bracket::Right),
						],
						found,
					}.into()),
				}
			}
		}
		
		let return_type_name: Option<NameToken> = match self.tokens_iter.peek_or_end_reached_err()? {
			Token { content: TokenContent::StatementOp (StatementOp::ThinArrow), .. } => { // function returns value
				self.tokens_iter.skip_or_end_reached_err()?; // skip ThinArrow
				let return_type_name: NameToken = NameToken::from_or_err(self.tokens_iter.next_or_end_reached_err()?)?;
				Some(return_type_name)
			},
			Token { content: TokenContent::Bracket (Bracket::LeftCurly), .. } => { // function doesn't return value
				None
			},
			found @ _ => return Err(TokenErr::ExpectedButFound {
				expected: vec![
					TokenContent::StatementOp (StatementOp::ThinArrow),
					TokenContent::Bracket (Bracket::LeftCurly),
				], 
				found: found.clone(),
			}.into()),
		};
		
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
				return_type_name,
				body: ReturningBody::new(statements),
			},
			pos,
		} )
	}

	fn parse_return_statement(&mut self, return_keyword_pos: CodePos) -> Result<Statement, InterpErr> {
		self.tokens_iter.expect_keyword(Keyword::Return)?;
		
		let (return_expr, pos): (Option<Expr>, CodePos) = 
			if let TokenContent::StatementOp (StatementOp::Semicolon) = 
				self.tokens_iter.peek_or_end_reached_err()?.content() 
			{
				self.tokens_iter.skip_or_end_reached_err()?; // skip semicolon
				(None, return_keyword_pos)
			} else {
				let expr: Expr = Expr::new(
					&mut self.tokens_iter, 
					ExprContextKind::ToReturn)?;
				self.tokens_iter.next_expect_semicolon()?;
				let pos_end: CharPos = expr.pos().end();
				(Some(expr), CodePos::new(return_keyword_pos.begin(), pos_end))
			};
		
		Ok ( Statement {
			kind: StatementKind::UserDefinedFuncReturn { return_expr },
			pos,
		} )
	}

	fn parse_struct_declaration(&mut self, struct_keyword_pos: CodePos) -> Result<Statement, InterpErr> {
		self.tokens_iter.expect_keyword(Keyword::Struct)?;
		
		let new_type_name: NameToken = NameToken::from_or_err(self.tokens_iter.next_or_end_reached_err()?)?;
		
		self.tokens_iter.next_expect_left_curly_bracket()?;
		
		let mut fields = Vec::<ParsedStructFieldDef>::new();
		
		let last_pos: CharPos = loop {
			match self.tokens_iter.next_or_end_reached_err()? {
				Token { content: TokenContent::Bracket (Bracket::RightCurly), pos } => break pos.end(),
				Token { content: TokenContent::Name (name), pos } => {
					let field_name = NameToken::new_with_pos(name, pos, false);
					self.tokens_iter.next_expect_colon()?;
					let data_type_name = NameToken::from_or_err(self.tokens_iter.next_or_end_reached_err()?)?;
					fields.push(ParsedStructFieldDef::new(field_name, data_type_name));
					
					match self.tokens_iter.next_or_end_reached_err()? {
						Token { content: TokenContent::StatementOp (StatementOp::Comma), .. } => continue,
						Token { content: TokenContent::Bracket (Bracket::RightCurly), pos } => break pos.end(),
						found @ _ => return Err(TokenErr::ExpectedButFound {
							expected: vec![
								TokenContent::StatementOp (StatementOp::Comma),
								TokenContent::Bracket (Bracket::RightCurly),
							],
							found,
						}.into()),
					}
				},
				found @ _ => return Err(TokenErr::ExpectedButFound {
					expected: vec![
						TokenContent::Bracket (Bracket::RightCurly),
						TokenContent::Name (String::from("<name>")),
					],
					found,
				}.into()),
			}
		};
		
		Ok ( Statement {
			kind: StatementKind::UserStructDeclare {
				new_type_name,
				fields,
			},
			pos: CodePos::new(struct_keyword_pos.begin(), last_pos),
		} )
	}

	fn parse_next_statement(&mut self) -> Option<Result<Statement, InterpErr>> {
		let first: &Token = match self.tokens_iter.peek() {
			Ok(tok) => tok?,
			Err(err) => return Some(Err(err.into())),
		};
		
		let begin_pos: CharPos = first.pos().begin();
		
		let statement_result: Result<Statement, InterpErr> = match first {
			// iterator doesn't give comment tokens by default
			Token { content: TokenContent::Comment (_), .. } => unreachable!(),
				
			Token { content: TokenContent::Keyword ( Keyword::Var ), .. } =>
				self.parse_varable_declaration(begin_pos),	
				
			Token { content: TokenContent::Keyword ( Keyword::If ), .. } =>
				self.parse_if_else_statement(begin_pos),	
				
			Token { content: TokenContent::Keyword ( Keyword::While ), .. } =>
				self.parse_while_statement(begin_pos),	
				
			Token { content: TokenContent::Keyword ( Keyword::F ), .. } =>
				self.parse_user_func_def(begin_pos),	
				
			Token { content: TokenContent::Keyword ( Keyword::Return ), pos } => {
				let pos = *pos;
				self.parse_return_statement(pos)
			},
				
			Token { content: TokenContent::Keyword ( Keyword::Struct ), pos } => {
				let pos = *pos;
				self.parse_struct_declaration(pos)
			},	
			
			Token { content: TokenContent::Name(_), .. } 
			| Token { content: TokenContent::BuiltinName(_), .. }
			=> 
				self.parse_variable_set_or_func_call(),
				
			found @ _ => 
				Err(TokenErr::ExpectedButFound { 
					expected: vec![
						TokenContent::Keyword ( Keyword::Var ),
						TokenContent::Name( String::from("<name>") ),
					], 
					found: found.clone(),
				}.into()),
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

//------------------- Statement --------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Statement {
	pos: CodePos,
	kind: StatementKind,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StatementKind {
	VariableDeclareSet { 
		var_name: NameToken, 
		data_type_name: NameToken, 
		value_expr: Expr,
	},
	UserStructDeclare {
		new_type_name: NameToken,
		fields: Vec<ParsedStructFieldDef>,
	},
	UserDefinedFuncDeclare {
		name: NameToken,
		args: Vec<ParsedFuncArgDef>,
		return_type_name: Option<NameToken>,
		body: ReturningBody,
	},
	
	VariableSet { 
		left_expr: Expr, 
		right_expr: Expr
	},
	FuncCall {
		left_expr: Expr,
	},
	
	UserDefinedFuncReturn { 
		return_expr: Option<Expr>,
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
	pub fn new(pos: CodePos, kind: StatementKind) -> Self {
		Self {
			pos, 
			kind,
		}
	}
	
	pub fn check(
		&self, 
		check_context: &mut Context
	) -> Result<(), InterpErr> 
	{
		match &self.kind {	
			StatementKind::VariableDeclareSet { var_name, data_type_name, value_expr } => {
				let data_type: DataType = check_context.find_type_by_name(data_type_name)?;
				let default_value = data_type.default_value();
				check_context.add_variable(var_name.clone(), data_type.clone(), default_value)?;
				
				let expr_data_type: DataType = value_expr.check_as_rhs_and_calc_data_type(check_context)?;
				if data_type.clone() != expr_data_type {
					return Err(VarErr::WrongType { 
						value_data_type: expr_data_type, 
						variable_type: data_type.clone(),
						var_name: var_name.clone(),
					}.into());
				}
			},
			
			StatementKind::UserStructDeclare { new_type_name, fields } => {
				let mut typed_fields = Vec::<StructFieldDef>::new();
				for field in fields.iter() {
					let data_type: DataType = check_context.find_type_by_name(&field.data_type_name)?;					
					typed_fields.push(StructFieldDef::new(
						field.name.clone(), data_type));
				}
				check_context.add_user_struct(new_type_name.clone(), typed_fields)?;
			},
		
			StatementKind::UserDefinedFuncDeclare { name, args, return_type_name, body } => {
				let return_type: DataType = match return_type_name {
					Some(nt) => check_context.find_type_by_name(nt)?,
					None => DataType::Builtin (BuiltinType::None),
				};
				
				let mut typed_args = Vec::<UserFuncArg>::new();
				for arg in args {
					// duplicating args are checked when copying their values into new stack frame context
					typed_args.push(UserFuncArg::new(
						arg.name.clone(),
						check_context.find_type_by_name(&arg.data_type_name)?));
				}
				
				check_context.add_user_func(
					name.clone(), 
					typed_args, 
					return_type.clone(), 
					body.clone())?;
					
				let user_func_def = check_context.find_user_func_def(name).unwrap();
				
				let mut next_check_context = check_context.new_stack_frame_context();
				
				for arg_ref in user_func_def.args().iter() {
					next_check_context.add_variable(
						arg_ref.name().clone(), 
						arg_ref.data_type().clone(),
						arg_ref.data_type().default_value())?;
				}
				
				body.check(&return_type, &mut next_check_context)?;
			},
			
			
			StatementKind::VariableSet { left_expr, right_expr } => {
				let right_expr_data_type: DataType = right_expr.check_as_rhs_and_calc_data_type(check_context)?;
				
				let left_expr_data_type: DataType = left_expr.check_as_lhs_and_calc_data_type(check_context)?;
				
				if left_expr_data_type != right_expr_data_type {
					return Err(StatementErr::WrongAssignedType { 
						lhs_data_type: left_expr_data_type,
						rhs_data_type: right_expr_data_type, 
						statement_pos: self.pos(),
					}.into());
				}
			},
			
			StatementKind::FuncCall { left_expr } => {
				left_expr.check_as_standalone(check_context)?;
			},
			
			
			StatementKind::UserDefinedFuncReturn { return_expr } => {
				if let Some(ref expr) = return_expr {
					expr.check_as_rhs_and_calc_data_type(check_context)?;
				}
			},
		
			StatementKind::BranchingIfElse { if_bodies, else_body } => {
				for body in if_bodies.iter() {
					body.check(check_context)?;
				}
				
				else_body.check(check_context)?;
			},
			
			StatementKind::BranchingWhile { body } => body.check(check_context)?,
		}
		Ok(())
	}
	
	pub fn run(&self, context: &mut Context) -> Option<Value> {
		match &self.kind {					
			StatementKind::VariableDeclareSet { var_name, data_type_name, value_expr } => {
				let data_type: DataType = context.find_type_by_name(data_type_name).unwrap();
				let value: Value = value_expr.calc_as_rhs(context);
				context.add_variable(
					var_name.clone(), 
					data_type,
					value).unwrap();
			},
				
			StatementKind::UserStructDeclare { new_type_name, fields } => {
				let mut typed_fields = Vec::<StructFieldDef>::new();
				for field in fields.iter() {
					let data_type: DataType = context.find_type_by_name(&field.data_type_name).unwrap();					
					typed_fields.push(StructFieldDef::new(
						field.name.clone(), data_type));
				}
				context.add_user_struct(new_type_name.clone(), typed_fields).unwrap();
			},
			
			StatementKind::UserDefinedFuncDeclare { name, args, return_type_name, body } => {
				let return_type: DataType = match return_type_name {
					Some(nt) => context.find_type_by_name(nt).unwrap(),
					None => DataType::Builtin (BuiltinType::None),
				};
				
				let mut typed_args = Vec::<UserFuncArg>::new();
				for arg in args {
					// duplicating args are checked when copying their values into new stack frame context
					typed_args.push(UserFuncArg::new(
						arg.name.clone(),
						context.find_type_by_name(&arg.data_type_name).unwrap()));
				}
				
				context.add_user_func(name.clone(), typed_args, return_type, body.clone()).unwrap();
			},
			
			
			StatementKind::VariableSet { left_expr, right_expr } => {
				let value: Value = right_expr.calc_as_rhs(context);
				left_expr.calc_as_lhs_and_set(value, context);
			},
			
			StatementKind::FuncCall { left_expr } => {
				left_expr.run_as_standalone(context);
			},
			
			
			StatementKind::UserDefinedFuncReturn { return_expr } => {
				return match return_expr {
					Some(expr) => {
						let value: Value = expr.calc_as_rhs(context);
						Some(value)
					},
					None => Some(Value::None),
				}
			},
			
			StatementKind::BranchingIfElse { if_bodies, else_body } => {
				for body in if_bodies {
					if let (true, value_op) = body.run_if_true(context) {
						return value_op;
					}
				}
				
				return else_body.run(context);
			},
			
			StatementKind::BranchingWhile { body } => {
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

	pub fn kind(&self) -> &StatementKind {
		&self.kind
	}
}

impl std::fmt::Display for Statement {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "[{}, {}] ", self.pos().begin(), self.pos().end())?;
		match &self.kind {				
			StatementKind::VariableDeclareSet { var_name, data_type_name, .. } => writeln!(f, "declare {}: {} = expr", var_name, data_type_name),
				
			StatementKind::VariableSet { .. } => writeln!(f, "expr = expr;"),
			
			StatementKind::FuncCall { .. } => writeln!(f, "expr;"),
				
			StatementKind::UserDefinedFuncDeclare { name, return_type_name, body, .. } => {
				write!(f, "define user func {}(...)", name)?;
				
				match return_type_name {
					Some(nt) => writeln!(f, " -> {}", nt.value())?,
					None => writeln!(f, "")?,
				}
				
				for st_ref in body.statements().iter() {
					writeln!(f, "\t{}", st_ref)?;
				}
				
				writeln!(f, "------------------")
			},
			
			StatementKind::UserDefinedFuncReturn { .. } => writeln!(f, "return ..."),
		
			StatementKind::UserStructDeclare { new_type_name, fields } => 
				writeln!(f, "struct '{}' with fields {:?}", new_type_name.value(), fields),
		
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

//------------------------- ConditionalBody -----------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ConditionalBody {
	condition_expr: Expr,
	statements: Vec<Statement>,
}

impl ConditionalBody {
	pub fn new(condition_expr: Expr, statements: Vec<Statement>) -> Self {
		Self {
			condition_expr,
			statements,
		}
	}
	
	pub fn condition_expr(&self) -> &Expr {
		&self.condition_expr
	}
	
	pub fn statements(&self) -> &Vec<Statement> {
		&self.statements
	}
	
	pub fn check(&self, check_context: &mut Context) -> Result<(), InterpErr> {
		match self.condition_expr.check_as_rhs_and_calc_data_type(check_context)? {
			DataType::Builtin (BuiltinType::Bool) => {},
			_ => return Err(StatementErr::IfConditionType { 
										pos: self.condition_expr().pos(),
									}.into()),
		}
		
		check_context.push_scope();
		for st_ref in self.statements.iter() {
			st_ref.check(check_context)?;
		}
		check_context.pop_scope();
		
		Ok(())
	}
	
	pub fn run_if_true(&self, context: &mut Context) -> (bool, Option<Value>) {
		match self.condition_expr().calc_as_rhs(context) {
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
	statements: Vec<Statement>,
}

impl UnconditionalBody {
	pub fn new(statements: Vec<Statement>) -> Self {
		Self {
			statements,
		}
	}
	
	pub fn statements(&self) -> &Vec<Statement> {
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
	statements: Vec<Statement>,
}

impl ReturningBody {
	pub fn new(statements: Vec<Statement>) -> Self {
		Self {
			statements,
		}
	}
	
	pub fn check(&self, return_type: &DataType, check_context: &mut Context) -> Result<(), InterpErr> {
		for st_ref in self.statements.iter() {
			st_ref.check(check_context)?;
		}
		
		let mut has_return = false; // at least one statement on 1st level returns
		for st_ref in self.statements().iter() {
			if Self::returns(st_ref, return_type, check_context)? {
				has_return = true;
			}
		}
		
		if !has_return && return_type.ne(&DataType::Builtin (BuiltinType::None)) {
			return Err(StatementErr::UserFuncNotAllFuncPathsReturn {
				last_statement_pos: CodePos::from(CharPos::new()),
			}.into());
		}
		
		Ok(())
	}
	
	fn returns(statement: &Statement, declared_return_type: &DataType, check_context: &Context) -> Result<bool, InterpErr> {
		match &statement.kind {
			StatementKind::VariableDeclareSet { .. }
				| StatementKind::VariableSet { .. }
				| StatementKind::UserDefinedFuncDeclare { .. }
				| StatementKind::FuncCall { .. }
				| StatementKind::UserStructDeclare { .. }
			=> Ok(false),
			
			StatementKind::UserDefinedFuncReturn { return_expr } => {
				let returned_type: DataType = match return_expr {
					Some(expr) => {
						let dt: DataType = expr.check_as_rhs_and_calc_data_type(check_context)?;
						dt
					},
					None => DataType::Builtin (BuiltinType::None),
				};
				if returned_type.ne(declared_return_type) {
					Err(StatementErr::UserFuncReturnType {
						return_expr_pos: statement.pos(),
						declared_return_type: declared_return_type.clone(),
						returned_type,
					}.into())
				} else {
					Ok(true)
				}
			},
			
			StatementKind::BranchingIfElse { if_bodies, else_body } => {
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
				
				return if let DataType::Builtin (BuiltinType::None) = declared_return_type {
					Ok(true)
				} else {
					Ok(all_bodies_return)
				};
			},
				
			StatementKind::BranchingWhile { body } => {
				let mut body_returns = false;
				for st_ref in body.statements().iter() {
					if Self::returns(st_ref, declared_return_type, check_context)? {
						body_returns = true;
					}
				}
				
				return if let DataType::Builtin (BuiltinType::None) = declared_return_type {
					Ok(true)
				} else {
					Ok(body_returns)
				};
			},
		}
	}
	
	pub fn run(&self, context: &mut Context) -> Value {
		for st_ref in self.statements.iter() {
			match st_ref.run(context) {
				Some(value) => return value,
				None => {},
			}
		}
		
		Value::None
	}
	
	pub fn statements(&self) -> &Vec<Statement> {
		&self.statements
	}
}

//------------------- ParsedFuncArgDef --------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ParsedFuncArgDef {
	name: NameToken,
	data_type_name: NameToken,
}

impl ParsedFuncArgDef {
	pub fn new(name: NameToken, data_type_name: NameToken) -> Self {
		Self {
			name,
			data_type_name,
		}
	}
	
	pub fn name(&self) -> &NameToken {
		&self.name
	}
	
	pub fn data_type_name(&self) -> &NameToken {
		&self.data_type_name
	}
}

//------------------- ParsedStructFieldDef --------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ParsedStructFieldDef {
	name: NameToken,
	data_type_name: NameToken,
}

impl ParsedStructFieldDef {
	pub fn new(name: NameToken, data_type_name: NameToken) -> Self {
		Self {
			name,
			data_type_name,
		}
	}
	
	pub fn name(&self) -> &NameToken {
		&self.name
	}
	
	pub fn data_type_name(&self) -> &NameToken {
		&self.data_type_name
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
	WrongAssignedType {
		rhs_data_type: DataType,
		lhs_data_type: DataType,
		statement_pos: CodePos,
	}
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
			StatementErr::WrongAssignedType { rhs_data_type, lhs_data_type, .. } =>
				write!(f, "Value of type '{}' can't be assigned to value of type '{}'",
					rhs_data_type, lhs_data_type),
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
	use super::super::primitive_type_member_builtin_funcs_list::PrimitiveTypeMemberBuiltinFuncsList;
	
	#[test]
	fn does_not_can_parse_comment_by_default() {
		let mut statements_iter = StatementsIter::new();
		
		statements_iter.push_string(r#"
		//var a: @f32;
		var a: @f32 = 3;//d
		"#.to_string());
		
		let builtin_func_defs = Vec::<BuiltinFuncDef>::new();
		let primitive_type_member_funcs_list = PrimitiveTypeMemberBuiltinFuncsList::new();
		let context = Context::new(
			&builtin_func_defs,
			&primitive_type_member_funcs_list,
			Vec::<StructDef>::new());
		
		let st = statements_iter.next().unwrap().unwrap();
		match st.kind {
			StatementKind::VariableDeclareSet {
					var_name, 
					data_type_name,
					value_expr
				} => {
					assert_eq!(data_type_name, new_name_token("f32", true));
					assert_eq!(value_expr.calc_as_rhs(&context), Value::from(3_f32));
					assert_eq!(var_name, new_name_token("a", false));
				},
			_ => panic!("wrong statement: {:?}", st),
		};
		
		assert!(statements_iter.next().is_none());
	}
		
	#[test]
	fn can_make_variable_declare_set_statement() {
		let mut statements_iter = StatementsIter::new();
		
		let nt = new_name_token("a", false);
		let builtin_func_defs = Vec::<BuiltinFuncDef>::new();
		let primitive_type_member_funcs_list = PrimitiveTypeMemberBuiltinFuncsList::new();
		let context = Context::new(
			&builtin_func_defs,
			&primitive_type_member_funcs_list,
			Vec::<StructDef>::new());
		
		statements_iter.push_string("var a: @f32 = 3;".to_string());
		let st = statements_iter.next().unwrap().unwrap();
		match st.kind {
			StatementKind::VariableDeclareSet {
					var_name, 
					data_type_name,
					value_expr
				} => {
					assert_eq!(data_type_name, new_name_token("f32", true));
					assert_eq!(value_expr.calc_as_rhs(&context), Value::from(3_f32));
					assert_eq!(var_name, nt);
				},
			_ => panic!("wrong statement: {:?}", st),
		};
		assert!(statements_iter.next().is_none());
		
		statements_iter.push_string("var a: @str = \"hello\";".to_string());
		let st = statements_iter.next().unwrap().unwrap();
		match st.kind {
			StatementKind::VariableDeclareSet {
					var_name, 
					data_type_name, 
					value_expr
				} => {
					assert_eq!(data_type_name, new_name_token("str", true));
					assert_eq!(value_expr.calc_as_rhs(&context), Value::from("hello"));
					assert_eq!(var_name, nt);
				},
			_ => panic!("wrong statement: {:?}", st),
		};		
		assert!(statements_iter.next().is_none());
		
		statements_iter.push_string("var a: @bool = True;".to_string());
		let st = statements_iter.next().unwrap().unwrap();
		match st.kind {
			StatementKind::VariableDeclareSet {
					var_name, 
					data_type_name, 
					value_expr
				} => {
					assert_eq!(data_type_name, new_name_token("bool", true));
					assert_eq!(value_expr.calc_as_rhs(&context), Value::from(true));
					assert_eq!(var_name, nt);
				},
			_ => panic!("wrong statement: {:?}", st),
		};		
		assert!(statements_iter.next().is_none());
	}
	
	#[test]
	fn can_parse_builtin_funcs_call() {
		let mut st_iter = StatementsIter::new();
		st_iter.push_string("@print(1.2 + 3.45);".to_string());
		
		match st_iter.next().unwrap().unwrap().kind {
			StatementKind::FuncCall { .. } => {},
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
		
		match st_iter.next().unwrap().unwrap().kind {
			StatementKind::FuncCall { .. } => {},
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
		if (2 == 2) { 
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
				check_conditional_body(cb1, true);
			},
			found @ _ => panic!("Wrong if_bodies: {:?}", found),
		}
		assert_eq!(else_body.statements().len(), 0);
		
		let st = statements_iter.next().unwrap().unwrap();
		check_is_exit_call(&st);
		
		//--------------------- if-else --------------------- 
		statements_iter.push_string(r#"
		if (3 == 1 + 2) { 
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
				check_conditional_body(cb1, true);
			},
			found @ _ => { dbg!(found); panic!("Wrong if_bodies:"); },
		}
		check_unconditional_body(&else_body);
		
		let st = statements_iter.next().unwrap().unwrap();
		check_is_exit_call(&st);
		
		//--------------------- if-elseif ---------------------
		statements_iter.push_string(r#"
		if (3 == 1 + 2) { 
			@print("3 == 1 + 2"); 
			@print("Cool!"); 
		} else if (45 * 6 == 6 * 45) { 
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
				check_conditional_body(cb1, true);
				check_conditional_body(cb2, true);
			},
			found @ _ => { dbg!(found); panic!("Wrong if_bodies:"); },
		}
		assert_eq!(else_body.statements().len(), 0);
		
		let st = statements_iter.next().unwrap().unwrap();
		check_is_exit_call(&st);
		
		//--------------------- if-elseif-else ---------------------
		statements_iter.push_string(r#"
		if (3 == 1 + 2) { 
			@print("3 == 1 + 2"); 
			@print("Cool!"); 
		} else if ((45 * 6 == 6 * 45) land False) { 
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
				check_conditional_body(cb1, true);
				check_conditional_body(cb2, false);
			},
			found @ _ => { dbg!(found); panic!("Wrong if_bodies:"); },
		}
		check_unconditional_body(&else_body);
		
		let st = statements_iter.next().unwrap().unwrap();
		check_is_exit_call(&st);
		
		//--------------------- if-elseif-elseif-else ---------------------
		statements_iter.push_string(r#"
		if (3 == 1 + 2) { 
			@print("3 == 1 + 2"); 
			@print("Cool!"); 
		} else if ((45 * 6 == 6 * 45) land False) { 
			@print("45 * 6 == 6 * 45"); 
			@print("Nice!"); 
		} else if ((10 == 7) lor True) { 
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
				check_conditional_body(cb1, true);
				check_conditional_body(cb2, false);
				check_conditional_body(cb3, true);
			},
			found @ _ => { dbg!(found); panic!("Wrong if_bodies:"); },
		}
		check_unconditional_body(&else_body);
		
		let st = statements_iter.next().unwrap().unwrap();
		check_is_exit_call(&st);
		
		assert!(statements_iter.next().is_none());
	}
	
	#[test]
	fn can_parse_branching_while_statement() {
		let mut statements_iter = StatementsIter::new();
		
		statements_iter.push_string(r#"
		var a: @f32 = 3;
		while (3 > 0) { 
			@print("a is"); 
			@print("a"); 
		} 
		@print("End!");
		"#.to_string());	
			
		let builtin_func_defs = Vec::<BuiltinFuncDef>::new();
		let primitive_type_member_funcs_list = PrimitiveTypeMemberBuiltinFuncsList::new();
		let context = Context::new(
			&builtin_func_defs,
			&primitive_type_member_funcs_list,
			Vec::<StructDef>::new());
		
		let st = statements_iter.next().unwrap().unwrap();
		if let StatementKind::VariableDeclareSet {
			var_name,
			data_type_name,
			value_expr,
		} = st.kind {
			assert_eq!(var_name.value(), "a");
			assert_eq!(data_type_name, new_name_token("f32", true));
		
			assert_eq!(
				value_expr.check_as_rhs_and_calc_data_type(&context).unwrap(), 
				DataType::Builtin (BuiltinType::Float32));
						
			assert_eq!(
				value_expr.calc_as_rhs(&context), 
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
		check_conditional_body(&body, true);
		
		let st = statements_iter.next().unwrap().unwrap();
		check_is_exit_call(&st);
	}
	
	fn check_is_exit_call(st: &Statement) {
		match &st.kind {
			StatementKind::FuncCall { .. } => {},
			_ => { dbg!(st); panic!("Not IfElse: "); },
		}
	}
	
	fn check_conditional_body(body: &ConditionalBody, conditional_result: bool) {
		let ConditionalBody { condition_expr, statements } = body;
		
		let builtin_func_defs = Vec::<BuiltinFuncDef>::new();
		let primitive_type_member_funcs_list = PrimitiveTypeMemberBuiltinFuncsList::new();
		let context = Context::new(
			&builtin_func_defs,
			&primitive_type_member_funcs_list,
			Vec::<StructDef>::new());
		
		assert_eq!(
			condition_expr.check_as_rhs_and_calc_data_type(&context).unwrap(), 
			DataType::Builtin (BuiltinType::Bool));
					
		assert_eq!(
			condition_expr.calc_as_rhs(&context), 
			Value::Bool(conditional_result));
		
		assert_eq!(statements.len(), 2);
		
		check_statement(&statements[0]);
		check_statement(&statements[1]);
	}
	
	fn check_unconditional_body(body: &UnconditionalBody) {
		let UnconditionalBody { statements } = body;
		
		assert_eq!(statements.len(), 2);
		
		check_statement(&statements[0]);
		check_statement(&statements[1]);
	}
	
	fn check_statement(statement: &Statement) {
		match &statement.kind {
			StatementKind::FuncCall { .. } => {},
			_ => panic!("Wrong statement: {:?}", statement),
		}
	}
	
	#[test]
	fn can_parse_user_func_def_statement() {
		let mut statements_iter = StatementsIter::new();
		
		let builtin_func_defs = Vec::<BuiltinFuncDef>::new();
		let primitive_type_member_funcs_list = PrimitiveTypeMemberBuiltinFuncsList::new();
		let mut context = Context::new(
			&builtin_func_defs,
			&primitive_type_member_funcs_list,
			Vec::<StructDef>::new());
		
		statements_iter.push_string(r#"
		f add_squared(a: @f32, b: @f32) -> @f32 {
			var a2: @f32 = a ^ 2;
			var b2: @f32 = b ^ 2;
			return a2 + b2;
		}
		"#.to_string());
		
		let st = statements_iter.next().unwrap().unwrap();
		match st.kind {
			StatementKind::UserDefinedFuncDeclare { name, args, return_type_name, body } => {
				assert_eq!(name, new_name_token("add_squared", false));
				assert_eq!(
					args,
					vec![
						ParsedFuncArgDef::new(new_name_token("a", false), new_name_token("f32", true)),
						ParsedFuncArgDef::new(new_name_token("b", false), new_name_token("f32", true)),
					]);
				assert_eq!(return_type_name, Some(new_name_token("f32", true)));
				
				assert_eq!(body.statements().len(), 3);
				
				context.add_variable(new_name_token("a", false), DataType::Builtin (BuiltinType::Float32), DataType::Builtin (BuiltinType::Float32).default_value()).unwrap();
				context.add_variable(new_name_token("b", false), DataType::Builtin (BuiltinType::Float32), DataType::Builtin (BuiltinType::Float32).default_value()).unwrap();
				
				match &body.statements()[0].kind {
					StatementKind::VariableDeclareSet { var_name, data_type_name, value_expr } => {
						assert_eq!(*var_name, new_name_token("a2", false));
						assert_eq!(*data_type_name, new_name_token("f32", true));
						let dt: DataType = value_expr.check_as_rhs_and_calc_data_type(&context).unwrap();
						assert_eq!(dt, DataType::Builtin (BuiltinType::Float32));
					},
					_ => {
						println!("Wrong statement:");
						dbg!(&body.statements()[0]);
						panic!("Err!");
					},
				}
				
				context.add_variable(new_name_token("a2", false), DataType::Builtin (BuiltinType::Float32), DataType::Builtin (BuiltinType::Float32).default_value()).unwrap();
				
				match &body.statements()[1].kind {
					StatementKind::VariableDeclareSet { var_name, data_type_name, value_expr } => {
						assert_eq!(*var_name, new_name_token("b2", false));
						assert_eq!(*data_type_name, new_name_token("f32", true));
						let dt: DataType = value_expr.check_as_rhs_and_calc_data_type(&context).unwrap();
						assert_eq!(dt, DataType::Builtin (BuiltinType::Float32));
					},
					_ => {
						println!("Wrong statement:");
						dbg!(&body.statements()[1]);
						panic!("Err!");
					},
				}
				
				context.add_variable(new_name_token("b2", false), DataType::Builtin (BuiltinType::Float32), DataType::Builtin (BuiltinType::Float32).default_value()).unwrap();
				
				match &body.statements()[2].kind {
					StatementKind::UserDefinedFuncReturn { return_expr } => {
						let dt: Option<DataType> = return_expr.as_ref()
							.map(|expr| expr.check_as_rhs_and_calc_data_type(&context).unwrap());
							
						assert_eq!(dt, Some(DataType::Builtin (BuiltinType::Float32)));
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
		let mut statements_iter = StatementsIter::new();
		
		let builtin_func_defs = Vec::<BuiltinFuncDef>::new();
		let primitive_type_member_funcs_list = PrimitiveTypeMemberBuiltinFuncsList::new();
		let mut context = Context::new(
			&builtin_func_defs,
			&primitive_type_member_funcs_list,
			Vec::<StructDef>::new());
		
		statements_iter.push_string(r#"
		f add_squared(a: @f32, b: @f32) {
			var a2: @f32 = a ^ 2;
			var b2: @f32 = b ^ 2;
			@println(a2 + b2);
			return;
		}
		"#.to_string());
		
		let st = statements_iter.next().unwrap().unwrap();
		match st.kind {
			StatementKind::UserDefinedFuncDeclare { name, args, return_type_name, body } => {
				assert_eq!(name, new_name_token("add_squared", false));
				assert_eq!(
					args,
					vec![
						ParsedFuncArgDef::new(new_name_token("a", false), new_name_token("f32", true)),
						ParsedFuncArgDef::new(new_name_token("b", false), new_name_token("f32", true)),
					]);
				assert_eq!(return_type_name, None);
				
				assert_eq!(body.statements().len(), 4);
				
				context.add_variable(new_name_token("a", false), DataType::Builtin (BuiltinType::Float32), DataType::Builtin (BuiltinType::Float32).default_value()).unwrap();
				context.add_variable(new_name_token("b", false), DataType::Builtin (BuiltinType::Float32), DataType::Builtin (BuiltinType::Float32).default_value()).unwrap();
				
				match &body.statements()[0].kind {
					StatementKind::VariableDeclareSet { var_name, data_type_name, value_expr } => {
						assert_eq!(*var_name, new_name_token("a2", false));
						assert_eq!(*data_type_name, new_name_token("f32", true));
						let dt: DataType = value_expr.check_as_rhs_and_calc_data_type(&context).unwrap();
						assert_eq!(dt, DataType::Builtin (BuiltinType::Float32));
					},
					_ => {
						println!("Wrong statement:");
						dbg!(&body.statements()[0]);
						panic!("Err!");
					},
				}
				
				context.add_variable(new_name_token("a2", false), DataType::Builtin (BuiltinType::Float32), DataType::Builtin (BuiltinType::Float32).default_value()).unwrap();
				
				match &body.statements()[1].kind {
					StatementKind::VariableDeclareSet { var_name, data_type_name, value_expr } => {
						assert_eq!(*var_name, new_name_token("b2", false));
						assert_eq!(*data_type_name, new_name_token("f32", true));
						let dt: DataType = value_expr.check_as_rhs_and_calc_data_type(&context).unwrap();
						assert_eq!(dt, DataType::Builtin (BuiltinType::Float32));
					},
					_ => {
						println!("Wrong statement:");
						dbg!(&body.statements()[1]);
						panic!("Err!");
					},
				}
				
				context.add_variable(new_name_token("b2", false), DataType::Builtin (BuiltinType::Float32), DataType::Builtin (BuiltinType::Float32).default_value()).unwrap();
				
				match &body.statements()[2].kind {
					StatementKind::FuncCall { .. } => {},
					_ => {
						println!("Wrong statement:");
						dbg!(&body.statements()[2]);
						panic!("Err!");
					},
				}
				
				match &body.statements()[3].kind {
					StatementKind::UserDefinedFuncReturn { return_expr } => {
						let dt: Option<DataType> = return_expr.as_ref()
							.map(|expr| expr.check_as_rhs_and_calc_data_type(&context).unwrap());
							
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
	fn can_parse_struct_declaration() {
		let test = |code: &str| {
			let mut statements_iter = StatementsIter::new();
			
			statements_iter.push_string(code.to_string());
			
			let st = statements_iter.next().unwrap().unwrap();
			match st.kind {
				StatementKind::UserStructDeclare {
					new_type_name,
					fields,
				} => {
					assert_eq!(new_type_name, new_name_token("Human", false));
					
					assert_eq!(fields.len(), 3);
					
					assert_eq!(fields[0], ParsedStructFieldDef::new(
						new_name_token("name", false),
						new_name_token("str", true)));
					
					assert_eq!(fields[1], ParsedStructFieldDef::new(
						new_name_token("age", false),
						new_name_token("f32", true)));
					
					assert_eq!(fields[2], ParsedStructFieldDef::new(
						new_name_token("is_married", false),
						new_name_token("bool", true)));
				},
				_ => {
					println!("Wrong statement:");
					dbg!(st);
					panic!("Err!");
				},
			}
		};
		
		// without trailing comma
		test(r#"
		struct Human {
			name: @str,
			age: @f32,
			is_married: @bool
		}
		"#);
		
		// with trailing comma
		test(r#"
		struct Human {
			name: @str,
			age: @f32,
			is_married: @bool,
		}
		"#);
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

	fn new_name_token(name: &str, is_builtin: bool) -> NameToken {
		NameToken::new_with_pos(name.to_string(), CodePos::from(CharPos::new()), is_builtin)
	}
}