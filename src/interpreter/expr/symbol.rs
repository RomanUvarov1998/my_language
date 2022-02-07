use super::super::token::{Token, TokensIter, TokenContent, Keyword, Operator, Bracket, StatementOp, TokenErr};
use super::super::InterpErr;
use super::super::value::Value;
use super::super::data_type::{DataType, DataTypeErr};
use super::super::builtin_func::BuiltinFuncDef;
use super::super::user_func::{UserFuncArg, UserFuncDef};
use super::super::utils::{CodePos, CharPos, NameToken};
use super::super::context::Context;
use super::super::struct_def::StructDef;
use super::{Expr, ExprContext, ExprContextKind};
use super::expr_operator::ExprOperator;
use super::unexpected;
use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

//------------------------------- Symbol ----------------------------------

#[derive(Debug, Clone)]
pub struct Symbol {
	pub kind: SymbolKind,
	pub pos: CodePos,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolKind {
	Operand (Operand),
	LeftBracket,
	RightBracket,
	ExprOperator (ExprOperator),
}

impl Symbol {
	pub fn next_from(tokens_iter: &mut TokensIter, expr_context: &mut ExprContext, prev_is_operand: bool) -> Result<Option<Self>, InterpErr> {
		let next_token_ref = tokens_iter.peek_or_end_reached_err()?;
		
		if expr_context.check_expr_end(next_token_ref)? {
			return Ok(None);
		}
		
		let token = tokens_iter.next().unwrap()?;
		
		let Token { pos, content } = token;
		
		match content {
			// iterator doesn't give comment tokens by default
			TokenContent::Comment (_) => unreachable!(),
			
			TokenContent::Number (num) => {
				if prev_is_operand {
					return Err(unexpected(pos));
				}				
				let sym = Symbol::new_number(num, pos);
				Ok(Some(sym))
			},
			
			TokenContent::StringLiteral (s) => {
				if prev_is_operand {
					return Err(unexpected(pos));
				}				
				let sym = Symbol::new_string_literal(s, pos);
				Ok(Some(sym))
			},
			
			TokenContent::BuiltinName (name) => {
				let nt = NameToken::new_with_pos(name, pos, true);
				let sym = Self::parse_func_call_or_name_or_struct_literal(tokens_iter, nt)?;
				Ok(Some(sym))
			},
			
			TokenContent::Name (name) => {
				let nt = NameToken::new_with_pos(name, pos, false);
				let sym = Self::parse_func_call_or_name_or_struct_literal(tokens_iter, nt)?;
				Ok(Some(sym))
			},
			
			TokenContent::Operator (ref tok_op) => {
				let optr: ExprOperator = match tok_op {
					Operator::Plus => if prev_is_operand {
						ExprOperator::BinPlus
					} else {
						ExprOperator::UnPlus
					},
					
					Operator::Minus => if prev_is_operand {
						ExprOperator::BinMinus
					} else {
						ExprOperator::UnMinus
					},
					
					Operator::Mul => ExprOperator::Mul,
					Operator::Div  => ExprOperator::Div,
					Operator::Pow  => ExprOperator::Pow,
					Operator::Greater  => ExprOperator::Greater,
					Operator::GreaterEqual => ExprOperator::GreaterEqual,
					Operator::Less => ExprOperator::Less,
					Operator::LessEqual => ExprOperator::LessEqual,
					Operator::Not => ExprOperator::Not,
					Operator::Equal => ExprOperator::Equal,
					Operator::NotEqual => ExprOperator::NotEqual,
					Operator::LogicalAnd => ExprOperator::LogicalAnd,
					Operator::LogicalOr => ExprOperator::LogicalOr,
					Operator::LogicalXor => ExprOperator::LogicalXor,
					
					Operator::Assign => return Err(unexpected(pos)),
				};
				
				Ok(Some(Symbol {
					pos,
					kind: SymbolKind::ExprOperator (optr),
				} ))
			},
			
			TokenContent::Bracket (tok_br) => {
				let kind: SymbolKind = match tok_br {
					Bracket::Right => SymbolKind::RightBracket,
					Bracket::Left => SymbolKind::LeftBracket,
					Bracket::LeftCurly | Bracket::RightCurly => return Err(unexpected(pos)),
				};
				
				Ok(Some(Symbol {
					pos,
					kind,
				} ))
			},
			
			TokenContent::Keyword (kw) => {
				let kind: SymbolKind = match kw {
					Keyword::Var | 
						Keyword::Struct | 
						Keyword::If | 
						Keyword::Else | 
						Keyword::While | 
						Keyword::F | 
						Keyword::Return => return Err(unexpected(pos)),
					Keyword::True => SymbolKind::Operand (Operand::Value (Value::from(true))),
					Keyword::False => SymbolKind::Operand (Operand::Value (Value::from(false))),
				};
				
				Ok(Some(Symbol {
					pos,
					kind,
				} ))
			},
			
			TokenContent::StatementOp (ref st_op) => {
				let kind: SymbolKind = match st_op {
					StatementOp::Dot => SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
					_ => return Err(unexpected(pos)),
				};
				
				Ok(Some(Symbol {
					pos,
					kind,
				} ))
			},
		}
	}
	
	fn parse_func_call_or_name_or_struct_literal(tokens_iter: &mut TokensIter, name: NameToken) -> Result<Symbol, InterpErr> {
		match tokens_iter.peek_or_end_reached_err()?.content() {
			// func call
			TokenContent::Bracket (Bracket::Left) => {
				tokens_iter.next_or_end_reached_err().unwrap();
				
				let mut arg_exprs = Vec::<Expr>::new();
				
				if let TokenContent::Bracket (Bracket::Right) = tokens_iter.peek_or_end_reached_err()?.content() {
					tokens_iter.next_or_end_reached_err().unwrap();
				} else {
					loop {
						arg_exprs.push(Expr::new(
							tokens_iter,
							ExprContextKind::FunctionArg)?);
							
						match tokens_iter.next_or_end_reached_err()? {
							Token { content: TokenContent::StatementOp ( StatementOp::Comma ), .. } => {},
							
							Token { content: TokenContent::Bracket ( Bracket::Right ), .. } => break,
							
							found @ _ => 
								return Err(TokenErr::ExpectedButFound { 
									expected: vec![
										TokenContent::StatementOp(StatementOp::Comma),
										TokenContent::Bracket ( Bracket::Right ),
									], 
									found
								}.into()),
						}
					}
				}
				
				let func_name = name;
				
				let sym = Symbol::new_func_call(func_name, arg_exprs);
				Ok(sym)
			},
			
			// struct literal
			TokenContent::Bracket (Bracket::LeftCurly) => {
				tokens_iter.next_or_end_reached_err().unwrap(); // skip '{'
				
				let data_type_name = name;
				
				let mut fields = Vec::<StructLiteralField>::new();
				
				let last_pos: CharPos = loop {
					match tokens_iter.next_or_end_reached_err()? {
						Token { content: TokenContent::Bracket (Bracket::RightCurly), pos } => break pos.end(),
						
						Token { content: TokenContent::Name (name), pos } => {
							let field_name = NameToken::new_with_pos(name, pos, false);
							
							tokens_iter.next_expect_colon()?;
							
							let value_expr = Expr::new(tokens_iter, ExprContextKind::StructField)?;
							
							fields.push(StructLiteralField::new(field_name, value_expr));
							
							match tokens_iter.next_or_end_reached_err()? {
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
						
						Token { content: TokenContent::BuiltinName (name), pos } => {
							let field_name = NameToken::new_with_pos(name, pos, true);
							
							tokens_iter.next_expect_colon()?;
							
							let value_expr = Expr::new(tokens_iter, ExprContextKind::StructField)?;
							
							fields.push(StructLiteralField::new(field_name, value_expr));
							
							match tokens_iter.next_or_end_reached_err()? {
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
										TokenContent::StatementOp (StatementOp::Comma),
										TokenContent::Bracket (Bracket::RightCurly),
									],
									found,
								}.into()),
					}
				};
				
				Ok(Symbol {
					pos: CodePos::new(data_type_name.pos().begin(), last_pos),
					kind: SymbolKind::Operand (Operand::StructLiteral {
						data_type_name,
						fields,
					}),
				})
			},
			
			// variable
			_ => {
				let sym = Symbol::new_name(name);
				Ok(sym)
			},
		}
	}

	
	pub fn new_number(num: f32, pos: CodePos) -> Self {
		Self {
			kind: SymbolKind::Operand( Operand::Value (Value::from(num)) ),
			pos,
		}
	}
	pub fn new_name(name_tok: NameToken) -> Self {
		Self {
			pos: name_tok.pos(),
			kind: SymbolKind::Operand( Operand::Variable (name_tok) ),
		}
	}
	pub fn new_func_call(func_name: NameToken, arg_exprs: Vec<Expr>) -> Self {
		let pos = func_name.pos();
		Self {
			kind: SymbolKind::Operand( Operand::FuncCall {
				func_name, 
				arg_exprs,
			} ),
			pos,
		}
	}
	pub fn new_string_literal(content: String, pos: CodePos) -> Self {
		Self {
			kind: SymbolKind::Operand( Operand::Value (Value::from(content)) ),
			pos,
		}
	}
	
	pub fn kind(&self) -> &SymbolKind {
		&self.kind
	}
	pub fn pos(&self) -> CodePos {
		self.pos
	}

	pub fn unwrap_operand(self) -> Operand {
		match self.kind {
			SymbolKind::Operand (op) => op,
			_ => panic!("Wrong input: {:?}", &self),
		}
	}
}

impl Eq for Symbol {}
impl PartialEq for Symbol {
	fn eq(&self, other: &Self) -> bool {
		self.kind == other.kind
	}
}

//------------------------------- Operand ----------------------------------

#[derive(Debug, Clone)]
pub enum Operand {
	Value (Value),
	Variable (NameToken),
	FuncCall {
		func_name: NameToken, 
		arg_exprs: Vec<Expr>,
	},
	StructLiteral {
		data_type_name: NameToken,
		fields: Vec<StructLiteralField>,
	},
	StructFieldValue (Rc<RefCell<Value>>),
}

impl Eq for Operand {}

impl PartialEq for Operand {
	fn eq(&self, other: &Self) -> bool {
		match self {
			Operand::Value (v1) => match other {
				Operand::Value (v2) => v1 == v2,
				_ => false,
			},
			Operand::Variable (op1) => match other {
				Operand::Variable (op2) => op1 == op2,
				_ => false,
			},
			Operand::FuncCall { func_name: fn1, arg_exprs: ae1 } => match other {
				Operand::FuncCall { func_name: fn2, arg_exprs: ae2 } => fn1 == fn2 && ae1 == ae2,
				_ => false,
			},
			sl_1 @ Operand::StructLiteral { .. } => match other {
				sl_2 @ Operand::StructLiteral { .. } => sl_1 == sl_2,
				_ => false,
			},
			Operand::StructFieldValue (v1) => match other {
				Operand::StructFieldValue (v2) => *v1.borrow() == *v2.borrow(),
				_ => false,
			},
		}
	}
}

impl Operand {
	pub fn check_and_calc_data_type_in_place(&self, check_context: &Context) -> Result<DataType, InterpErr> {
		let dt: DataType = match self {
			Operand::Value (val) => val.get_type().clone(),
			
			Operand::Variable (name) =>
				check_context.get_variable_value(&name)?.get_type().clone(),
				
			Operand::FuncCall { func_name, arg_exprs } => {
				if func_name.is_builtin() {
					let f: &BuiltinFuncDef = check_context.find_builtin_func_def(&func_name)?;
			
					f.check_args(&func_name, arg_exprs, check_context)?;
					f.return_type().clone()
				} else {
					let f: &UserFuncDef = check_context.find_user_func_def(&func_name)?;
			
					f.check_args(arg_exprs, check_context)?;
					f.return_type().clone()
				}
			},
			
			Operand::StructLiteral { data_type_name, fields } => {
				let dt: DataType = check_context.find_type_by_name(data_type_name)?;
				
				if let DataType::Complex(ref struct_def) = dt {
					struct_def.check_fields_set(&data_type_name, fields, check_context)?;
				} else {
					return Err(DataTypeErr::PrimitiveTypeInitializedAsComplex { type_name_in_code: data_type_name.clone() }.into());
				}
				
				dt
			},
			
			Operand::StructFieldValue (value_rc) => value_rc.borrow().get_type(),
		};
		
		Ok(dt)
	}
	
	pub fn calc_in_place(&self, context: &Context) -> Option<Value> {
		match self {
			Operand::Value (val) => Some(val.clone()), // TODO: try do it without cloning values
			
			Operand::Variable (name) => Some(context.get_variable_value(&name).unwrap().clone()),
			
			Operand::FuncCall { func_name, arg_exprs } => {
				if func_name.is_builtin() {
					let f: &BuiltinFuncDef = context.find_builtin_func_def(&func_name).unwrap();
			
					let mut args_values = Vec::<Value>::with_capacity(arg_exprs.len());
					
					for expr in arg_exprs {
						let value: Value = expr.calc(context);
						args_values.push(value);
					}
					
					f.call(args_values)
				} else {
					let f: &UserFuncDef = context.find_user_func_def(&func_name).unwrap();
			
					let mut args_values = Vec::<Value>::with_capacity(arg_exprs.len());
					
					for expr in arg_exprs {
						let value: Value = expr.calc(context);
						args_values.push(value);
					}
					
					let mut next_context = context.new_stack_frame_context();
					
					let func_args: &Vec<UserFuncArg> = f.args();
					for i in 0..args_values.len() {
						next_context.add_variable(
							func_args[i].name().clone(),
							func_args[i].data_type().clone(),
							args_values[i].clone()).unwrap();
					}
					
					f.call(&mut next_context)
				}
			},
			
			Operand::StructLiteral { data_type_name, fields } => {
				let mut calculated_fields = HashMap::<String, Rc<RefCell<Value>>>::new();
				
				for field in fields {
					calculated_fields.insert(
						field.field_name().value().to_string(), 
						Rc::new(RefCell::new(field.value_expr().calc(context))));
				}
				
				let struct_def: StructDef = if let DataType::Complex(struct_def) = context.find_type_by_name(data_type_name).unwrap() {
					struct_def
				} else { unreachable!(); };
				
				Some(Value::Struct {
					struct_def,
					fields: calculated_fields,
				})
			},
			
			Operand::StructFieldValue (value_rc) => Some(value_rc.borrow().clone()),
		}
	}
}

impl std::fmt::Display for Operand {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Operand::Value (_) => write!(f, "value literal"),
			Operand::Variable (_) => write!(f, "variable"),
			Operand::FuncCall { .. } => write!(f, "function call"),
			Operand::StructLiteral { .. } => write!(f, "struct literal"),
			Operand::StructFieldValue (_) => write!(f, "struct field"),
		}
	}
}

//------------------------------- StructLiteralField ----------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructLiteralField {
	field_name: NameToken,
	value_expr: Expr,
}

impl StructLiteralField {
	pub fn new(field_name: NameToken, value_expr: Expr) -> Self {
		Self {
			field_name,
			value_expr,
		}
	}
	
	pub fn field_name(&self) -> &NameToken {
		&self.field_name
	}
	
	pub fn value_expr(&self) -> &Expr {
		&self.value_expr
	}
}