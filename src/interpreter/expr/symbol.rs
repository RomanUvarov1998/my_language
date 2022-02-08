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
use std::collections::VecDeque;

//------------------------------- SymbolIterator ----------------------------------

pub struct SymbolIterator<'tokens_iter> {
	iter: &'tokens_iter mut TokensIter,
	expr_context: ExprContext,
	cached_symbols: VecDeque<Symbol>,
	prev_is_operand: bool,
}

impl<'tokens_iter> SymbolIterator<'tokens_iter> {
	pub fn new(iter: &'tokens_iter mut TokensIter, expr_context: ExprContext) -> Self {
		Self {
			iter,
			expr_context,
			cached_symbols: VecDeque::new(),
			prev_is_operand: false,
		}
	}
}

impl Iterator for SymbolIterator<'_> {
	type Item = Result<Symbol, InterpErr>;
	
	fn next(&mut self) -> Option<Self::Item> {
		if let Some(symbol) = self.cached_symbols.pop_front() {
			return Some(Ok(symbol));
		}
		
		let next_token_ref: &Token = match self.iter.peek() {
			Ok(tok_ref_op) => tok_ref_op?,
			Err(err) => return Some(Err(err.into())),
		};
		
		match self.expr_context.check_expr_end(next_token_ref) {
			Ok(should_stop) => if should_stop {
				return None;
			},
			Err(err) => return Some(Err(err.into())),
		}
		
		let token = self.iter.next().unwrap().unwrap();
		
		let Token { pos, content } = token;
		
		let symbol_result: Result<Symbol, InterpErr> = match content {
			// iterator doesn't give comment tokens by default
			TokenContent::Comment (_) => unreachable!(),
			
			TokenContent::Number (num) => Ok( Symbol::new_number(num, pos) ),
			
			TokenContent::StringLiteral (s) => Ok( Symbol::new_string_literal(s, pos) ),
			
			TokenContent::CharLiteral (c) => Ok( Symbol::new_char_literal(c, pos) ),
			
			TokenContent::BuiltinName (name) => {
				let nt = NameToken::new_with_pos(name, pos, true);
				Symbol::parse_func_call_or_name_or_struct_literal(self.iter, nt)
			},
			
			TokenContent::Name (name) => {
				let nt = NameToken::new_with_pos(name, pos, false);
				Symbol::parse_func_call_or_name_or_struct_literal(self.iter, nt)
			},
			
			TokenContent::Operator (ref tok_op) => {
				let optr: ExprOperator = match tok_op {
					Operator::Plus => if self.prev_is_operand {
						ExprOperator::BinPlus
					} else {
						ExprOperator::UnPlus
					},
					
					Operator::Minus => if self.prev_is_operand {
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
					
					Operator::Assign => return Some(Err(unexpected(pos))),
				};
				
				Ok(Symbol {
					pos,
					kind: SymbolKind::ExprOperator (optr),
				} )
			},
			
			TokenContent::Bracket (tok_br) => match tok_br {
				Bracket::Right => Ok( Symbol { pos, kind: SymbolKind::RightRoundBracket } ),
				
				Bracket::Left => Ok( Symbol { pos, kind: SymbolKind::LeftRoundBracket } ),
				
				Bracket::LeftCurly | Bracket::RightCurly => return Some(Err(unexpected(pos))),
				
				Bracket::LeftSquared => {
					// cache Operand::IndexExpr as symbol to be returned next
					match Symbol::parse_index_value(self.iter) {
						Ok(sym) => self.cached_symbols.push_back(sym),
						Err(err) => return Some(Err(err)),
					};
					
					// skip ']' token
					if let Some( Ok ( Token {
						content: TokenContent::Bracket (Bracket::RightSquared), .. 
					} ) ) = self.iter.next() {} else { unreachable!(); }
					
					// return '[' as Index operator
					Ok( Symbol {
						pos,
						kind: SymbolKind::ExprOperator (ExprOperator::Index),
					} )
				},
				
				Bracket::RightSquared => return Some(Err(unexpected(pos))),
			},
			
			TokenContent::Keyword (kw) => {
				let kind: SymbolKind = match kw {
					Keyword::Var | 
						Keyword::Struct | 
						Keyword::If | 
						Keyword::Else | 
						Keyword::While | 
						Keyword::F | 
						Keyword::Return => return Some(Err(unexpected(pos))),
					Keyword::True => SymbolKind::Operand (Operand::Value (Value::from(true))),
					Keyword::False => SymbolKind::Operand (Operand::Value (Value::from(false))),
				};
				
				Ok(Symbol {
					pos,
					kind,
				} )
			},
			
			TokenContent::StatementOp (ref st_op) => {
				let kind: SymbolKind = match st_op {
					StatementOp::Dot => SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
					_ => return Some(Err(unexpected(pos))),
				};
				
				Ok( Symbol {
					pos,
					kind,
				} )
			},
		};
		
		match symbol_result {
			Ok( Symbol{ kind: SymbolKind::Operand(_), .. } ) => {
				if self.prev_is_operand {
					return Some(Err(unexpected(pos))); 
				}
				self.prev_is_operand = true;
			},
			
			Ok( Symbol{ kind: SymbolKind::RightRoundBracket, .. } ) => {
				self.prev_is_operand = true;
			},
			
			_ => {
				self.prev_is_operand = false;
			}
		}
		
		Some(symbol_result)
	}
}

//------------------------------- Symbol ----------------------------------

#[derive(Debug, Clone)]
pub struct Symbol {
	pub kind: SymbolKind,
	pub pos: CodePos,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolKind {
	Operand (Operand),
	LeftRoundBracket,
	RightRoundBracket,
	ExprOperator (ExprOperator),
}

impl Symbol {
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
							
							let value_expr = Expr::new(tokens_iter, ExprContextKind::StructFieldValue)?;
							
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
							
							let value_expr = Expr::new(tokens_iter, ExprContextKind::StructFieldValue)?;
							
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

	fn parse_index_value(tokens_iter: &mut TokensIter) -> Result<Symbol, InterpErr> {
		let index_expr = Expr::new(tokens_iter, ExprContextKind::IndexValue)?;
		Ok( Symbol {
			pos: index_expr.pos(),
			kind: SymbolKind::Operand ( Operand::IndexExpr (index_expr) ),
		} )
	}

	fn new_number(num: f32, pos: CodePos) -> Self {
		Self {
			kind: SymbolKind::Operand( Operand::Value (Value::from(num)) ),
			pos,
		}
	}
	
	fn new_name(name_tok: NameToken) -> Self {
		Self {
			pos: name_tok.pos(),
			kind: SymbolKind::Operand( Operand::Variable (name_tok) ),
		}
	}
	
	fn new_func_call(func_name: NameToken, arg_exprs: Vec<Expr>) -> Self {
		let pos = func_name.pos();
		Self {
			kind: SymbolKind::Operand( Operand::FuncCall {
				func_name, 
				arg_exprs,
			} ),
			pos,
		}
	}
	
	fn new_string_literal(content: String, pos: CodePos) -> Self {
		Self {
			kind: SymbolKind::Operand( Operand::Value (Value::from(content)) ),
			pos,
		}
	}
	
	fn new_char_literal(ch: char, pos: CodePos) -> Self {
		Self {
			kind: SymbolKind::Operand( Operand::Value (Value::from(ch)) ),
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
	ValueRef (Rc<RefCell<Value>>),
	IndexExpr (Expr),
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
			Operand::ValueRef (v1) => match other {
				Operand::ValueRef (v2) => *v1.borrow() == *v2.borrow(),
				_ => false,
			},
			Operand::IndexExpr (se1) => match other {
				Operand::IndexExpr (se2) => se1 == se2,
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
			
			Operand::ValueRef (value_rc) => value_rc.borrow().get_type(),
			
			Operand::IndexExpr (se) => se.check_and_calc_data_type(check_context)?,
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
			
			Operand::ValueRef (value_rc) => Some(value_rc.borrow().clone()),
			
			Operand::IndexExpr (se) => Some(se.calc(context)),
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
			Operand::ValueRef (_) => write!(f, "struct field"),
			Operand::IndexExpr (_) => write!(f, "index expr"),
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