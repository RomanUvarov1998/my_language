mod symbol;
mod expr_operator;

use super::token::{Token, TokenContent, TokensIter, Operator, Bracket, StatementOp, Keyword, TokenErr};
use super::InterpErr;
use super::value::Value;
use super::data_type::DataType;
use super::builtin_func::BuiltinFuncDef;
use super::utils::{CharPos, CodePos, NameToken};
use super::statement::FuncKind;
use super::context::Context;
use symbol::{Symbol, SymbolKind, Operand};
use expr_operator::{ExprOperator, OpAssot};
use std::rc::Rc;

#[derive(Debug)]
pub struct Expr {
	expr_stack: Rc<Vec<Symbol>>,
	pos: CodePos,
}

impl Expr {	
	pub fn new(tokens_iter: &mut TokensIter, context_kind: ExprContextKind) -> Result<Self, InterpErr> {
		let context = ExprContext::new(context_kind);
		let expr_stack = Self::create_stack(tokens_iter, context)?;
		
		let mut pos_begin: CharPos = expr_stack[0].pos().begin();
		let mut pos_end: CharPos = expr_stack[0].pos().end();
		
		for sym_ref in &expr_stack {
			pos_begin = std::cmp::min(sym_ref.pos().begin(), pos_begin);
			pos_end = std::cmp::max(sym_ref.pos().end(), pos_end);
		}
		
		Ok( Self { 
			expr_stack: Rc::new(expr_stack),
			pos: CodePos::new(pos_begin, pos_end),
		} )
	}
	
	pub fn calc(&self, context: &Context) -> Value {
		let mut calc_stack = Vec::<Symbol>::with_capacity(self.expr_stack.len());
		
		for sym in self.expr_stack.iter() {
			match sym.kind() {
				SymbolKind::Operand (_) => calc_stack.push(sym.clone()), // TODO: avoid cloning symbols
				
				SymbolKind::LeftBracket => unreachable!(),
				
				SymbolKind::ExprOperator (op) => {
					let value: Value = op.apply(&mut calc_stack, context);
					calc_stack.push(Symbol { 
						pos: sym.pos(), 
						kind: SymbolKind::Operand (Operand::Value(value)),
					}); // TODO: somehow place DataType here, not a massive symbol
				},
			}
		}
		
		let result: Value = calc_stack.pop()
			.unwrap()
			.unwrap_operand()
			.calc_in_place(context);
		assert_eq!(calc_stack.pop(), None);
		
		result
	}
	
	pub fn check_and_calc_data_type(&self, check_context: &Context) -> Result<DataType, InterpErr> {		
		assert!(self.expr_stack.len() > 0);
		
		let mut type_calc_stack = Vec::<Symbol>::with_capacity(self.expr_stack.len());
		
		for sym in self.expr_stack.iter() {
			match sym.kind() {
				SymbolKind::Operand (_) => type_calc_stack.push(sym.clone()), // TODO: avoid cloning symbols
				
				SymbolKind::LeftBracket => unreachable!(),
				
				SymbolKind::ExprOperator (op) => {
					let dt: DataType = op.get_result_data_type(&mut type_calc_stack, check_context, sym.pos())?;
					type_calc_stack.push(Symbol { 
						pos: sym.pos(), 
						kind: SymbolKind::Operand (Operand::Value(dt.default_value())),
					}); // TODO: somehow place DataType here, not a massive symbol
				}, 
			}
		}
		
		let result: DataType = type_calc_stack.pop()
			.unwrap()
			.unwrap_operand()
			.check_and_calc_data_type_in_place(check_context)
			.unwrap();
		assert_eq!(type_calc_stack.pop(), None);
		
		Ok(result)
	}

	pub fn pos(&self) -> CodePos {
		self.pos
	}
	
	fn create_stack(tokens_iter: &mut TokensIter, mut context: ExprContext) -> Result<Vec<Symbol>, InterpErr> {
		let mut tmp_stack = Vec::<Symbol>::new();
		let mut expr_stack = Vec::<Symbol>::new();
		let mut prev_is_operand = false;
		
		loop {
			let next_token_ref = tokens_iter.peek_or_end_reached_err()?;
			
			if context.check_expr_end(next_token_ref)? {		
				if expr_stack.len() == 0 {
					let found_token = tokens_iter.next_or_end_reached_err()?;
					return Err( InterpErr::from(ExprErr::ExpectedExprButFound(found_token.pos())) );
				}
				break;
			}
			
			let token = tokens_iter.next().unwrap()?;
			
			let Token { pos, content } = token;
			
			match content {	
				TokenContent::Number (num) => {
					if prev_is_operand {
						return Err(unexpected(pos));
					}
					
					let sym = Symbol::new_number(num, pos);
					expr_stack.push(sym);
					
					prev_is_operand = true;
				},
				
				TokenContent::BuiltinName (name) => {
					if prev_is_operand {
						return Err(unexpected(pos));
					}
					
					let sym = Self::parse_func_call_or_name(FuncKind::Builtin, &mut prev_is_operand, tokens_iter, name, pos)?;
					expr_stack.push(sym);
				},
				
				TokenContent::Name (name) => {
					if prev_is_operand {
						return Err(unexpected(pos));
					}
					
					let sym = Self::parse_func_call_or_name(FuncKind::UserDefined, &mut prev_is_operand, tokens_iter, name, pos)?;
					expr_stack.push(sym);
				},
				
				TokenContent::StringLiteral (s) => {
					if prev_is_operand {
						return Err(unexpected(pos));
					}
					
					let sym = Symbol::new_string_literal(s, pos);
					expr_stack.push(sym);
					
					prev_is_operand = true;					
				},
				
				TokenContent::Operator (ref tok_op) => {
					match tok_op {
						Operator::Plus | Operator::Minus => {
							if prev_is_operand {
								Self::add_bin_op(&mut expr_stack, &mut tmp_stack, pos, content)?;
							} else {
								tmp_stack.push(Symbol::new_un_pref_op(content, pos));
							}
						},
						Operator::Mul 
							| Operator::Div 
							| Operator::Pow 
							| Operator::Greater 
							| Operator::GreaterEqual
							| Operator::Less
							| Operator::LessEqual
							| Operator::Not
							| Operator::Equal
							| Operator::NotEqual
							| Operator::LogicalAnd
							| Operator::LogicalOr
							| Operator::LogicalXor
							=> {
								Self::add_bin_op(&mut expr_stack, &mut tmp_stack, pos, content)?;
						},
						Operator::Assign => return Err(unexpected(pos)),
					};
					
					prev_is_operand = false;
				},
				
				TokenContent::Bracket (tok_br) => {					
					prev_is_operand = match tok_br {
						Bracket::Right => true,
						Bracket::Left => false,
						Bracket::LeftCurly | Bracket::RightCurly => return Err(unexpected(pos)),
					};
					
					Self::add_bracket(&mut expr_stack, &mut tmp_stack, pos, tok_br)?;
				},
				
				TokenContent::Keyword (kw) => match kw {
					Keyword::Var | 
						Keyword::Struct | 
						Keyword::If | 
						Keyword::Else | 
						Keyword::While | 
						Keyword::F | 
						Keyword::Return => return Err(unexpected(pos)),
					Keyword::True | Keyword::False => {
						if prev_is_operand {
							return Err(unexpected(pos));
						}
						
						let sym = Symbol::new_bool_literal(kw, pos);
						expr_stack.push(sym);
						prev_is_operand = true;
					},
				},
				
				TokenContent::StatementOp (ref st_op) => match st_op {
					StatementOp::Dot => {
						Self::add_bin_op(&mut expr_stack, &mut tmp_stack, pos, content)?;
						prev_is_operand = false;
					},
					_ => return Err(unexpected(pos)),
				},
			}
		}
		
		while let Some(top_sym) = tmp_stack.pop() {
			match top_sym.kind() {
				SymbolKind::Operand (..) => expr_stack.push(top_sym),
				SymbolKind::LeftBracket => return Err( unpaired_bracket(top_sym.pos()) ),
				SymbolKind::ExprOperator (..) => expr_stack.push(top_sym),
			}
		}
				
		Ok(expr_stack)
	}
	
	fn add_bin_op(
		expr_stack: &mut Vec<Symbol>, 
		tmp_stack: &mut Vec<Symbol>, 
		pos: CodePos, tc: TokenContent
		) -> Result<(), InterpErr> 
	{
		let next_kind = ExprOperator::new_bin(tc);
		
		use std::cmp::Ordering;
		
		loop {
			let top_tok_sym: Symbol = 
				match tmp_stack.last() {
					Some(top_ref) => 
						match top_ref.kind() {
							SymbolKind::Operand (..) => return Err(unexpected(tmp_stack.pop().unwrap().pos())),
								
							SymbolKind::LeftBracket => break,
							
							SymbolKind::ExprOperator ( top_ref ) => {
								match next_kind.rank().cmp(&top_ref.rank()) {
									Ordering::Less => tmp_stack.pop().unwrap(),
									Ordering::Equal => match (top_ref.assot(), next_kind.assot()) {
										(OpAssot::Right, OpAssot::Right) => break,
										_ => tmp_stack.pop().unwrap(),
									},
									Ordering::Greater => break,
								}
							},
						},
						
					None => break,
				};
			
			expr_stack.push(top_tok_sym);
		}
			
		tmp_stack.push(Symbol { kind: SymbolKind::ExprOperator (next_kind), pos });
		
		Ok(())
	}
	
	fn add_bracket(
		expr_stack: &mut Vec<Symbol>, 
		tmp_stack: &mut Vec<Symbol>, 
		pos: CodePos, br: Bracket
	) -> Result<(), InterpErr> 
	{
		match br {
			Bracket::Left => {
				tmp_stack.push(Symbol::new_left_bracket(pos));
			},
			Bracket::Right => {
				'out: loop {
					match tmp_stack.pop() {
						Some( sym ) => match sym.kind() {
								SymbolKind::LeftBracket => break 'out,
								SymbolKind::Operand (..) | SymbolKind::ExprOperator (..) => expr_stack.push(sym),
							},
						None => return Err( InterpErr::from( ExprErr::UnpairedBracket (pos) ) ),
					};
				};
			},
			Bracket::LeftCurly => return Err(unexpected(pos)),
			Bracket::RightCurly => return Err(unexpected(pos)),
		};
		
		Ok(())
	}

	fn parse_func_call_or_name(kind: FuncKind, prev_is_operand: &mut bool, tokens_iter: &mut TokensIter, name: String, pos: CodePos) -> Result<Symbol, InterpErr> {
		if *prev_is_operand {
			return Err(unexpected(pos));
		}
		*prev_is_operand = true;
		
		match tokens_iter.peek_or_end_reached_err()?.content() {
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
				
				let func_name = NameToken::new_with_pos(name, pos);
				
				let sym = Symbol::new_func_call(kind, func_name, arg_exprs);
				Ok(sym)
			},
			_ => {
				let name = NameToken::new_with_pos(name, pos);
				let sym = Symbol::new_name(name);
				Ok(sym)
			},
		}
	}
}

impl Eq for Expr {}

impl PartialEq for Expr {
	fn eq(&self, other: &Self) -> bool {
		Vec::<Symbol>::eq(&*self.expr_stack, &*other.expr_stack)
	}
}

impl Clone for Expr {
	fn clone(&self) -> Self {
		Self {
			pos: self.pos,
			expr_stack: Rc::clone(&self.expr_stack),
		}
	}
}

//------------------------------- ExprContext ----------------------------------

#[derive(Debug, Clone, Copy)]
pub enum ExprContextKind {
	ValueToAssign,
	ToReturn,
	FunctionArg,
	IfCondition,
}

pub struct ExprContext {
	kind: ExprContextKind,
	left_brackets_count: u32,
}

impl ExprContext {
	fn new(kind: ExprContextKind)-> Self {
		Self {
			kind,
			left_brackets_count: 0_u32,
		}
	}
	
	fn check_expr_end(&mut self, tok: &Token) -> Result<bool, InterpErr> {
		match tok.content() {
			TokenContent::Number (..) | 
				TokenContent::Operator (..) | 
				TokenContent::Name (..) |
				TokenContent::BuiltinName (..) |
				TokenContent::StringLiteral (..)
				=> Ok(false),
			TokenContent::Bracket (_) 
				=> self.check_brackets(tok),
			TokenContent::StatementOp (st_op) => match st_op {
				StatementOp::Dot => Ok(false),
				StatementOp::Colon | StatementOp::Comment (_) | StatementOp::ThinArrow => Err(unexpected(tok.pos())),
				StatementOp::Comma => match self.kind {
					ExprContextKind::ValueToAssign | ExprContextKind::IfCondition | ExprContextKind::ToReturn => Err(unexpected(tok.pos())),
					ExprContextKind::FunctionArg => Ok(true),
				},
				StatementOp::Semicolon => Ok(true),
			},
			TokenContent::Keyword (kw) => match kw {
				Keyword::Var | 
					Keyword::If | 
					Keyword::Struct | 
					Keyword::Else | 
					Keyword::While | 
					Keyword::F | 
					Keyword::Return => Err(unexpected(tok.pos())),
				Keyword::True | Keyword::False => Ok(false),
			},
		}
	}
	
	fn check_brackets(&mut self, br_tok: &Token) -> Result<bool, InterpErr> {
		match br_tok.content() {
			TokenContent::Bracket(br) => match br {
				Bracket::Left => {
					self.left_brackets_count += 1;
					Ok(false)
				},
				Bracket::Right  => {
					if self.left_brackets_count > 0 {
						self.left_brackets_count -= 1;
						Ok(false)
					} else {
						match self.kind {
							ExprContextKind::ValueToAssign | ExprContextKind::IfCondition | ExprContextKind::ToReturn => Err( unpaired_bracket(br_tok.pos()) ),
							ExprContextKind::FunctionArg => Ok(true),
						}
					}
				},
				Bracket::LeftCurly => match self.kind {
					ExprContextKind::IfCondition => Ok(true),
					ExprContextKind::ValueToAssign | ExprContextKind::FunctionArg | ExprContextKind::ToReturn => Err( unpaired_bracket(br_tok.pos()) ),
				},
				_ => Err( unexpected(br_tok.pos()) ),
			},
			_ => unreachable!(),
		}
	}
}

//------------------------------- ExprErr ----------------------------------

#[derive(Debug, PartialEq, Eq)]
pub enum ExprErr {
	UnexpectedToken (CodePos),
	UnpairedBracket (CodePos),
	ExpectedExprButFound (CodePos),
	WrongOperandsTypeForOperator { 
		operator_pos: CodePos,
		descr: String,
	},
	NotEnoughOperandsForOperator { 
		operator_pos: CodePos,
		descr: String,
	},
}

impl ExprErr {
	fn not_enough_operands_for_operator(operator_pos: CodePos, provided_cnt: usize, required_cnt: usize) -> Self {
		ExprErr::NotEnoughOperandsForOperator { 
			operator_pos,
			descr: format!("Expected {} operand(-s) for operator, but found {}", required_cnt, provided_cnt),
		}
	}
	fn wrong_operands_type_for_operator(op: ExprOperator, operator_pos: CodePos, operands_types: &[&DataType]) -> Self {
		ExprErr::WrongOperandsTypeForOperator { 
			operator_pos,
			descr: format!("Operator '{:?}' can't be applied to operands with type(-s) {:?}", op, operands_types),
		}
	}
}

impl std::fmt::Display for ExprErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ExprErr::UnexpectedToken (_) => write!(f, "Unexpected token"),
			ExprErr::UnpairedBracket (_) => write!(f, "Unpaired bracket"),
			ExprErr::ExpectedExprButFound (_) => write!(f, "Expected arithmetical expression, but found"),
			ExprErr::WrongOperandsTypeForOperator { ref descr, .. } => write!(f, "{}", descr),
			ExprErr::NotEnoughOperandsForOperator { ref descr, .. } => write!(f, "{}", descr),
		}
	}
}

fn unexpected(pos: CodePos) -> InterpErr {
	InterpErr::from (ExprErr::UnexpectedToken (pos))
}

fn unpaired_bracket(pos: CodePos) -> InterpErr {
	InterpErr::from (ExprErr::UnpairedBracket (pos))
}

//------------------------------- Tests ----------------------------------

#[cfg(test)]
mod tests {
	use super::super::token::*;
	use super::*;
	use super::super::primitive_type_member_funcs_list::PrimitiveTypeMemberFuncsList;
	use super::super::struct_def::StructDef;
	use super::expr_operator::*;
	
	#[test]
	fn check_stack_creation_and_arithmetic_calc() {
		test_expr_and_its_stack_eq_and_value("3.125;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (3.125_f32))),
		],
		Value::Float32(3.125_f32));
		
		test_expr_and_its_stack_eq_and_value("3.125 + 5.4;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (3.125_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (5.4_f32))),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
		],
		Value::Float32(3.125_f32 + 5.4_f32));
		
		test_expr_and_its_stack_eq_and_value("3.125 + 5.4 * 2.46;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (3.125_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (5.4_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2.46_f32))),
			SymbolKind::ExprOperator (ExprOperator::Mul),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
		],
		Value::Float32(3.125_f32 + 5.4_f32 * 2.46_f32));
		
		test_expr_and_its_stack_eq_and_value("3.125 + 0 + 5.25 * 2.25 - 3.25 / 2 * 4.25;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (3.125_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (0_f32))),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (5.25_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2.25_f32))),
			SymbolKind::ExprOperator (ExprOperator::Mul),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (3.25_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::ExprOperator (ExprOperator::Div),
			SymbolKind::Operand (Operand::Value (Value::Float32 (4.25_f32))),
			SymbolKind::ExprOperator (ExprOperator::Mul),
			SymbolKind::ExprOperator (ExprOperator::BinMinus),
		],
		Value::Float32(3.125_f32 + 0.0_f32 + 5.25_f32 * 2.25_f32 - 3.25_f32 / 2.0_f32 * 4.25_f32));
		
		test_expr_and_its_stack_eq_and_value("3.125 + -5.25 * 2.25;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (3.125_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (5.25_f32))),
			SymbolKind::ExprOperator (ExprOperator::UnMinus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2.25_f32))),
			SymbolKind::ExprOperator (ExprOperator::Mul),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
		],
		Value::Float32(3.125_f32 + -5.25_f32 * 2.25_f32));
		
		test_expr_and_its_stack_eq_and_value("2.5 * ---5.5;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (2.5_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (5.5_f32))),
			SymbolKind::ExprOperator (ExprOperator::UnMinus),
			SymbolKind::ExprOperator (ExprOperator::UnMinus),
			SymbolKind::ExprOperator (ExprOperator::UnMinus),
			SymbolKind::ExprOperator (ExprOperator::Mul),
		],
		Value::Float32(2.5_f32 * ---5.5_f32));
		
		test_expr_and_its_stack_eq_and_value("1.125 * (3.125 + 2.125);", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (1.125_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (3.125_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2.125_f32))),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::ExprOperator (ExprOperator::Mul),
		],
		Value::Float32(1.125_f32 * (3.125_f32 + 2.125_f32)));
		
		test_expr_and_its_stack_eq_and_value("33 + (1 + 2 * (3 + 4) + 5) / 10 - 30;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (33_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (1_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (3_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (4_f32))),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::ExprOperator (ExprOperator::Mul),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (5_f32))),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (10_f32))),
			SymbolKind::ExprOperator (ExprOperator::Div),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (30_f32))),
			SymbolKind::ExprOperator (ExprOperator::BinMinus),
		],
		Value::Float32(33_f32 + (1_f32 + 2_f32 * (3_f32 + 4_f32) + 5_f32) / 10_f32 - 30_f32));
		
		test_expr_and_its_stack_eq_and_value("-(8 - 2.125 * 5.125 + 4.125) / -3.125;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (8_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2.125_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (5.125_f32))),
			SymbolKind::ExprOperator (ExprOperator::Mul),
			SymbolKind::ExprOperator (ExprOperator::BinMinus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (4.125_f32))),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::ExprOperator (ExprOperator::UnMinus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (3.125_f32))),
			SymbolKind::ExprOperator (ExprOperator::UnMinus),
			SymbolKind::ExprOperator (ExprOperator::Div),
		],
		Value::Float32(-(8_f32 - 2.125_f32 * 5.125_f32 + 4.125_f32) / -3.125_f32));
		
		test_expr_and_its_stack_eq_and_value("33 + (1 + 2 * (3 + 4) + 5) / 10 - 30;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (33_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (1_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (3_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (4_f32))),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::ExprOperator (ExprOperator::Mul),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (5_f32))),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (10_f32))),
			SymbolKind::ExprOperator (ExprOperator::Div),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (30_f32))),
			SymbolKind::ExprOperator (ExprOperator::BinMinus),
		],
		Value::Float32(33_f32 + (1_f32 + 2_f32 * (3_f32 + 4_f32) + 5_f32) / 10_f32 - 30_f32));
		
		test_expr_and_its_stack_eq_and_value("2^2;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::ExprOperator (ExprOperator::Pow),
		],
		Value::Float32(2_f32.powf(2_f32)));
		
		test_expr_and_its_stack_eq_and_value("-2^2+4;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::ExprOperator (ExprOperator::Pow),
			SymbolKind::ExprOperator (ExprOperator::UnMinus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (4_f32))),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
		],
		Value::Float32(-2_f32.powf(2_f32) + 4_f32));
		
		test_expr_and_its_stack_eq_and_value("3^1^2;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (3_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (1_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::ExprOperator (ExprOperator::Pow),
			SymbolKind::ExprOperator (ExprOperator::Pow),
		],
		Value::Float32(3_f32.powf(1_f32.powf(2_f32))));
		
		let zero_pos = CodePos::from(CharPos::new());
		
		test_expr_and_its_stack_eq("a + add(2, 4) + @add(4, 9);", vec![
			SymbolKind::Operand (Operand::Variable (new_name_token("a"))),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("add"),
				arg_exprs: vec![
					// arg 1
					Expr { 
						pos: zero_pos,
						expr_stack: Rc::new(vec![
							Symbol {
								kind: SymbolKind::Operand (Operand::Value (Value::from(2_f32))),
								pos: zero_pos,
							},
						]),
					},
					
					// arg 2
					Expr {
						pos: zero_pos,
						expr_stack: Rc::new(vec![
							Symbol {
								kind: SymbolKind::Operand (Operand::Value (Value::from(4_f32))),
								pos: zero_pos,
							},
						]),
					},
				],
			}),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::Builtin,
				func_name: new_name_token("add"),
				arg_exprs: vec![
					// arg 1
					Expr { 
						pos: zero_pos,
						expr_stack: Rc::new(vec![
							Symbol {
								kind: SymbolKind::Operand (Operand::Value (Value::from(4_f32))),
								pos: zero_pos,
							},
						]),
					},
					
					// arg 2
					Expr {
						pos: zero_pos,
						expr_stack: Rc::new(vec![
							Symbol {
								kind: SymbolKind::Operand (Operand::Value (Value::from(9_f32))),
								pos: zero_pos,
							},
						]),
					},
				],
			}),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
		]);
		
		test_expr_and_its_stack_eq("a.foo1() + b.foo2();", vec![
			SymbolKind::Operand (Operand::Variable (new_name_token("a"))),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("foo1"),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
			SymbolKind::Operand (Operand::Variable (new_name_token("b"))),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("foo2"),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
		]);
		
		test_expr_and_its_stack_eq("foo1().foo3() + b.foo2();", vec![
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("foo1"),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("foo3"),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
			SymbolKind::Operand (Operand::Variable (new_name_token("b"))),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("foo2"),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
		]);
		
		test_expr_and_its_stack_eq("2 * a.foo1(c.foo3().foo5() - 3) ^ b.foo2() / d.foo4();", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::Operand (Operand::Variable (new_name_token("a"))),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("foo1"),
				arg_exprs: vec![
					// arg 1
					Expr {
						pos: zero_pos,
						expr_stack: Rc::new(vec![
							Symbol { kind: SymbolKind::Operand (Operand::Variable (new_name_token("c"))), pos: zero_pos, },
							Symbol { kind: SymbolKind::Operand (Operand::FuncCall {
								kind: FuncKind::UserDefined,
								func_name: new_name_token("foo3"),
								arg_exprs: Vec::new(),
							}), pos: zero_pos, },
							Symbol { kind: SymbolKind::ExprOperator (ExprOperator::DotMemberAccess), pos: zero_pos, },
							Symbol { kind: SymbolKind::Operand (Operand::FuncCall {
								kind: FuncKind::UserDefined,
								func_name: new_name_token("foo5"),
								arg_exprs: Vec::new(),
							}), pos: zero_pos, },
							Symbol { kind: SymbolKind::ExprOperator (ExprOperator::DotMemberAccess), pos: zero_pos, },
							Symbol { kind: SymbolKind::Operand (Operand::Value (Value::Float32 (3_f32))), pos: zero_pos, },
							Symbol { kind: SymbolKind::ExprOperator (ExprOperator::BinMinus), pos: zero_pos, },
						]),
					},
				],
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
			
			SymbolKind::Operand (Operand::Variable (new_name_token("b"))),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("foo2"),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),

			SymbolKind::ExprOperator (ExprOperator::Pow),
			
			SymbolKind::ExprOperator (ExprOperator::Mul),
			
			SymbolKind::Operand (Operand::Variable (new_name_token("d"))),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("foo4"),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
			
			SymbolKind::ExprOperator (ExprOperator::Div),
		]);
	}
	
	fn new_name_token(name: &str) -> NameToken {
		NameToken::new_with_pos(name.to_string(), CodePos::from(CharPos::new()))
	}

	#[test]
	fn check_stack_creation_and_bool_calc() {
		test_expr_and_its_stack_eq_and_value("False;", vec![
			SymbolKind::Operand (Operand::Value (Value::Bool (false))),
		],
		Value::Bool(false));
		
		test_expr_and_its_stack_eq_and_value("True;", vec![
			SymbolKind::Operand (Operand::Value (Value::Bool (true))),
		],
		Value::Bool(true));
		
		test_expr_and_its_stack_eq_and_value("!True;", vec![
			SymbolKind::Operand (Operand::Value (Value::Bool (true))),
			SymbolKind::ExprOperator (ExprOperator::Not),
		],
		Value::Bool(false));
		
		test_expr_and_its_stack_eq_and_value("!False;", vec![
			SymbolKind::Operand (Operand::Value (Value::Bool (false))),
			SymbolKind::ExprOperator (ExprOperator::Not),
		],
		Value::Bool(true));
		
		test_expr_and_its_stack_eq_and_value("True == True;", vec![
			SymbolKind::Operand (Operand::Value (Value::Bool (true))),
			SymbolKind::Operand (Operand::Value (Value::Bool (true))),
			SymbolKind::ExprOperator (ExprOperator::Equal),
		],
		Value::Bool(true));
		
		test_expr_and_its_stack_eq_and_value("False == False;", vec![
			SymbolKind::Operand (Operand::Value (Value::Bool (false))),
			SymbolKind::Operand (Operand::Value (Value::Bool (false))),
			SymbolKind::ExprOperator (ExprOperator::Equal),
		],
		Value::Bool(true));
		
		test_expr_and_its_stack_eq_and_value("True != False;", vec![
			SymbolKind::Operand (Operand::Value (Value::Bool (true))),
			SymbolKind::Operand (Operand::Value (Value::Bool (false))),
			SymbolKind::ExprOperator (ExprOperator::NotEqual),
		],
		Value::Bool(true));
		
		test_expr_and_its_stack_eq_and_value("True land False;", vec![
			SymbolKind::Operand (Operand::Value (Value::Bool (true))),
			SymbolKind::Operand (Operand::Value (Value::Bool (false))),
			SymbolKind::ExprOperator (ExprOperator::LogicalAnd),
		],
		Value::Bool(false));
		
		test_expr_and_its_stack_eq_and_value("True lor False;", vec![
			SymbolKind::Operand (Operand::Value (Value::Bool (true))),
			SymbolKind::Operand (Operand::Value (Value::Bool (false))),
			SymbolKind::ExprOperator (ExprOperator::LogicalOr),
		],
		Value::Bool(true));
		
		test_expr_and_its_stack_eq_and_value("True lxor False;", vec![
			SymbolKind::Operand (Operand::Value (Value::Bool (true))),
			SymbolKind::Operand (Operand::Value (Value::Bool (false))),
			SymbolKind::ExprOperator (ExprOperator::LogicalXor),
		],
		Value::Bool(true));
		
		test_expr_and_its_stack_eq_and_value("2 > 3;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (3_f32))),
			SymbolKind::ExprOperator (ExprOperator::Greater),
		],
		Value::Bool(false));
		
		test_expr_and_its_stack_eq_and_value("!(2 > 3);", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (3_f32))),
			SymbolKind::ExprOperator (ExprOperator::Greater),
			SymbolKind::ExprOperator (ExprOperator::Not),
		],
		Value::Bool(true));
		
		test_expr_and_its_stack_eq_and_value("2 >= 3;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (3_f32))),
			SymbolKind::ExprOperator (ExprOperator::GreaterEqual),
		],
		Value::Bool(false));
		
		test_expr_and_its_stack_eq_and_value("2 < 3;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (3_f32))),
			SymbolKind::ExprOperator (ExprOperator::Less),
		],
		Value::Bool(true));
		
		test_expr_and_its_stack_eq_and_value("2 <= 3;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (3_f32))),
			SymbolKind::ExprOperator (ExprOperator::LessEqual),
		],
		Value::Bool(true));
		
		test_expr_and_its_stack_eq_and_value("1 + 1 ^ 10 <= 27 / 9;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (1_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (1_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (10_f32))),
			SymbolKind::ExprOperator (ExprOperator::Pow),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (27_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (9_f32))),
			SymbolKind::ExprOperator (ExprOperator::Div),
			SymbolKind::ExprOperator (ExprOperator::LessEqual),
		],
		Value::Bool(true));
	}
	
	fn test_expr_and_its_stack_eq_and_value(
		expr_str: &str, 
		correct_expr_stack: Vec<SymbolKind>,
		result: Value
	) {
		let mut tokens_iter = TokensIter::new();	
		tokens_iter.push_string(expr_str.to_string());
		
		let expr_stack: Vec<Symbol> = Expr::create_stack(
			&mut tokens_iter, 
			ExprContext::new(ExprContextKind::ValueToAssign)).unwrap();
		
		let syms_expr_stack: Vec<SymbolKind> = expr_stack.iter().map(|Symbol { kind, .. }| kind.clone()).collect();
		
		if syms_expr_stack == correct_expr_stack { 
			let mut tokens_iter = TokensIter::new();	
			tokens_iter.push_string(expr_str.to_string());
		
			let expr = Expr::new(&mut tokens_iter, ExprContextKind::ValueToAssign).unwrap();
			
			let builtin_func_defs = Vec::<BuiltinFuncDef>::new();
			let primitive_type_member_funcs_list = PrimitiveTypeMemberFuncsList::new();
			let context = Context::new(
				&builtin_func_defs,
				&primitive_type_member_funcs_list,
				Vec::<StructDef>::new());
			
			let ans: Value = expr.calc(&context);
			
			if ans != result {
				panic!("Wrong result for code '{}': {:?} != {:?}", expr_str, ans, result);
			}
			
			return; 
		}
		
		let max_len = std::cmp::max(expr_stack.len(), syms_expr_stack.len());
		for i in 0..max_len {
			match syms_expr_stack.get(i) {
				Some(sym) => print!("{:?}", sym),
				None => print!("None"),
			}
			match syms_expr_stack.get(i) == correct_expr_stack.get(i) {
				true => print!(" == "),
				false => {
					print!(" != "); 
				},
			}
			match correct_expr_stack.get(i) {
				Some(sym) => println!("{:?}", sym),
				None => println!("None"),
			}
		}
		
		panic!("Test failed ^^^");
	}
	
	
	fn test_expr_and_its_stack_eq(
		expr_str: &str, 
		correct_expr_stack: Vec<SymbolKind>,
	) {
		let mut tokens_iter = TokensIter::new();	
		tokens_iter.push_string(expr_str.to_string());
		
		let expr_stack: Vec<Symbol> = Expr::create_stack(
			&mut tokens_iter, 
			ExprContext::new(ExprContextKind::ValueToAssign)).unwrap();
		
		let syms_expr_stack: Vec<SymbolKind> = expr_stack.iter().map(|Symbol { kind, .. }| kind.clone()).collect();
		
		if syms_expr_stack == correct_expr_stack {		
			return; 
		}
		
		let max_len = std::cmp::max(expr_stack.len(), syms_expr_stack.len());
		for i in 0..max_len {
			match syms_expr_stack.get(i) {
				Some(sym) => print!("{:?}", sym),
				None => print!("None"),
			}
			match syms_expr_stack.get(i) == correct_expr_stack.get(i) {
				true => print!(" == "),
				false => {
					print!(" != "); 
				},
			}
			match correct_expr_stack.get(i) {
				Some(sym) => println!("{:?}", sym),
				None => println!("None"),
			}
		}
		
		panic!("Test failed ^^^");
	}
}