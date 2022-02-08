mod symbol;
mod expr_operator;

use super::token::{Token, TokenContent, TokensIter, Operator, Bracket, StatementOp, Keyword};
use super::InterpErr;
use super::value::Value;
use super::data_type::DataType;
use super::utils::{CharPos, CodePos};
use super::context::Context;
use symbol::{SymbolIterator, Symbol, SymbolKind, Operand};
use expr_operator::{ExprOperator, OpAssot};
use std::rc::Rc;

pub use symbol::StructLiteralField;

#[derive(Debug)]
pub struct Expr {
	expr_stack: Rc<Vec<Symbol>>,
	pos: CodePos,
}

impl Expr {	
	pub fn new(tokens_iter: &mut TokensIter, context_kind: ExprContextKind) -> Result<Self, InterpErr> {
		let expr_stack = Self::create_stack(tokens_iter, context_kind)?;
		
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
				
				SymbolKind::LeftRoundBracket => unreachable!(),
				SymbolKind::RightRoundBracket => unreachable!(),
				
				SymbolKind::ExprOperator (op) => {
					let result_sym: Symbol = op.apply(&mut calc_stack, context, sym.pos());
					calc_stack.push(result_sym);
				},
			}
		}
		
		let result: Value = calc_stack.pop()
			.unwrap()
			.unwrap_operand()
			.calc_in_place(context)
			.unwrap();
		assert_eq!(calc_stack.pop(), None);
		
		result
	}
	
	pub fn check_and_calc_data_type(&self, check_context: &Context) -> Result<DataType, InterpErr> {
		let mut type_calc_stack = Vec::<Symbol>::with_capacity(self.expr_stack.len());
		
		for sym in self.expr_stack.iter() {
			match sym.kind() {
				SymbolKind::Operand (_) => type_calc_stack.push(sym.clone()), // TODO: avoid cloning symbols
				
				SymbolKind::LeftRoundBracket => unreachable!(),
				SymbolKind::RightRoundBracket => unreachable!(),
				
				SymbolKind::ExprOperator (op) => {
					let dt: DataType = op.get_result_data_type(&mut type_calc_stack, check_context, sym.pos())?;
					type_calc_stack.push(Symbol { 
						pos: sym.pos(), 
						kind: SymbolKind::Operand (Operand::Value(dt.default_value())),
					}); // TODO: somehow place DataType here, not a massive symbol
				}, 
			}
		}
		
		assert_eq!(type_calc_stack.len(), 1);
		
		let data_type: DataType = type_calc_stack.pop()
			.unwrap()
			.unwrap_operand()
			.check_and_calc_data_type_in_place(check_context)?;
		
		Ok(data_type)
	}

	pub fn pos(&self) -> CodePos {
		self.pos
	}
	
	pub fn check_as_standalone_expression(&self, check_context: &Context) -> Result<(), InterpErr> {
		let mut type_calc_stack = Vec::<Symbol>::with_capacity(self.expr_stack.len());
		
		for sym in self.expr_stack.iter() {
			match sym.kind() {
				SymbolKind::Operand (opnd) => match opnd {
					Operand::Variable(_) | Operand::FuncCall { .. } => type_calc_stack.push(sym.clone()),
					_ => return Err( ExprErr::NotLhsExprSymbol(sym.pos).into() ),
				}, // TODO: avoid cloning symbols
				
				SymbolKind::LeftRoundBracket => unreachable!(),
				SymbolKind::RightRoundBracket => unreachable!(),
				
				SymbolKind::ExprOperator (op) => match op {
					ExprOperator::DotMemberAccess => {
						let dt: DataType = op.get_result_data_type(&mut type_calc_stack, check_context, sym.pos())?;
						type_calc_stack.push(Symbol { 
							pos: sym.pos(), 
							kind: SymbolKind::Operand (Operand::Value(dt.default_value())),
						}); // TODO: somehow place DataType here, not a massive symbol
					},
					_ => return Err( ExprErr::NotLhsExprSymbol(sym.pos).into() ),
				}
			}
		}
		
		assert_eq!(type_calc_stack.len(), 1);
		
		type_calc_stack.pop()
			.unwrap()
			.unwrap_operand()
			.check_and_calc_data_type_in_place(check_context)?;
		
		Ok(())
	}
	
	pub fn run_as_standalone_expression(&self, context: &Context) {
		let mut calc_stack = Vec::<Symbol>::with_capacity(self.expr_stack.len());
		
		for sym in self.expr_stack.iter() {
			match sym.kind() {
				SymbolKind::Operand (_) => calc_stack.push(sym.clone()), // TODO: avoid cloning symbols
				
				SymbolKind::LeftRoundBracket => unreachable!(),
				SymbolKind::RightRoundBracket => unreachable!(),
				
				SymbolKind::ExprOperator (op) => {
					let result_sym: Symbol = op.apply(&mut calc_stack, context, sym.pos());
					calc_stack.push(result_sym);
				},
			}
		}
			
		assert_eq!(calc_stack.len(), 1);
		
		calc_stack.pop()
			.unwrap()
			.unwrap_operand()
			.calc_in_place(context);
	}
	
	pub fn check_and_get_variable_to_set_type(&self, check_context: &Context) -> Result<DataType, InterpErr> {
		let mut type_calc_stack = Vec::<Symbol>::with_capacity(self.expr_stack.len());
		
		for sym in self.expr_stack.iter() {
			match sym.kind() {
				SymbolKind::Operand (opnd) => match opnd {
					Operand::Variable(_) | Operand::FuncCall { .. } => type_calc_stack.push(sym.clone()),
					_ => return Err( ExprErr::NotLhsExprSymbol(sym.pos).into() ),
				}, // TODO: avoid cloning symbols
				
				SymbolKind::LeftRoundBracket => unreachable!(),
				SymbolKind::RightRoundBracket => unreachable!(),
				
				SymbolKind::ExprOperator (op) => match op {
					ExprOperator::DotMemberAccess => {
						let dt: DataType = op.get_result_data_type(&mut type_calc_stack, check_context, sym.pos())?;
						type_calc_stack.push(Symbol { 
							pos: sym.pos(), 
							kind: SymbolKind::Operand (Operand::Value(dt.default_value())),
						}); // TODO: somehow place DataType here, not a massive symbol
					},
					_ => return Err( ExprErr::NotLhsExprSymbol(sym.pos).into() ),
				},
			}
		}
			
		assert_eq!(type_calc_stack.len(), 1);
		
		let data_type: DataType = type_calc_stack.pop()
			.unwrap()
			.unwrap_operand()
			.check_and_calc_data_type_in_place(check_context)?;
		
		Ok(data_type)
	}
	
	pub fn set_as_to_lhs(&self, value: Value, context: &mut Context) {
		let mut calc_stack = Vec::<Symbol>::with_capacity(self.expr_stack.len());
					
		for sym in self.expr_stack.iter() {
			match sym.kind() {
				SymbolKind::Operand (_) => {
					calc_stack.push(sym.clone());
				}, // TODO: avoid cloning symbols
				
				SymbolKind::LeftRoundBracket => unreachable!(),
				SymbolKind::RightRoundBracket => unreachable!(),
				
				SymbolKind::ExprOperator (op) => {
					match op {
						ExprOperator::DotMemberAccess => {
							let result_sym: Symbol = op.apply(&mut calc_stack, context, sym.pos());
							calc_stack.push(result_sym);
						}
						_ => unreachable!(),
					}
				},
			}
		}
			
		assert_eq!(calc_stack.len(), 1);
		
		let last_opnd: Operand = calc_stack.pop()
			.unwrap()
			.unwrap_operand();
		
		match last_opnd {
			Operand::Variable (var_name) => context.set_variable(&var_name, value).unwrap(),
			Operand::StructFieldValue (value_rc) => *value_rc.borrow_mut() = value,
			opnd @ _ => panic!("Wrong last operand: {:#?}", opnd),
		}
	}
	
	fn create_stack(tokens_iter: &mut TokensIter, context_kind: ExprContextKind) -> Result<Vec<Symbol>, InterpErr> {
		use std::cmp::Ordering;
		
		let context = ExprContext::new(context_kind);
		let mut tmp_stack = Vec::<Symbol>::new();
		let mut expr_stack = Vec::<Symbol>::new();
		
		'outer: for symbol_result in SymbolIterator::new(tokens_iter, context) {
			let symbol: Symbol = symbol_result?;
			
			match symbol.kind() {
				SymbolKind::Operand(_) => expr_stack.push(symbol),
				
				SymbolKind::LeftRoundBracket => tmp_stack.push(symbol),
				
				SymbolKind::RightRoundBracket => {
					while let Some(top_sym) = tmp_stack.pop() {
						match top_sym.kind() {
							SymbolKind::Operand (..) => unreachable!(),
							SymbolKind::LeftRoundBracket => continue 'outer,
							SymbolKind::RightRoundBracket => unreachable!(),
							SymbolKind::ExprOperator (..) => expr_stack.push(top_sym),
						}
					}
					
					return Err(ExprErr::UnpairedBracket (symbol.pos()).into());
				},
				
				SymbolKind::ExprOperator (ref optr) => {
					match optr {
						ExprOperator::UnPlus
						| ExprOperator::UnMinus
						| ExprOperator::Not 
						=> tmp_stack.push(symbol),
						
						ExprOperator::LogicalOr
						| ExprOperator::LogicalAnd
						| ExprOperator::LogicalXor
						
						| ExprOperator::Equal
						| ExprOperator::NotEqual
						
						| ExprOperator::Less
						| ExprOperator::LessEqual
						| ExprOperator::Greater
						| ExprOperator::GreaterEqual
						
						| ExprOperator::BinPlus
						| ExprOperator::BinMinus
						
						| ExprOperator::Div
						| ExprOperator::Mul
						
						| ExprOperator::Pow
						
						| ExprOperator::DotMemberAccess
						| ExprOperator::Index
						=> {							
							'tmp_to_expr: while let Some(top_tok_sym) = tmp_stack.last() {
								match top_tok_sym.kind() {
									SymbolKind::Operand (..) => unreachable!(),
									
									SymbolKind::LeftRoundBracket => break 'tmp_to_expr,
									
									SymbolKind::RightRoundBracket => unreachable!(),
									
									SymbolKind::ExprOperator ( top_ref ) => {
										let cmp_result: Ordering = top_ref.rank().cmp(&optr.rank());
										
										match cmp_result {
											Ordering::Greater => {
												let popped: Symbol = tmp_stack.pop().unwrap();
												expr_stack.push(popped);
											},
											
											Ordering::Equal => match (top_ref.assot(), optr.assot()) {
												(OpAssot::Right, OpAssot::Right) => break 'tmp_to_expr,
												
												_ => {
													let popped: Symbol = tmp_stack.pop().unwrap();
													expr_stack.push(popped);
												},
											},
											
											Ordering::Less => break 'tmp_to_expr,
										}
									}
								}
							}
							
							tmp_stack.push(symbol);
						},
					}
				},
			}
		}
				
		while let Some(top_sym) = tmp_stack.pop() {
			match top_sym.kind() {
				SymbolKind::Operand (..) => expr_stack.push(top_sym),
				SymbolKind::LeftRoundBracket => return Err( unpaired_bracket(top_sym.pos()) ),
				SymbolKind::RightRoundBracket => unreachable!(),
				SymbolKind::ExprOperator (..) => expr_stack.push(top_sym),
			}
		}
		
		if expr_stack.len() == 0 {
			let found_token = tokens_iter.next_or_end_reached_err()?;
			return Err(ExprErr::ExpectedExprButFound(found_token.pos()).into());
		}
				
		Ok(expr_stack)
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
	RightAssignOperand,
	LeftAssignOperand,
	ToReturn,
	FunctionArg,
	IfCondition,
	StructFieldValue,
	IndexValue,
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
			// iterator doesn't give comment tokens by default
			TokenContent::Comment (_) => unreachable!(),
			TokenContent::Operator (op) => match op {
				Operator::Assign => match self.kind {
					ExprContextKind::LeftAssignOperand => Ok(true),
					_ => Err(unexpected(tok.pos())),
				},
				_ => Ok(false),
			},
			TokenContent::Number (..) | 
				TokenContent::Name (..) |
				TokenContent::BuiltinName (..) |
				TokenContent::StringLiteral (..)
				=> Ok(false),
			TokenContent::Bracket (_) 
				=> self.check_brackets(tok),
			TokenContent::StatementOp (st_op) => match st_op {
				StatementOp::Dot => Ok(false),
				StatementOp::Colon => Err(unexpected(tok.pos())),
				StatementOp::ThinArrow => Err(unexpected(tok.pos())),
				StatementOp::Comma => match self.kind {
					ExprContextKind::RightAssignOperand => Err(unexpected(tok.pos())),
					ExprContextKind::LeftAssignOperand => Ok(false),
					ExprContextKind::IfCondition => Err(unexpected(tok.pos())),
					ExprContextKind::ToReturn => Err(unexpected(tok.pos())),
					ExprContextKind::FunctionArg => Ok(true),
					ExprContextKind::StructFieldValue => Ok(true),
					ExprContextKind::IndexValue => Err(unexpected(tok.pos())),
				},
				StatementOp::Semicolon => match self.kind {
					ExprContextKind::RightAssignOperand => Ok(true),
					ExprContextKind::LeftAssignOperand => Ok(true),
					ExprContextKind::IfCondition => Err(unexpected(tok.pos())),
					ExprContextKind::ToReturn => Ok(true),
					ExprContextKind::FunctionArg => Err(unexpected(tok.pos())),
					ExprContextKind::StructFieldValue => Err(unexpected(tok.pos())),
					ExprContextKind::IndexValue => Err(unexpected(tok.pos())),
				}
			},
			TokenContent::Keyword (kw) => match kw {
				Keyword::Var | 
					Keyword::If | 
					Keyword::Else | 
					Keyword::While | 
					Keyword::F | 
					Keyword::Struct |
					Keyword::Return => Err(unexpected(tok.pos())),
				Keyword::True => Ok(false),
				Keyword::False => Ok(false),
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
							ExprContextKind::RightAssignOperand => Err( unpaired_bracket(br_tok.pos()) ),
							ExprContextKind::LeftAssignOperand => Err( unpaired_bracket(br_tok.pos()) ),
							ExprContextKind::IfCondition => Ok(true),
							ExprContextKind::ToReturn => Err( unpaired_bracket(br_tok.pos()) ),
							ExprContextKind::StructFieldValue => Err( unpaired_bracket(br_tok.pos()) ),
							ExprContextKind::FunctionArg => Ok(true),
							ExprContextKind::IndexValue => Err( unpaired_bracket(br_tok.pos()) ),
						}
					}
				},
				Bracket::LeftCurly => match self.kind {
					ExprContextKind::RightAssignOperand => Ok(false),
					ExprContextKind::LeftAssignOperand => Ok(false),
					ExprContextKind::IfCondition => Ok(false),
					ExprContextKind::FunctionArg => Ok(false),
					ExprContextKind::ToReturn => Ok(false),
					ExprContextKind::StructFieldValue => Ok(true),
					ExprContextKind::IndexValue => Err(unexpected(br_tok.pos())),
				},
				Bracket::RightCurly => match self.kind {
					ExprContextKind::RightAssignOperand => Ok(false),
					ExprContextKind::LeftAssignOperand => Ok(false),
					ExprContextKind::IfCondition => Ok(false),
					ExprContextKind::FunctionArg => Ok(false),
					ExprContextKind::ToReturn => Ok(false),
					ExprContextKind::StructFieldValue => Ok(true),
					ExprContextKind::IndexValue => Err(unexpected(br_tok.pos())),
				},
				Bracket::LeftSquared => Ok(false),
				Bracket::RightSquared => match self.kind {
					ExprContextKind::IndexValue => Ok(true),
					_ => Ok(false),
				},
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
	WrongOperandsForOperator { 
		operator_pos: CodePos,
		descr: String,
	},
	NotEnoughOperandsForOperator { 
		operator_pos: CodePos,
		descr: String,
	},
	NotStruct (CodePos),
	NotLhsExprSymbol (CodePos),
}

impl ExprErr {
	fn not_enough_operands_for_operator(operator_pos: CodePos, provided_cnt: usize, required_cnt: usize) -> Self {
		ExprErr::NotEnoughOperandsForOperator { 
			operator_pos,
			descr: format!("Expected {} operand(-s) for operator, but found {}", required_cnt, provided_cnt),
		}
	}
	fn wrong_operands_type_for_operator(op: ExprOperator, operator_pos: CodePos, operands_types: &[&DataType]) -> Self {
		let mut descr: String = format!("Operator '{:?}' can't be applied to operands with type(-s) [", op);
		
		let mut is_first = true;
		for (ind, opt) in operands_types.iter().enumerate() {
			if !is_first {
				descr.push_str(", ");
			}
			is_first = false;
			descr.push_str(&format!("{}: {:?}", ind, opt));
		}
		
		descr.push_str("]");
		
		ExprErr::WrongOperandsTypeForOperator { 
			operator_pos,
			descr,
		}
	}
	fn wrong_operands_for_operator(op: ExprOperator, operator_pos: CodePos, operands: &[&Operand]) -> Self {
		let mut descr: String = format!("Operator '{:?}' can't be applied to operands [", op);
		
		let mut is_first = true;
		for op in operands {
			if !is_first {
				descr.push_str(", ");
			}
			is_first = false;
			descr.push_str(&format!("{}", op));
		}
		descr.push_str("]");
		
		ExprErr::WrongOperandsForOperator { 
			operator_pos,
			descr,
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
			ExprErr::WrongOperandsForOperator { ref descr, .. } => write!(f, "{}", descr),
			ExprErr::NotEnoughOperandsForOperator { ref descr, .. } => write!(f, "{}", descr),
			ExprErr::NotStruct (_) => write!(f, "Value is not a struct"),
			ExprErr::NotLhsExprSymbol (_) => write!(f, "Not allowed to use as left hand side of '=' operator"),
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
	//use super::super::token::*;
use super::{Expr, ExprContextKind};
	use super::symbol::{SymbolKind, Symbol, Operand};
	use super::super::value::Value;
	use std::rc::Rc;
	use super::super::context::Context;
	use super::super::token::TokensIter;
	use super::super::primitive_type_member_builtin_funcs_list::PrimitiveTypeMemberBuiltinFuncsList;
	use super::super::struct_def::StructDef;
	use super::super::builtin_func::BuiltinFuncDef;
	use super::expr_operator::ExprOperator;
use super::super::utils::{NameToken, CodePos, CharPos};
	
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
		
		test_expr_and_its_stack_eq_and_value("6 / 2;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (6_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::ExprOperator (ExprOperator::Div),
		],
		Value::Float32(6_f32 / 2_f32));
		
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
			SymbolKind::Operand (Operand::Variable (new_name_token("a", false))),
			SymbolKind::Operand (Operand::FuncCall {
				func_name: new_name_token("add", false),
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
				func_name: new_name_token("add", true),
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
			SymbolKind::Operand (Operand::Variable (new_name_token("a", false))),
			SymbolKind::Operand (Operand::FuncCall {
				func_name: new_name_token("foo1", false),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
			SymbolKind::Operand (Operand::Variable (new_name_token("b", false))),
			SymbolKind::Operand (Operand::FuncCall {
				func_name: new_name_token("foo2", false),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
		]);
		
		test_expr_and_its_stack_eq("foo1().foo3() + b.foo2();", vec![
			SymbolKind::Operand (Operand::FuncCall {
				func_name: new_name_token("foo1", false),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::Operand (Operand::FuncCall {
				func_name: new_name_token("foo3", false),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
			SymbolKind::Operand (Operand::Variable (new_name_token("b", false))),
			SymbolKind::Operand (Operand::FuncCall {
				func_name: new_name_token("foo2", false),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
		]);
		
		test_expr_and_its_stack_eq("2 * a.foo1(c.foo3().foo5() - 3) ^ b.foo2() / d.foo4();", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::Operand (Operand::Variable (new_name_token("a", false))),
			SymbolKind::Operand (Operand::FuncCall {
				func_name: new_name_token("foo1", false),
				arg_exprs: vec![
					// arg 1
					Expr {
						pos: zero_pos,
						expr_stack: Rc::new(vec![
							Symbol { kind: SymbolKind::Operand (Operand::Variable (new_name_token("c", false))), pos: zero_pos, },
							Symbol { kind: SymbolKind::Operand (Operand::FuncCall {
								func_name: new_name_token("foo3", false),
								arg_exprs: Vec::new(),
							}), pos: zero_pos, },
							Symbol { kind: SymbolKind::ExprOperator (ExprOperator::DotMemberAccess), pos: zero_pos, },
							Symbol { kind: SymbolKind::Operand (Operand::FuncCall {
								func_name: new_name_token("foo5", false),
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
			
			SymbolKind::Operand (Operand::Variable (new_name_token("b", false))),
			SymbolKind::Operand (Operand::FuncCall {
				func_name: new_name_token("foo2", false),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),

			SymbolKind::ExprOperator (ExprOperator::Pow),
			
			SymbolKind::ExprOperator (ExprOperator::Mul),
			
			SymbolKind::Operand (Operand::Variable (new_name_token("d", false))),
			SymbolKind::Operand (Operand::FuncCall {
				func_name: new_name_token("foo4", false),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
			
			SymbolKind::ExprOperator (ExprOperator::Div),
		]);
	}
	
	fn new_name_token(name: &str, is_builtin: bool) -> NameToken {
		NameToken::new_with_pos(
			name.to_string(), 
			CodePos::from(CharPos::new()), 
			is_builtin)
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
	
	#[test]
	fn check_index_operator() {
		test_expr_and_its_stack_eq_and_value(
		r#"  "Hello!"[0];"#, 
		vec![
			SymbolKind::Operand (Operand::Value (Value::from("Hello!"))),
			SymbolKind::Operand (Operand::IndexExpr (Expr {
				expr_stack: Rc::new(vec![
					Symbol { kind: SymbolKind::Operand (Operand::Value (Value::Float32 (0_f32))), pos: CodePos::from(CharPos::new()) },
				]),
				pos: CodePos::from(CharPos::new()),
			} ) ),
			SymbolKind::ExprOperator (ExprOperator::Index),
		],
		Value::from('H'));
		
		test_expr_and_its_stack_eq_and_value(
		r#"  ("Hello, " + "world!")[32 - 21];  "#, 
		vec![
			SymbolKind::Operand (Operand::Value (Value::from("Hello, "))),
			SymbolKind::Operand (Operand::Value (Value::from("world!"))),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::Operand (Operand::IndexExpr (Expr {
				expr_stack: Rc::new(vec![
					Symbol { kind: SymbolKind::Operand (Operand::Value (Value::Float32 (32_f32))), pos: CodePos::from(CharPos::new()) },
					Symbol { kind: SymbolKind::Operand (Operand::Value (Value::Float32 (21_f32))), pos: CodePos::from(CharPos::new()) },
					Symbol { kind: SymbolKind::ExprOperator (ExprOperator::BinMinus), pos: CodePos::from(CharPos::new()) },
				]),
				pos: CodePos::from(CharPos::new()),
			} ) ),
			SymbolKind::ExprOperator (ExprOperator::Index),
		],
		Value::from('d'));
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
			ExprContextKind::RightAssignOperand).unwrap();
		
		let syms_expr_stack: Vec<SymbolKind> = expr_stack.iter().map(|Symbol { kind, .. }| kind.clone()).collect();
		
		if syms_expr_stack == correct_expr_stack { 
			let mut tokens_iter = TokensIter::new();	
			tokens_iter.push_string(expr_str.to_string());
		
			let expr = Expr::new(&mut tokens_iter, ExprContextKind::RightAssignOperand).unwrap();
			
			let builtin_func_defs = Vec::<BuiltinFuncDef>::new();
			let primitive_type_member_funcs_list = PrimitiveTypeMemberBuiltinFuncsList::new();
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
		
		println!("\t\tGOT vs CORRECT");
		
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
			ExprContextKind::RightAssignOperand).unwrap();
		
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