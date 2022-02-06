use super::super::InterpErr;
use super::super::value::Value;
use super::super::data_type::{DataType, Primitive};
use super::super::builtin_func::BuiltinFuncDef;
use super::super::utils::CodePos;
use super::super::statement::FuncKind;
use super::super::context::Context;
use super::super::token::{TokenContent, Operator, StatementOp};
use super::symbol::{Symbol, SymbolKind, Operand};
use super::ExprErr;

//------------------------------- ExprOperator ----------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
pub enum ExprOperator {
	LogicalOr = 0_usize,
	LogicalAnd,
	LogicalXor,
	
	Equal,
	NotEqual,
	
	Less,
	LessEqual,
	Greater,
	GreaterEqual,
	
	BinPlus,
	BinMinus,
	
	Div,
	Mul,
	
	UnPlus,
	UnMinus,
	Not,
	
	Pow,
	
	DotMemberAccess,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct OpInfo {
	arity: OpArity,
	rank: u32,
	assot: OpAssot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpArity {
	Binary,
	Unary,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpAssot {
	Left,
	Right,
}

pub static OP_ATTRS: [OpInfo; 18] = [
	OpInfo { arity: OpArity::Binary, rank: 0, assot: OpAssot::Left },		//LogicalOr
	OpInfo { arity: OpArity::Binary, rank: 1, assot: OpAssot::Left },		//LogicalAnd
	OpInfo { arity: OpArity::Binary, rank: 2, assot: OpAssot::Left },		//LogicalXor
	
	OpInfo { arity: OpArity::Binary, rank: 3, assot: OpAssot::Left },		//Equal
	OpInfo { arity: OpArity::Binary, rank: 3, assot: OpAssot::Left },		//NotEqual
	
	OpInfo { arity: OpArity::Binary, rank: 4, assot: OpAssot::Left },		//Less
	OpInfo { arity: OpArity::Binary, rank: 4, assot: OpAssot::Left },		//LessEqual
	OpInfo { arity: OpArity::Binary, rank: 4, assot: OpAssot::Left },		//Greater
	OpInfo { arity: OpArity::Binary, rank: 4, assot: OpAssot::Left },		//GreaterEqual
	
	OpInfo { arity: OpArity::Binary, rank: 5, assot: OpAssot::Left },		//BinPlus
	OpInfo { arity: OpArity::Binary, rank: 5, assot: OpAssot::Left }, 	//BinMinus
	
	OpInfo { arity: OpArity::Binary, rank: 6, assot: OpAssot::Left }, 	//Div
	OpInfo { arity: OpArity::Binary, rank: 6, assot: OpAssot::Left }, 	//Mul
	
	OpInfo { arity: OpArity::Unary, rank: 7, assot: OpAssot::Left },		//UnPlus
	OpInfo { arity: OpArity::Unary, rank: 7, assot: OpAssot::Left },		//UnMinus
	OpInfo { arity: OpArity::Unary, rank: 7, assot: OpAssot::Left },		//Not
	
	OpInfo { arity: OpArity::Binary, rank: 8, assot: OpAssot::Right }, 	//Pow
	
	OpInfo { arity: OpArity::Binary, rank: 9, assot: OpAssot::Left }, 	//Dot
];

impl ExprOperator {
	pub fn new_bin(tc: TokenContent) -> Self {
		match tc {
			TokenContent::Operator (ref tok_op) => match tok_op {
				Operator::Plus => ExprOperator::BinPlus,
				Operator::Minus => ExprOperator::BinMinus,
				Operator::Mul => ExprOperator::Mul,
				Operator::Div => ExprOperator::Div,
				Operator::Pow => ExprOperator::Pow,
				Operator::Equal => ExprOperator::Equal,
				Operator::Greater => ExprOperator::Greater,
				Operator::GreaterEqual => ExprOperator::GreaterEqual,
				Operator::Less => ExprOperator::Less,
				Operator::LessEqual => ExprOperator::LessEqual,
				Operator::NotEqual => ExprOperator::NotEqual,
				Operator::LogicalAnd => ExprOperator::LogicalAnd,
				Operator::LogicalOr => ExprOperator::LogicalOr,
				Operator::LogicalXor => ExprOperator::LogicalXor,
				Operator::Not => ExprOperator::Not,
				Operator::Assign => panic!("Wrong input: {:?}", tc),
			},
			TokenContent::StatementOp (ref st_op) => match st_op {
				StatementOp::Dot => ExprOperator::DotMemberAccess,
				_ => panic!("Wrong input: {:?}", tc),
			},
			_ => panic!("Wrong input: {:?}", tc),
		}
	}
	
	pub fn new_un_pref(tc: TokenContent) -> Self {
		match tc {
			TokenContent::Operator (tok_op) => match tok_op {
				Operator::Plus => ExprOperator::UnPlus,
				Operator::Minus => ExprOperator::UnMinus,
				Operator::Mul 
					| Operator::Div 
					| Operator::Equal 
					| Operator::Assign 
					| Operator::Pow 
					| Operator::Greater 
					| Operator::GreaterEqual
					| Operator::Less 
					| Operator::LessEqual 
					| Operator::NotEqual
					| Operator::Not
					| Operator::LogicalAnd
					| Operator::LogicalOr
					| Operator::LogicalXor
				=> panic!("Wrong input: {:?}", tc),
			},
			_ => panic!("Wrong input: {:?}", tc),
		}
	}
	
	pub fn rank(&self) -> u32 {
		OP_ATTRS[*self as usize].rank
	}
	
	pub fn assot(&self) -> OpAssot {
		OP_ATTRS[*self as usize].assot
	}

	pub fn apply(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {	
		use ExprOperator::*;
		
		if let DotMemberAccess = self {
			let rhs: Symbol = calc_stack
				.pop()
				.unwrap();
				
			let lhs: Symbol = calc_stack
				.pop()
				.unwrap();
			
			match (lhs.kind(), rhs.kind()) {
				(SymbolKind::Operand (Operand::Value (ref value)), SymbolKind::Operand (Operand::FuncCall {
					ref kind,
					ref func_name,
					ref arg_exprs,
				})) => {
					match kind {
						FuncKind::Builtin => {
							let func_def: &BuiltinFuncDef = value.get_type()
								.find_member_builtin_func(&func_name, context).unwrap();
							
							let mut args_values = Vec::<Value>::with_capacity(arg_exprs.len() + 1);
							args_values.push(value.clone());
							for expr in arg_exprs {
								let value: Value = expr.calc(context);
								args_values.push(value);
							}
							
							return func_def.call(args_values).unwrap();
						},
						
						FuncKind::UserDefined => todo!(),
					}
				},
				(SymbolKind::Operand (Operand::Variable (ref var_name)), SymbolKind::Operand (Operand::FuncCall {
					ref kind,
					ref func_name,
					ref arg_exprs,
				})) => {
					let value: &Value = context.get_variable_value(var_name).unwrap();
					match kind {
						FuncKind::Builtin => {
							let func_def: &BuiltinFuncDef = value.get_type()
								.find_member_builtin_func(&func_name, context).unwrap();
							
							let mut args_values = Vec::<Value>::with_capacity(arg_exprs.len() + 1);
							args_values.push(value.clone());
							for expr in arg_exprs {
								let value: Value = expr.calc(context);
								args_values.push(value);
							}
							
							return func_def.call(args_values).unwrap();
						},
						
						FuncKind::UserDefined => todo!(),
					}
				},
				_ => todo!(),
			}
		} else {
			match self {
				BinPlus => self.apply_bin_plus(calc_stack, context),
				BinMinus => self.apply_bin_minus(calc_stack, context),
				Div => self.apply_bin_div(calc_stack, context),
				Mul => self.apply_bin_mul(calc_stack, context),
				Pow => self.apply_bin_pow(calc_stack, context),
				UnPlus => self.apply_unary_plus(calc_stack, context),
				UnMinus => self.apply_unary_minus(calc_stack, context),
				Equal => self.apply_equal(calc_stack, context),
				NotEqual => self.apply_not_equal(calc_stack, context),
				Not => self.apply_not(calc_stack, context),
				Greater => self.apply_greater(calc_stack, context),
				GreaterEqual => self.apply_greater_equal(calc_stack, context),
				Less => self.apply_less(calc_stack, context),
				LessEqual => self.apply_less_equal(calc_stack, context),
				LogicalAnd => self.apply_logical_and(calc_stack, context),
				LogicalOr => self.apply_logical_or(calc_stack, context),
				LogicalXor => self.apply_logical_xor(calc_stack, context),
				DotMemberAccess => todo!(),
			}
		}
	}
	
	pub fn get_result_data_type(self, calc_stack: &mut Vec<Symbol>, check_context: &Context, operator_pos: CodePos) -> Result<DataType, InterpErr> {
		use ExprOperator::*;
		
		if let DotMemberAccess = self {
			let rhs: Symbol = calc_stack.pop()
				.ok_or(InterpErr::from(ExprErr::not_enough_operands_for_operator(operator_pos, 0, 2)) )?;
				
			let lhs: Symbol = calc_stack.pop()
				.ok_or(InterpErr::from(ExprErr::not_enough_operands_for_operator(operator_pos, 1, 2)) )?;
				
			match (lhs.kind(), rhs.kind()) {
				(SymbolKind::Operand (Operand::Value (ref value)), SymbolKind::Operand (Operand::FuncCall {
					ref kind,
					ref func_name,
					ref arg_exprs,
				})) => {
					match kind {
						FuncKind::Builtin => {
							let func_def: &BuiltinFuncDef = value.get_type()
								.find_member_builtin_func(&func_name, check_context)?;
							
							func_def.check_args_as_member_function(func_name, arg_exprs, value, lhs.pos(), check_context)?;
							
							return Ok(func_def.return_type().clone());
						},
						
						FuncKind::UserDefined => todo!(),
					}
				},
				(SymbolKind::Operand (Operand::Variable (ref var_name)), SymbolKind::Operand (Operand::FuncCall {
					ref kind,
					ref func_name,
					ref arg_exprs,
				})) => {
					let value: &Value = check_context.get_variable_value(&var_name)?;
					match kind {
						FuncKind::Builtin => {
							let func_def: &BuiltinFuncDef = value.get_type()
								.find_member_builtin_func(&func_name, check_context)?;
							
							func_def.check_args_as_member_function(func_name, arg_exprs, value, lhs.pos(), check_context)?;
							
							return Ok(func_def.return_type().clone());
						},
						
						FuncKind::UserDefined => todo!(),
					}
				},
				_ => todo!(),
			}
		} else {
			match OP_ATTRS[self as usize].arity {
				OpArity::Binary => {
					let rhs: DataType = calc_stack.pop()
						.ok_or(InterpErr::from(ExprErr::not_enough_operands_for_operator(operator_pos, 0, 2)) )?
						.unwrap_operand()
						.check_and_calc_data_type_in_place(check_context)?;
						
					let lhs: DataType = calc_stack.pop()
						.ok_or(InterpErr::from(ExprErr::not_enough_operands_for_operator(operator_pos, 1, 2)) )?
						.unwrap_operand()
						.check_and_calc_data_type_in_place(check_context)?;
					
					let result = match self {
						BinPlus => self.get_bin_plus_result_type(&rhs, &lhs),
						BinMinus => self.get_bin_minus_result_type(&rhs, &lhs),
						Div => self.get_bin_div_result_type(&rhs, &lhs),
						Mul => self.get_bin_mul_result_type(&rhs, &lhs),
						Pow => self.get_bin_pow_result_type(&rhs, &lhs),
						Equal => self.get_equal_result_type(&rhs, &lhs),
						NotEqual => self.get_not_equal_result_type(&rhs, &lhs),
						Greater => self.get_greater_result_type(&rhs, &lhs),
						GreaterEqual => self.get_greater_equal_result_type(&rhs, &lhs),
						Less => self.get_less_result_type(&rhs, &lhs),
						LessEqual => self.get_less_equal_result_type(&rhs, &lhs),
						LogicalAnd => self.get_logical_and_result_type(&rhs, &lhs),
						LogicalOr => self.get_logical_or_result_type(&rhs, &lhs),
						LogicalXor => self.get_logical_xor_result_type(&rhs, &lhs),
						_ => unreachable!(),
					};
					
					match result {
						Ok(dt) => Ok(dt),
						Err(()) => Err( InterpErr::from(ExprErr::wrong_operands_type_for_operator(
							self, operator_pos, &[&lhs, &rhs])) ),
					}
				},
				OpArity::Unary => {
					let op: DataType = calc_stack
						.pop()
						.ok_or(InterpErr::from(ExprErr::not_enough_operands_for_operator(operator_pos, 0, 1)) )?
						.unwrap_operand()
						.check_and_calc_data_type_in_place(check_context)?;
					
					let result = match self {
						UnPlus => self.get_unary_plus_result_type(&op),
						UnMinus => self.get_unary_minus_result_type(&op),
						Not => self.get_not_result_type(&op),
						_ => unreachable!(),
					};
					
					match result {
						Ok(dt) => Ok(dt),
						Err(()) => Err( InterpErr::from(ExprErr::wrong_operands_type_for_operator(
							self, operator_pos, &[&op])) ),
					}
				},
			}
		}
	}
	
	
	fn apply_bin_plus(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Float32(val1 + val2),
			(Value::String (val1), Value::String (val2)) => {
				let mut res: String = val1.clone();
				res.push_str(&val2);
				Value::String(res)
			},
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}
	
	fn apply_bin_minus(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Float32(val1 - val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}
	
	fn apply_bin_mul(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Float32(val1 * val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}
	
	fn apply_bin_div(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Float32(val1 / val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}
	
	fn apply_bin_pow(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Float32(val1.powf(val2)),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}
	
	fn apply_equal(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Bool(val1 == val2),
			(Value::String (val1), Value::String (val2)) => Value::Bool(val1 == val2),
			(Value::Bool (val1), Value::Bool (val2)) => Value::Bool(val1 == val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}

	fn apply_not_equal(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Bool(val1 != val2),
			(Value::String (val1), Value::String (val2)) => Value::Bool(val1 != val2),
			(Value::Bool (val1), Value::Bool (val2)) => Value::Bool(val1 != val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}

	fn apply_greater(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Bool(val1 > val2),
			(Value::String (val1), Value::String (val2)) => Value::Bool(val1 > val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}

	fn apply_greater_equal(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Bool(val1 >= val2),
			(Value::String (val1), Value::String (val2)) => Value::Bool(val1 >= val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}

	fn apply_less(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Bool(val1 < val2),
			(Value::String (val1), Value::String (val2)) => Value::Bool(val1 < val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}

	fn apply_less_equal(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Bool(val1 <= val2),
			(Value::String (val1), Value::String (val2)) => Value::Bool(val1 <= val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}

	fn apply_logical_and(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Bool (val1), Value::Bool (val2)) => Value::Bool(val1 && val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}

	fn apply_logical_or(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Bool (val1), Value::Bool (val2)) => Value::Bool(val1 || val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}

	fn apply_logical_xor(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Bool (val1), Value::Bool (val2)) => Value::Bool(val1 ^ val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}
	
	fn take_2_values(calc_stack: &mut Vec<Symbol>, context: &Context) -> (Value, Value) {
		let rhs: Value = calc_stack
			.pop()
			.unwrap()
			.unwrap_operand()
			.calc_in_place(context);
			
		let lhs: Value = calc_stack
			.pop()
			.unwrap()
			.unwrap_operand()
			.calc_in_place(context);
			
		(lhs, rhs)
	}
	
	
	fn apply_unary_plus(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let op: Value = Self::take_1_value(calc_stack, context);
		match op {
			Value::Float32 (val1) => Value::Float32(val1),
			_ => panic!("Wrong type {:?} for operand {:?}", op, self),
		}
	}
	
	fn apply_unary_minus(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let op: Value = Self::take_1_value(calc_stack, context);
		match op {
			Value::Float32 (val1) => Value::Float32(-val1),
			_ => panic!("Wrong type {:?} for operand {:?}", op, self),
		}
	}
	
	fn apply_not(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let op: Value = Self::take_1_value(calc_stack, context);
		match op {
			Value::Bool (val1) => Value::Bool(!val1),
			_ => panic!("Wrong type {:?} for operand {:?}", op, self),
		}
	}
	
	fn take_1_value(calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let op: Value = calc_stack
			.pop()
			.unwrap()
			.unwrap_operand()
			.calc_in_place(context);
		
		op
	}
	
	//---------------- Result data type -----------------------
	
	fn get_bin_plus_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => 
				Ok(DataType::Primitive (Primitive::Float32)),
			(DataType::Primitive (Primitive::String), DataType::Primitive (Primitive::String)) => 
				Ok(DataType::Primitive (Primitive::String)),
			_ => return Err(()),
		}
	}
	
	fn get_bin_minus_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => Ok(DataType::Primitive (Primitive::Float32)),
			_ => return Err(()),
		}
	}
	
	fn get_bin_mul_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => Ok(DataType::Primitive (Primitive::Float32)),
			_ => return Err(()),
		}
	}
	
	fn get_bin_div_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => Ok(DataType::Primitive (Primitive::Float32)),
			_ => return Err(()),
		}
	}
	
	fn get_bin_pow_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => Ok(DataType::Primitive (Primitive::Float32)),
			_ => return Err(()),
		}
	}
	
	fn get_equal_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => Ok(DataType::Primitive (Primitive::Bool)),
			(DataType::Primitive (Primitive::String), DataType::Primitive (Primitive::String)) => Ok(DataType::Primitive (Primitive::Bool)),
			(DataType::Primitive (Primitive::Bool), DataType::Primitive (Primitive::Bool)) => Ok(DataType::Primitive (Primitive::Bool)),
			_ => return Err(()),
		}
	}

	fn get_not_equal_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => Ok(DataType::Primitive (Primitive::Bool)),
			(DataType::Primitive (Primitive::String), DataType::Primitive (Primitive::String)) => Ok(DataType::Primitive (Primitive::Bool)),
			(DataType::Primitive (Primitive::Bool), DataType::Primitive (Primitive::Bool)) => Ok(DataType::Primitive (Primitive::Bool)),
			_ => return Err(()),
		}
	}

	fn get_greater_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => Ok(DataType::Primitive (Primitive::Bool)),
			(DataType::Primitive (Primitive::String), DataType::Primitive (Primitive::String)) => Ok(DataType::Primitive (Primitive::Bool)),
			_ => return Err(()),
		}
	}

	fn get_greater_equal_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => Ok(DataType::Primitive (Primitive::Bool)),
			(DataType::Primitive (Primitive::String), DataType::Primitive (Primitive::String)) => Ok(DataType::Primitive (Primitive::Bool)),
			_ => return Err(()),
		}
	}

	fn get_less_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => Ok(DataType::Primitive (Primitive::Bool)),
			(DataType::Primitive (Primitive::String), DataType::Primitive (Primitive::String)) => Ok(DataType::Primitive (Primitive::Bool)),
			_ => return Err(()),
		}
	}

	fn get_less_equal_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => Ok(DataType::Primitive (Primitive::Bool)),
			(DataType::Primitive (Primitive::String), DataType::Primitive (Primitive::String)) => Ok(DataType::Primitive (Primitive::Bool)),
			_ => return Err(()),
		}
	}

	fn get_logical_and_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Bool), DataType::Primitive (Primitive::Bool)) => Ok(DataType::Primitive (Primitive::Bool)),
			_ => return Err(()),
		}
	}

	fn get_logical_or_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Bool), DataType::Primitive (Primitive::Bool)) => Ok(DataType::Primitive (Primitive::Bool)),
			_ => return Err(()),
		}
	}

	fn get_logical_xor_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Bool), DataType::Primitive (Primitive::Bool)) => Ok(DataType::Primitive (Primitive::Bool)),
			_ => return Err(()),
		}
	}

	
	fn get_unary_plus_result_type(&self, operand: &DataType) -> Result<DataType, ()> {
		match operand {
			DataType::Primitive (Primitive::Float32) => Ok(DataType::Primitive (Primitive::Float32)),
			_ => return Err(()),
		}
	}
	
	fn get_unary_minus_result_type(&self, operand: &DataType) -> Result<DataType, ()> {
		match operand {
			DataType::Primitive (Primitive::Float32) => Ok(DataType::Primitive (Primitive::Float32)),
			_ => return Err(()),
		}
	}
	
	fn get_not_result_type(&self, operand: &DataType) -> Result<DataType, ()> {
		match operand {
			DataType::Primitive (Primitive::Bool) => Ok(DataType::Primitive (Primitive::Bool)),
			_ => return Err(()),
		}
	}
}

//------------------------------- Tests ----------------------------------

#[cfg(test)]
mod tests {
	use super::*;
	
	#[test]
	fn lookup_array_initialization() {
		assert_eq!(
			OP_ATTRS[ExprOperator::LogicalOr as usize], 
			OpInfo { arity: OpArity::Binary, rank: 0, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ExprOperator::LogicalAnd as usize], 
			OpInfo { arity: OpArity::Binary, rank: 1, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ExprOperator::LogicalXor as usize], 
			OpInfo { arity: OpArity::Binary, rank: 2, assot: OpAssot::Left });
			
		assert_eq!(
			OP_ATTRS[ExprOperator::Equal as usize], 
			OpInfo { arity: OpArity::Binary, rank: 3, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ExprOperator::NotEqual as usize], 
			OpInfo { arity: OpArity::Binary, rank: 3, assot: OpAssot::Left });
			
		assert_eq!(
			OP_ATTRS[ExprOperator::Less as usize], 
			OpInfo { arity: OpArity::Binary, rank: 4, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ExprOperator::LessEqual as usize], 
			OpInfo { arity: OpArity::Binary, rank: 4, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ExprOperator::Greater as usize], 
			OpInfo { arity: OpArity::Binary, rank: 4, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ExprOperator::GreaterEqual as usize], 
			OpInfo { arity: OpArity::Binary, rank: 4, assot: OpAssot::Left });
			
		assert_eq!(
			OP_ATTRS[ExprOperator::BinPlus as usize], 
			OpInfo { arity: OpArity::Binary, rank: 5, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ExprOperator::BinMinus as usize], 
			OpInfo { arity: OpArity::Binary, rank: 5, assot: OpAssot::Left });
			
		assert_eq!(
			OP_ATTRS[ExprOperator::Div as usize], 
			OpInfo { arity: OpArity::Binary, rank: 6, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ExprOperator::Mul as usize], 
			OpInfo { arity: OpArity::Binary, rank: 6, assot: OpAssot::Left });
			
		assert_eq!(
			OP_ATTRS[ExprOperator::UnPlus as usize], 
			OpInfo { arity: OpArity::Unary, rank: 7, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ExprOperator::UnMinus as usize], 
			OpInfo { arity: OpArity::Unary, rank: 7, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ExprOperator::Not as usize], 
			OpInfo { arity: OpArity::Unary, rank: 7, assot: OpAssot::Left });
			
		assert_eq!(
			OP_ATTRS[ExprOperator::Pow as usize], 
			OpInfo { arity: OpArity::Binary, rank: 8, assot: OpAssot::Right });
			
		assert_eq!(
			OP_ATTRS[ExprOperator::DotMemberAccess as usize], 
			OpInfo { arity: OpArity::Binary, rank: 9, assot: OpAssot::Left });
	}
}