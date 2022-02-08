use super::super::InterpErr;
use super::super::value::Value;
use super::super::data_type::{DataType, Primitive};
use super::super::builtin_func::BuiltinFuncDef;
use super::super::user_func::{UserFuncDef, UserFuncArg};
use super::super::utils::CodePos;
use super::super::context::Context;
use super::super::struct_def::StructFieldDef;
use super::super::utils::NameToken;
use super::symbol::{Symbol, SymbolKind, Operand};
use super::{Expr, ExprErr};
use std::cell::RefCell;
use std::rc::Rc;

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
	Index,
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

pub static OP_ATTRS: [OpInfo; 19] = [
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
	
	OpInfo { arity: OpArity::Unary, rank: 7, assot: OpAssot::Right },		//UnPlus
	OpInfo { arity: OpArity::Unary, rank: 7, assot: OpAssot::Right },		//UnMinus
	OpInfo { arity: OpArity::Unary, rank: 7, assot: OpAssot::Right },		//Not
	
	OpInfo { arity: OpArity::Binary, rank: 8, assot: OpAssot::Right }, 	//Pow
	
	OpInfo { arity: OpArity::Binary, rank: 9, assot: OpAssot::Left }, 	//Dot
	OpInfo { arity: OpArity::Binary, rank: 9, assot: OpAssot::Left }, 	//Index
];

impl ExprOperator {	
	pub fn rank(&self) -> u32 {
		OP_ATTRS[*self as usize].rank
	}
	
	pub fn assot(&self) -> OpAssot {
		OP_ATTRS[*self as usize].assot
	}
	
	
	pub fn get_result_data_type(self, calc_stack: &mut Vec<Symbol>, check_context: &Context, operator_pos: CodePos) -> Result<DataType, InterpErr> {
		use ExprOperator::*;
		
		if let DotMemberAccess = self {
			let rhs: Symbol = calc_stack.pop()
				.ok_or(InterpErr::from(ExprErr::not_enough_operands_for_operator(operator_pos, 0, 2)))?;
				
			let rhs = if let Symbol { kind: SymbolKind::Operand (opnd), .. } = rhs {
				opnd
			} else { unreachable!() };
			
			let lhs: Symbol = calc_stack.pop()
				.ok_or(InterpErr::from(ExprErr::not_enough_operands_for_operator(operator_pos, 1, 2)))?;
				
			let (lhs, lhs_pos) = if let Symbol { kind: SymbolKind::Operand (opnd), pos } = lhs {
				(opnd, pos)
			} else { unreachable!() };
				
			match (&lhs, &rhs) {
				(_, Operand::Value (_)) =>
					Err(ExprErr::wrong_operands_for_operator(self, operator_pos, &[&lhs, &rhs]).into()),
					
				(_, Operand::StructLiteral { .. }) =>
					Err(ExprErr::wrong_operands_for_operator(self, operator_pos, &[&lhs, &rhs]).into()),
					
				(_, Operand::ValueRef (_)) =>
					unreachable!(),
				
				(_, Operand::IndexExpr (_)) => 
					Err(ExprErr::wrong_operands_for_operator(self, operator_pos, &[&lhs, &rhs]).into()),
				
				(Operand::IndexExpr (_), _) => 
					Err(ExprErr::wrong_operands_for_operator(self, operator_pos, &[&lhs, &rhs]).into()),
				
				
				(Operand::Value (ref value), Operand::Variable (ref var_name)) => {
					let value_type: DataType = value.get_type();
					
					if let DataType::Complex (ref struct_def) = value_type {
						let sd_inner = struct_def.inner();
						let field_def: &StructFieldDef = sd_inner.member_field(var_name)?;
						Ok(field_def.data_type().clone())
					} else {
						Err( ExprErr::NotStruct (lhs_pos).into() )
					}
				},
				
				(Operand::Value (ref value), Operand::FuncCall {
					ref func_name,
					ref arg_exprs,
				}) => {
					let value_type: DataType = value.get_type();
					
					if func_name.is_builtin() {
						let func_def: &BuiltinFuncDef = value_type
							.find_member_builtin_func(&func_name, check_context)?;
						
						func_def.check_args_as_member_function(func_name, arg_exprs, &value_type, lhs_pos, check_context)?;
						
						return Ok(func_def.return_type().clone());
					} else {
						let sd = value_type.struct_def(lhs_pos)?;
						let sd_inner = sd.inner();
						let func_def: &UserFuncDef = sd_inner.find_user_func_def(func_name)?;
						
						func_def.check_args_as_member_function(func_name, arg_exprs, &value_type, lhs_pos, check_context)?;
						
						return Ok(func_def.return_type().clone());
					}
				},
				
								
				(Operand::Variable (ref var_name_1), Operand::Variable (ref var_name_2)) => {
					let value_type: &DataType = check_context.get_variable_def(&var_name_1)?.get_type();
					
					if let DataType::Complex (struct_def) = value_type {
						let sd_inner = struct_def.inner();
						let field_def: &StructFieldDef = sd_inner.member_field(var_name_2)?;
						Ok(field_def.data_type().clone())
					} else {
						Err( ExprErr::NotStruct (var_name_1.pos()).into() )
					}
				},
				
				(Operand::Variable (ref var_name), Operand::FuncCall {
					ref func_name,
					ref arg_exprs,
				}) => {
					let value: &Value = check_context.get_variable_value(&var_name)?;
					let value_type: DataType = value.get_type();
					
					if func_name.is_builtin() {
						let func_def: &BuiltinFuncDef = value_type
							.find_member_builtin_func(&func_name, check_context)?;
						
						func_def.check_args_as_member_function(func_name, arg_exprs, &value_type, lhs_pos, check_context)?;
						
						return Ok(func_def.return_type().clone());
					} else {
						let sd = value_type.struct_def(var_name.pos())?;
						let sd_inner = sd.inner();
						let func_def: &UserFuncDef = sd_inner.find_user_func_def(func_name)?;
						
						func_def.check_args_as_member_function(func_name, arg_exprs, &value_type, lhs_pos, check_context)?;
						
						return Ok(func_def.return_type().clone());
					}
				},
				
				
				(ref fc @ Operand::FuncCall { .. }, Operand::Variable (ref var_name)) => {
					let return_type: DataType = fc.check_and_calc_data_type_in_place(check_context)?;
					
					if let DataType::Complex (struct_def) = return_type {
						let sd_inner = struct_def.inner();
						let field_def: &StructFieldDef = sd_inner.member_field(var_name)?;
						Ok(field_def.data_type().clone())
					} else {
						Err( ExprErr::NotStruct (lhs_pos).into() )
					}
				},
				
				(ref fc1 @ Operand::FuncCall { .. }, Operand::FuncCall { ref func_name, ref arg_exprs }) => {
					let return_type_1: DataType = fc1.check_and_calc_data_type_in_place(check_context)?;
					
					if func_name.is_builtin() {
						let func_def: &BuiltinFuncDef = return_type_1
							.find_member_builtin_func(&func_name, check_context)?;
						
						func_def.check_args_as_member_function(func_name, arg_exprs, &return_type_1, lhs_pos, check_context)?;
						
						return Ok(func_def.return_type().clone());
					} else {
						let sd = return_type_1.struct_def(lhs_pos)?;
						let sd_inner = sd.inner();
						let func_def: &UserFuncDef = sd_inner.find_user_func_def(func_name)?;
						
						func_def.check_args_as_member_function(func_name, arg_exprs, &return_type_1, lhs_pos, check_context)?;
						
						return Ok(func_def.return_type().clone());
					}
				},
				
				
				(sl @ Operand::StructLiteral { .. }, Operand::Variable (ref field_name)) => {
					let struct_type: DataType = sl.check_and_calc_data_type_in_place(check_context)?;
					
					if let DataType::Complex (ref struct_def) = struct_type {
						let sd_inner = struct_def.inner();
						let field_def: &StructFieldDef = sd_inner.member_field(field_name)?;
						Ok(field_def.data_type().clone())
					} else { unreachable!() }
				},
				
				(sl @ Operand::StructLiteral { .. }, Operand::FuncCall { ref func_name, ref arg_exprs }) => {
					let struct_type: DataType = sl.check_and_calc_data_type_in_place(check_context)?;
					
					if let DataType::Complex (ref struct_def) = struct_type {
						let sd_inner = struct_def.inner();
						
						if func_name.is_builtin() {
							let func_def: &BuiltinFuncDef = sd_inner.find_builtin_func_def(func_name)?;
							func_def.check_args_as_member_function(func_name, arg_exprs, &struct_type, lhs_pos, check_context)?;
							Ok(func_def.return_type().clone())
						} else {
							let func_def: &BuiltinFuncDef = sd_inner.find_builtin_func_def(func_name)?;
							func_def.check_args_as_member_function(func_name, arg_exprs, &struct_type, lhs_pos, check_context)?;
							Ok(func_def.return_type().clone())
						}
					} else { unreachable!() }
				},
				
				
				(Operand::ValueRef (ref value_rc), Operand::Variable (ref field_name)) => {
					let value_type: DataType = value_rc.borrow().get_type();
					
					if let DataType::Complex (struct_def) = value_type {
						let sd_inner = struct_def.inner();
						let field_def: &StructFieldDef = sd_inner.member_field(field_name)?;
						Ok(field_def.data_type().clone())
					} else {
						Err( ExprErr::NotStruct (lhs_pos).into() )
					}
				},
				
				(Operand::ValueRef (ref value_rc), Operand::FuncCall { ref func_name, ref arg_exprs }) => {
					let value_type: DataType = value_rc.borrow().get_type();
					
					if let DataType::Complex (ref struct_def) = value_type {
						let sd_inner = struct_def.inner();
						
						if func_name.is_builtin() {
							let func_def: &BuiltinFuncDef = sd_inner.find_builtin_func_def(func_name)?;
							func_def.check_args_as_member_function(func_name, arg_exprs, &value_type, lhs_pos, check_context)?;
							Ok(func_def.return_type().clone())
						} else {
							let func_def: &BuiltinFuncDef = sd_inner.find_builtin_func_def(func_name)?;
							func_def.check_args_as_member_function(func_name, arg_exprs, &value_type, lhs_pos, check_context)?;
							Ok(func_def.return_type().clone())
						}
					} else {
						Err( ExprErr::NotStruct (lhs_pos).into() )
					}
				},
			}
		} else {
			match OP_ATTRS[self as usize].arity {
				OpArity::Binary => {
					let rhs: Operand = calc_stack.pop()
						.ok_or(InterpErr::from(ExprErr::not_enough_operands_for_operator(operator_pos, 0, 2)))?
						.unwrap_operand();
						
					let lhs: Operand = calc_stack.pop()
						.ok_or(InterpErr::from(ExprErr::not_enough_operands_for_operator(operator_pos, 1, 2)))?
						.unwrap_operand();
					
					let lhs: DataType = lhs.check_and_calc_data_type_in_place(check_context)?;
					let rhs: DataType = rhs.check_and_calc_data_type_in_place(check_context)?;
					
					let result = match self {
						BinPlus => self.get_bin_plus_result_type(&lhs, &rhs),
						BinMinus => self.get_bin_minus_result_type(&lhs, &rhs),
						Div => self.get_bin_div_result_type(&lhs, &rhs),
						Mul => self.get_bin_mul_result_type(&lhs, &rhs),
						Pow => self.get_bin_pow_result_type(&lhs, &rhs),
						Equal => self.get_equal_result_type(&lhs, &rhs),
						NotEqual => self.get_not_equal_result_type(&lhs, &rhs),
						Greater => self.get_greater_result_type(&lhs, &rhs),
						GreaterEqual => self.get_greater_equal_result_type(&lhs, &rhs),
						Less => self.get_less_result_type(&lhs, &rhs),
						LessEqual => self.get_less_equal_result_type(&lhs, &rhs),
						LogicalAnd => self.get_logical_and_result_type(&lhs, &rhs),
						LogicalOr => self.get_logical_or_result_type(&lhs, &rhs),
						LogicalXor => self.get_logical_xor_result_type(&lhs, &rhs),
						DotMemberAccess => unreachable!(),
						Index => self.get_index_result_type(&lhs, &rhs),
						
						UnPlus => unreachable!(),
						UnMinus => unreachable!(),
						Not => unreachable!(),
					};
					
					match result {
						Ok(dt) => Ok(dt),
						Err(()) => Err(ExprErr::wrong_operands_type_for_operator(
							self, operator_pos, &[&lhs, &rhs]).into()),
					}
				},
				OpArity::Unary => {
					let op: DataType = calc_stack
						.pop()
						.ok_or(InterpErr::from(ExprErr::not_enough_operands_for_operator(operator_pos, 0, 1)))?
						.unwrap_operand()
						.check_and_calc_data_type_in_place(check_context)?;
					
					let result = match self {
						BinPlus => unreachable!(),
						BinMinus => unreachable!(),
						Div => unreachable!(),
						Mul => unreachable!(),
						Pow => unreachable!(),
						Equal => unreachable!(),
						NotEqual => unreachable!(),
						Greater => unreachable!(),
						GreaterEqual => unreachable!(),
						Less => unreachable!(),
						LessEqual => unreachable!(),
						LogicalAnd => unreachable!(),
						LogicalOr => unreachable!(),
						LogicalXor => unreachable!(),
						DotMemberAccess => unreachable!(),
						Index => unreachable!(),
						
						UnPlus => self.get_unary_plus_result_type(&op),
						UnMinus => self.get_unary_minus_result_type(&op),
						Not => self.get_not_result_type(&op),
					};
					
					match result {
						Ok(dt) => Ok(dt),
						Err(()) => Err(ExprErr::wrong_operands_type_for_operator(
							self, operator_pos, &[&op]).into()),
					}
				},
			}
		}
	}
	
	pub fn apply(&self, calc_stack: &mut Vec<Symbol>, context: &Context, optr_pos: CodePos) -> Symbol {	
		use ExprOperator::*;
		
		if let DotMemberAccess = self {
			let rhs: Symbol = calc_stack
				.pop()
				.unwrap();
				
			let (rhs_pos, rhs) = if let Symbol { pos, kind: SymbolKind::Operand (opnd) } = rhs {
				(pos, opnd)
			} else { unreachable!() };
			
			let lhs: Symbol = calc_stack
				.pop()
				.unwrap();
				
			let (lhs, lhs_pos) = if let Symbol { kind: SymbolKind::Operand (opnd), pos } = lhs {
				(opnd, pos)
			} else { unreachable!() };
			
			match (&lhs, &rhs) {
				(_, Operand::Value (_)) =>
					unreachable!(),
					
				(_, Operand::StructLiteral { .. }) =>
					unreachable!(),
					
				(_, Operand::ValueRef (_)) =>
					unreachable!(),
				
				(_, Operand::IndexExpr (_)) => 
					unreachable!(),
				
				(Operand::IndexExpr (_), _) => 
					unreachable!(),
				
				
				(Operand::Value (ref value), Operand::Variable (ref var_name)) => {
					if let Value::Struct { fields, .. } = value {
						let value_rc: Rc<RefCell<Value>> = Rc::clone(&fields.get(var_name.value()).unwrap());
						let opnd: Operand = Operand::ValueRef(value_rc);
						Symbol {
							pos: rhs_pos,
							kind: SymbolKind::Operand (opnd),
						}
					} else { unreachable!() }
				},
				
				(Operand::Value (ref value), Operand::FuncCall {
					ref func_name,
					ref arg_exprs,
				}) => Self::call_member_func_of_value(value, func_name, arg_exprs, context, lhs_pos, rhs_pos),
				
				
				(Operand::Variable (ref var_name_1), Operand::Variable (ref var_name_2)) => {
					let value: &Value = context.get_variable_value(&var_name_1).unwrap();
					
					if let Value::Struct { fields, .. } = value {
						let value_rc: Rc<RefCell<Value>> = Rc::clone(&fields.get(var_name_2.value()).unwrap());
						let opnd: Operand = Operand::ValueRef (value_rc);
						Symbol {
							pos: rhs_pos,
							kind: SymbolKind::Operand (opnd),
						}
					} else {
						unreachable!();
					}
				},
				
				(Operand::Variable (ref var_name), Operand::FuncCall {
					ref func_name,
					ref arg_exprs,
				}) => {
					let value: &Value = context.get_variable_value(var_name).unwrap();
					
					Self::call_member_func_of_value(value, func_name, arg_exprs, context, lhs_pos, rhs_pos)
				},
				
				
				(ref fc @ Operand::FuncCall { .. }, Operand::Variable (ref var_name)) => {
					let result: Value = fc.calc_in_place(context).unwrap();
					
					if let Value::Struct { fields, .. } = result {
						let value_rc: Rc<RefCell<Value>> = Rc::clone(&fields.get(var_name.value()).unwrap());
						let opnd: Operand = Operand::ValueRef (value_rc);
						Symbol {
							pos: rhs_pos,
							kind: SymbolKind::Operand (opnd),
						}
					} else {
						unreachable!();
					}
				},
				
				(ref fc1 @ Operand::FuncCall { .. }, Operand::FuncCall { ref func_name, ref arg_exprs }) => {
					let result: Value = fc1.calc_in_place(context).unwrap();					
					Self::call_member_func_of_value(&result, func_name, arg_exprs, context, lhs_pos, rhs_pos)
				},
				
				
				(sl @ Operand::StructLiteral { .. }, Operand::Variable (ref field_name)) => {
					let struct_value: Value = sl.calc_in_place(context).unwrap();
					
					if let Value::Struct { fields, .. } = struct_value {
						let value_rc: Rc<RefCell<Value>> = Rc::clone(&fields.get(field_name.value()).unwrap());
						let opnd: Operand = Operand::ValueRef(value_rc);
						Symbol {
							pos: rhs_pos,
							kind: SymbolKind::Operand (opnd),
						}
					} else { unreachable!() }
				},
				
				(sl @ Operand::StructLiteral { .. }, Operand::FuncCall { ref func_name, ref arg_exprs }) => {
					let struct_value: Value = sl.calc_in_place(context).unwrap();
					Self::call_member_func_of_value(&struct_value, func_name, arg_exprs, context, lhs_pos, rhs_pos)
				},
				
				
				(Operand::ValueRef (ref value_rc), Operand::Variable (ref field_name)) => {
					if let Value::Struct { ref fields, .. } = *value_rc.borrow() {
						let value_rc: Rc<RefCell<Value>> = Rc::clone(&fields.get(field_name.value()).unwrap());
						let opnd: Operand = Operand::ValueRef (value_rc);
						Symbol {
							pos: rhs_pos,
							kind: SymbolKind::Operand (opnd),
						}
					} else {
						unreachable!();
					}
				},
				
				(Operand::ValueRef (ref value_rc), Operand::FuncCall { ref func_name, ref arg_exprs }) =>
					Self::call_member_func_of_value(&value_rc.borrow(), func_name, arg_exprs, context, lhs_pos, rhs_pos),
			}
		} else {
			let value: Value = match self {
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
				DotMemberAccess => unreachable!(),
				Index => self.apply_index(calc_stack, context),
			};
			let opnd: Operand = Operand::Value (value);
			Symbol {
				pos: optr_pos,
				kind: SymbolKind::Operand (opnd),
			}
		}
	}
	
	
	fn call_member_func_of_value(value: &Value, func_name: &NameToken, arg_exprs: &Vec<Expr>, context: &Context, lhs_pos: CodePos, rhs_pos: CodePos) -> Symbol {
		if func_name.is_builtin() {
			let func_def: &BuiltinFuncDef = value.get_type()
				.find_member_builtin_func(&func_name, context).unwrap();
			
			let mut args_values = Vec::<Value>::with_capacity(arg_exprs.len() + 1);
			args_values.push(value.clone());
			for expr in arg_exprs {
				let value: Value = expr.calc(context);
				args_values.push(value);
			}
			
			let value: Value = func_def.call(args_values).unwrap();
			let opnd: Operand = Operand::Value(value);
			
			Symbol {
				pos: rhs_pos,
				kind: SymbolKind::Operand (opnd),
			}
		} else {
			let value_type: DataType = value.get_type();
			let sd = value_type.struct_def(lhs_pos).unwrap();
			let sd_inner = sd.inner();
			let func_def: &UserFuncDef = sd_inner.find_user_func_def(func_name).unwrap();
			
			let mut args_values = Vec::<Value>::with_capacity(arg_exprs.len() + 1);
			args_values.push(value.clone());
			for expr in arg_exprs {
				let value: Value = expr.calc(context);
				args_values.push(value);
			}
			
			let mut next_context = context.new_stack_frame_context();
			
			let func_args: &Vec<UserFuncArg> = func_def.args();
			for i in 0..args_values.len() {
				next_context.add_variable(
					func_args[i].name().clone(),
					func_args[i].data_type().clone(),
					args_values[i].clone()).unwrap();
			}
			
			let value: Value = func_def.call(&mut next_context).unwrap();
			let opnd: Operand = Operand::Value(value);
			
			Symbol {
				pos: rhs_pos,
				kind: SymbolKind::Operand (opnd),
			}
		}
	}
	
	fn apply_bin_plus(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Float32(val1 + val2),
			(Value::String (mut s1), Value::String (mut s2)) => {
				s1.append(&mut s2);
				Value::String(s1)
			},
			(Value::String (mut s), Value::Char (c)) => {
				s.push(c);
				Value::String(s)
			},
			(Value::Char (c), Value::String (mut s)) => {
				s.insert(0, c);
				Value::String(s)
			},
			(Value::Char (c1), Value::Char (c2)) => {
				let chars: Vec<char> = vec![c1, c2];
				Value::String(chars)
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
	
	fn apply_index(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::String (string), Value::Float32 (value)) => {
				if value.is_nan() {
					println!("\n\nIndex can't be NAN");
					std::process::exit(1);
				}
				
				let index: usize = (value.abs().floor() * value.signum()) as usize;
				
				Value::Char(string[index])
			},
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}
	
	fn take_2_values(calc_stack: &mut Vec<Symbol>, context: &Context) -> (Value, Value) {
		let rhs: Value = calc_stack
			.pop()
			.unwrap()
			.unwrap_operand()
			.calc_in_place(context)
			.unwrap();
			
		let lhs: Value = calc_stack
			.pop()
			.unwrap()
			.unwrap_operand()
			.calc_in_place(context)
			.unwrap();
		
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
			.calc_in_place(context)
			.unwrap();
		
		op
	}
	
	//---------------- Result data type -----------------------
	
	fn get_bin_plus_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => 
				Ok(DataType::Primitive (Primitive::Float32)),
				
			(DataType::Primitive (Primitive::String), DataType::Primitive (Primitive::String)) => 
				Ok(DataType::Primitive (Primitive::String)),
				
			(DataType::Primitive (Primitive::Char), DataType::Primitive (Primitive::String)) => 
				Ok(DataType::Primitive (Primitive::String)),
				
			(DataType::Primitive (Primitive::String), DataType::Primitive (Primitive::Char)) => 
				Ok(DataType::Primitive (Primitive::String)),
				
			(DataType::Primitive (Primitive::Char), DataType::Primitive (Primitive::Char)) => 
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

	fn get_index_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::String), DataType::Primitive (Primitive::Float32)) => Ok(DataType::Primitive (Primitive::Char)),
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
			OpInfo { arity: OpArity::Unary, rank: 7, assot: OpAssot::Right });
		assert_eq!(
			OP_ATTRS[ExprOperator::UnMinus as usize], 
			OpInfo { arity: OpArity::Unary, rank: 7, assot: OpAssot::Right });
		assert_eq!(
			OP_ATTRS[ExprOperator::Not as usize], 
			OpInfo { arity: OpArity::Unary, rank: 7, assot: OpAssot::Right });
			
		assert_eq!(
			OP_ATTRS[ExprOperator::Pow as usize], 
			OpInfo { arity: OpArity::Binary, rank: 8, assot: OpAssot::Right });
			
		assert_eq!(
			OP_ATTRS[ExprOperator::DotMemberAccess as usize], 
			OpInfo { arity: OpArity::Binary, rank: 9, assot: OpAssot::Left });			
		assert_eq!(
			OP_ATTRS[ExprOperator::Index as usize], 
			OpInfo { arity: OpArity::Binary, rank: 9, assot: OpAssot::Left });
	}
}