use super::builtin_func::{BuiltinFuncsDefList, BuiltinFuncDef, BuiltinFuncArg};
use super::user_func::{UserFuncDef, UserFuncArg};
use super::data_type::DataType;
use super::value::Value;
use super::InterpErr;
use super::utils::{CodePos, NameToken};
use super::expr::Expr;
use super::memory::Memory;

//---------------------- MemberFuncDef -------------------

pub enum MemberFuncDef {
	Builtin (BuiltinFuncDef),
	UserDefined (UserFuncDef),
}

impl MemberFuncDef {
	pub fn new_builtin(value_type: DataType, func_def: BuiltinFuncDef) -> Self {
		let args: &Vec<BuiltinFuncArg> = func_def.args();
		assert!(args.len() > 0);
		assert!(args[0].data_type() == value_type);
		assert!(args[0].data_type() == DataType::Primitive (Primitive::Any));
		Self::Builtin (func_def)
	}
	
	pub fn new_user_defined(value_type: DataType, func_def: UserFuncDef) -> Self {
		let args: &Vec<UserFuncArg> = func_def.args();
		assert!(args.len() > 0);
		assert!(args[0].data_type() == value_type);
		assert!(args[0].data_type() == DataType::Primitive (Primitive::Any));
		Self::UserDefined (func_def)
	}
	
	pub fn name(&self) -> &str {
		match self {
			MemberFuncDef::Builtin (fd) => fd.name(),
			MemberFuncDef::UserDefined (fd) => fd.name().value(),
		}
	}
	
	pub fn return_type(&self) -> DataType {
		match self {
			MemberFuncDef::Builtin (fd) => fd.return_type(),
			MemberFuncDef::UserDefined (fd) => fd.return_type(),
		}
	}
	
	pub fn check_args(&self, args_exprs: &Vec<Expr>, check_memory: &Memory, builtin_func_defs: &BuiltinFuncsDefList) -> Result<(), InterpErr> {
		match self {
			MemberFuncDef::Builtin (fd) => 
				fd.check_args(args_exprs, check_memory, builtin_func_defs),
			MemberFuncDef::UserDefined (fd) => 
				fd.check_args(args_exprs, check_memory, builtin_func_defs),
		}
	}
	
	pub fn call(&self, value: &mut Value, args_vals: Vec<Value>, memory: &mut Memory, builtin_func_defs: &BuiltinFuncsDefList) -> Option<Value> {
		match self {
			MemberFuncDef::Builtin (fd) => fd.call(args_vals),
			MemberFuncDef::UserDefined (fd) => 
				fd.call(memory, builtin_func_defs),
		}
	}
}