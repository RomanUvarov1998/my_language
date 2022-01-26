// TODO: rename module to 'func'

use super::var_data::{DataType, Value};
use super::InterpErr;
use super::utils::{CodePos, NameToken};
use super::expr::Expr;
use super::memory::Memory;

//------------------------- BuiltinFuncsDefList -----------------------

pub struct BuiltinFuncsDefList {
	funcs: Vec<BuiltinFuncDef>,
}

impl BuiltinFuncsDefList {
	pub fn new() -> Self {
		Self {
			funcs: Vec::new(),
		}
	}
	
	pub fn add(&mut self, func_def: BuiltinFuncDef) {
		let func_name: &str = func_def.get_name();
		if let Some(..) =  self.funcs
			.iter()
			.find(|func| func.get_name() == func_name) {
				panic!("Builtin function '{}' is already defined", func_name);
		}
		
		self.funcs.push(func_def);
	}
	
	pub fn find<'mem>(&'mem self, name: &NameToken) -> Result<&'mem BuiltinFuncDef, BuiltinFuncErr> {
		match self.funcs.iter().find(|func| func.get_name() == name.value()) {
			Some(func) => Ok(func),
			None => Err( BuiltinFuncErr::NotDefined { name_pos: name.pos() } ),
		}
	}
}

//------------------------- BuiltinFuncDef -----------------------

pub type BuiltinFuncBody = Box<dyn Fn(Vec<Value>) -> Result<Option<Value>, InterpErr>>;

pub struct BuiltinFuncDef {
	name: &'static str,
	args: Vec<BuiltinFuncArg>,
	body: BuiltinFuncBody,
	return_type: DataType,
}

impl BuiltinFuncDef {
	pub fn new(name: &'static str, args: Vec<BuiltinFuncArg>, body: BuiltinFuncBody, return_type: DataType) -> Self {
		Self {
			name,
			args,
			body,
			return_type,
		}
	}
	
	pub fn check_args(&self, func_name: &NameToken, args_exprs: &Vec<Expr>, check_memory: &Memory, builtin_func_defs: &BuiltinFuncsDefList) -> Result<(), InterpErr> {
		if self.args.len() != args_exprs.len() {
			return Err( InterpErr::from( BuiltinFuncErr::ArgsCnt { 
				func_name_pos: func_name.pos(),
				actual_cnt: self.args.len(),
				given_cnt: args_exprs.len(),
			} ) );
		}
						
		let args_data_types_result: Result<Vec<DataType>, InterpErr> = args_exprs.iter()
			.map(|expr| expr.check_and_calc_data_type(check_memory, builtin_func_defs))
			.collect();
			
		let args_data_types: Vec<DataType> = args_data_types_result?;
		
		for i in 0..self.args.len() {
			if !self.args[i].type_check(args_data_types[i]) {
				return Err( InterpErr::from( BuiltinFuncErr::ArgType {
					arg_expr_pos: args_exprs[i].pos(),
					actual_type: self.args[i].data_type,
					given_type: args_data_types[i],
				} ) );
			}
		}
		
		Ok(())
	}
	
	pub fn get_name(&self) -> &'static str {
		self.name
	}
	
	pub fn return_type(&self) -> DataType {
		self.return_type
	}
	
	pub fn call(&self, args_values: Vec<Value>) -> Result<Option<Value>, InterpErr> {
		(self.body)(args_values)
	}
}

pub struct BuiltinFuncArg {
	#[allow(dead_code)]
	name: String, // TODO: use token for func arg if function is user defined
	data_type: DataType,
}

impl BuiltinFuncArg {
	pub fn new(name: String, data_type: DataType) -> Self {
		Self {
			name,
			data_type,
		}
	}
	
	pub fn type_check(&self, data_type: DataType) -> bool {
		match self.data_type {
			DataType::Untyped => true,
			_ => self.data_type == data_type,
		}
	}
}

//------------------------- BuiltinFuncErr -----------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuiltinFuncErr {
	ArgsCnt {
		func_name_pos: CodePos,
		actual_cnt: usize,
		given_cnt: usize,
	},
	ArgType {
		arg_expr_pos: CodePos,
		actual_type: DataType,
		given_type: DataType,
	},
	NotDefined {
		name_pos: CodePos,
	},
}

impl std::fmt::Display for BuiltinFuncErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			BuiltinFuncErr::ArgsCnt { actual_cnt, given_cnt, .. } =>
				write!(f, "Wrong arguments count for builtin function: {} expected but {} found", actual_cnt, given_cnt),
			BuiltinFuncErr::ArgType { actual_type, given_type, .. } =>
				write!(f, "Wrong argument type for builtin function: {} expected but {} found", actual_type, given_type),
			BuiltinFuncErr::NotDefined { .. } =>
				write!(f, "Builtin function is not defined"),
		}
	}
}
