// TODO: rename module to 'func'

use super::var_data::{DataType, Value};
use super::InterpErr;

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
	
	pub fn find<'mem>(&'mem self, name: &str) -> Result<&'mem BuiltinFuncDef, BuiltinFuncErr> {
		match self.funcs.iter().find(|func| func.get_name() == name) {
			Some(func) => Ok(func),
			None => Err( BuiltinFuncErr::NotDefined ),
		}
	}
}

//------------------------- BuiltinFuncDef -----------------------

pub type BuiltinFuncBody = Box<dyn Fn(Vec<Value>) -> Result<(), InterpErr>>;

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
	
	pub fn check_args(&self, args_data_types: Vec<DataType>) -> Result<(), BuiltinFuncErr> {
		if self.args.len() != args_data_types.len() {
			return Err( BuiltinFuncErr::ArgsCnt { 
				actual_cnt: self.args.len(),
				given_cnt: args_data_types.len(),
			} );
		}
		
		for i in 0..self.args.len() {
			if !self.args[i].type_check(args_data_types[i]) {
				return Err( BuiltinFuncErr::ArgType { 
					actual_type: self.args[i].data_type,
					given_type: args_data_types[i],
				} );
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
	
	pub fn call(&self, args_values: Vec<Value>) -> Result<(), InterpErr> {
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
		actual_cnt: usize,
		given_cnt: usize,
	},
	ArgType { // TODO: provide token to keep wrong argument position
		actual_type: DataType,
		given_type: DataType,
	},
	NotDefined,
}