use super::var_data::{DataType, Value};
use super::InterpErr;
use super::utils::{CodePos, NameToken};
use super::expr::Expr;
use super::memory::Memory;
use super::statement::ReturningBody;

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

pub type BuiltinFuncBody = Box<dyn Fn(Vec<Value>) -> Option<Value>>;

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
				func_signature: format!("{}", self),
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
					func_signature: format!("{}", self),
					arg_name: self.args[i].name.clone(),
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
	
	pub fn call(&self, args_values: Vec<Value>) -> Option<Value> {
		(self.body)(args_values)
	}
}

impl std::fmt::Display for BuiltinFuncDef {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "f {}(", self.name)?;
		let mut need_comma_before = false;
		for arg in self.args.iter() {
			if need_comma_before {
				write!(f, ", ")?; 
			}
			write!(f, "{}: {}", arg.name, arg.data_type)?; 
			if !need_comma_before {
				need_comma_before = true;
			}
		}
		write!(f, ") -> {}", self.return_type)
	}
}

pub struct BuiltinFuncArg {
	name: String,
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
		func_signature: String,
		func_name_pos: CodePos,
		actual_cnt: usize,
		given_cnt: usize,
	},
	ArgType {
		func_signature: String,
		arg_name: String,
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
			BuiltinFuncErr::ArgsCnt { func_signature, actual_cnt, given_cnt, .. } =>
				write!(f, "Wrong arguments count for builtin function:\n{}\n{} expected but {} found", func_signature, actual_cnt, given_cnt),
			BuiltinFuncErr::ArgType { func_signature, arg_name, actual_type, given_type, .. } =>
				write!(f, "Wrong type of argument '{}' for builtin function:\n{}\n'{}' expected but '{}' found", arg_name, func_signature, actual_type, given_type),
			BuiltinFuncErr::NotDefined { .. } =>
				write!(f, "Builtin function is not defined"),
		}
	}
}

//------------------------- UserFuncDef -----------------------

#[derive(Debug, Clone)]
pub struct UserFuncDef {
	name: NameToken,
	args: Vec<UserFuncArg>,
	return_type: DataType, // TODO: make function be able to not to return anything by definition and use 'return;' statement
	body: ReturningBody,
}

impl UserFuncDef {
	pub fn new(name: NameToken, args: Vec<UserFuncArg>, return_type: DataType, body: ReturningBody) -> Self {
		Self {
			name,
			args,
			return_type,
			body,
		}
	}
	
	pub fn check_args(&self, args_exprs: &Vec<Expr>, check_memory: &Memory, builtin_func_defs: &BuiltinFuncsDefList) -> Result<(), InterpErr> {
		if self.args.len() != args_exprs.len() {
			return Err( InterpErr::from( UserFuncErr::ArgsCnt {
				func_signature: format!("{}", self),
				func_name_pos: self.name.pos(),
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
				return Err( InterpErr::from( UserFuncErr::ArgType {
					func_signature: format!("{}", self),
					arg_name: self.args[i].name.clone(),
					arg_expr_pos: args_exprs[i].pos(),
					actual_type: self.args[i].data_type,
					given_type: args_data_types[i],
				} ) );
			}
		}
		
		Ok(())
	}
	
	pub fn call(&self, memory: &mut Memory, builtin_func_defs: &BuiltinFuncsDefList) -> Value {
		self.body.run(memory, builtin_func_defs)
	}
	
	pub fn args(&self) -> &Vec<UserFuncArg> {
		&self.args
	}
	
	pub fn get_name(&self) -> &NameToken {
		&self.name
	}

	pub fn return_type(&self) -> DataType {
		self.return_type
	}
}

impl std::fmt::Display for UserFuncDef {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "f {}(", self.name)?;
		let mut need_comma_before = false;
		for arg in self.args.iter() {
			if need_comma_before {
				write!(f, ", ")?; 
			}
			write!(f, "{}: {}", arg.name, arg.data_type)?; 
			if !need_comma_before {
				need_comma_before = true;
			}
		}
		write!(f, ") -> {}", self.return_type)
	}
}

//------------------------- UserFuncArg -----------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UserFuncArg {
	name: NameToken,
	data_type: DataType,
}

impl UserFuncArg {
	pub fn new(name: NameToken, data_type: DataType) -> Self {
		Self {
			name,
			data_type,
		}
	}
	
	pub fn name(&self) -> &NameToken {
		&self.name
	}
	
	pub fn data_type(&self) -> DataType {
		self.data_type
	}

	pub fn type_check(&self, data_type: DataType) -> bool {
		match self.data_type {
			DataType::Untyped => true,
			_ => self.data_type == data_type,
		}
	}
}

//------------------------- UserFuncErr -----------------------

#[derive(Debug, PartialEq, Eq)]
pub enum UserFuncErr {
	ArgsCnt {
		func_signature: String,
		func_name_pos: CodePos,
		actual_cnt: usize,
		given_cnt: usize,
	},
	ArgType {
		func_signature: String,
		arg_name: NameToken,
		arg_expr_pos: CodePos,
		actual_type: DataType,
		given_type: DataType,
	},
	NotDefined {
		name: NameToken,
	},
	AlreadyExists {
		name: NameToken, 
	},
}

impl std::fmt::Display for UserFuncErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			UserFuncErr::ArgsCnt { func_signature, actual_cnt, given_cnt, .. } =>
				write!(f, "Wrong arguments count for user-defined function:\n{}\n{} expected but {} found", func_signature, actual_cnt, given_cnt),
			UserFuncErr::ArgType { func_signature, arg_name, actual_type, given_type, .. } =>
				write!(f, "Wrong type of argument '{}' for user-defined function:\n{}\n'{}' expected but '{}' found", arg_name, func_signature, actual_type, given_type),
			UserFuncErr::NotDefined { name } =>
				write!(f, "User-defined function {} is not defined", name.value()),
			UserFuncErr::AlreadyExists { name } => write!(f, "User-defined function {} already defined", name.value()),
		}
	}
}