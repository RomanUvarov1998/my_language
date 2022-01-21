use super::var_data::DataType;
use super::InterpErr;

//------------------------- FuncsDefList -----------------------

pub struct FuncsDefList {
	funcs: Vec<FuncDef>,
}

impl FuncsDefList {
	pub fn new() -> Self {
		Self {
			funcs: Vec::new(),
		}
	}
	
	pub fn add(&mut self, func_def: FuncDef) -> Result<(), InterpErr> {
		let func_name: &str = func_def.get_name();
		if let Some(..) =  self.funcs
			.iter()
			.find(|func| func.get_name() == func_name) {
			return Err( InterpErr::from(FuncErr::AlreadyDefined { name: func_name.to_string() }) );
		}
		
		self.funcs.push(func_def);
		
		Ok(())
	}
	
	pub fn try_find<'mem>(&'mem self, name: &str) -> Result<&'mem FuncDef, FuncErr> {
		match self.funcs.iter().find(|func| func.get_name() == name) {
			Some(func) => Ok(func),
			None => Err( FuncErr::NotDefined { name: name.to_string() } ),
		}
	}
}

//------------------------- FuncDef -----------------------

pub struct FuncDef {
	name: String,
	args: Vec<FuncArg>,
}

impl FuncDef {
	pub fn new(name: String, args: Vec<FuncArg>) -> Self {
		Self {
			name,
			args,
		}
	}
	
	pub fn check_args(&self, args_data_types: Vec<DataType>) -> Result<(), FuncErr> {
		if self.args.len() != args_data_types.len() {
			return Err( FuncErr::ArgsCnt { 
				func_name: self.name.clone(), 
				actual_cnt: self.args.len(),
				given_cnt: args_data_types.len(),
			} );
		}
		
		for i in 0..self.args.len() {
			if !self.args[i].type_check(args_data_types[i]) {
				return Err( FuncErr::ArgType { 
					param_name: self.args[i].name.clone(),
					actual_type: self.args[i].data_type,
					given_type: args_data_types[i],
				} );
			}
		}
		
		Ok(())
	}
	
	pub fn get_name(&self) -> &str {
		&self.name
	}
}

pub struct FuncArg {
	name: String,
	data_type: Option<DataType>,
}

impl FuncArg {
	pub fn new(name: String, data_type: Option<DataType>) -> Self {
		Self {
			name,
			data_type,
		}
	}
	
	pub fn type_check(&self, data_type: DataType) -> bool {
		match self.data_type {
			Some(dt) => data_type == dt,
			None => true,
		}
	}
}

//------------------------- FuncErr -----------------------

#[derive(Debug, PartialEq, Eq)]
pub enum FuncErr {
	ArgsCnt {
		func_name: String, 
		actual_cnt: usize,
		given_cnt: usize,
	},
	ArgType { 
		param_name: String,
		actual_type: Option<DataType>,
		given_type: DataType,
	},
	NotDefined { name: String },
	AlreadyDefined { name: String },
}

impl std::fmt::Display for FuncErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			FuncErr::ArgsCnt { func_name, actual_cnt, given_cnt } => 
				write!(f, "Function '{}' has {} argument(-s) but {} were given", &func_name, actual_cnt, given_cnt),
			FuncErr::ArgType { param_name, actual_type, given_type } => 
				write!(f, "Argument '{}' must have {:?} type but {:?} were given", &param_name, actual_type, given_type),
			FuncErr::NotDefined { name } => 
				write!(f, "Function '{}' is not defined", &name),
			FuncErr::AlreadyDefined { name } => 
				write!(f, "Function '{}' already defined", &name),
		}
	}
}