use super::var_data::DataType;

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
	
	pub fn add(&mut self, func_def: FuncDef) -> Result<(), FuncErr> {
		let func_name: &str = func_def.get_name();
		if let Some(..) =  self.funcs
			.iter()
			.find(|func| func.get_name() == func_name) {
			return Err( FuncErr::AlreadyDefined );
		}
		
		self.funcs.push(func_def);
		
		Ok(())
	}
	
	pub fn try_find<'mem>(&'mem self, name: &str) -> Result<&'mem FuncDef, FuncErr> {
		match self.funcs.iter().find(|func| func.get_name() == name) {
			Some(func) => Ok(func),
			None => Err( FuncErr::NotDefined ),
		}
	}
}

//------------------------- FuncDef -----------------------

pub struct FuncDef {
	name: &'static str,
	args: Vec<FuncArg>,
}

impl FuncDef {
	pub fn new(name: &'static str, args: Vec<FuncArg>) -> Self {
		Self {
			name,
			args,
		}
	}
	
	pub fn check_args(&self, args_data_types: Vec<DataType>) -> Result<(), FuncErr> {
		if self.args.len() != args_data_types.len() {
			return Err( FuncErr::ArgsCnt { 
				actual_cnt: self.args.len(),
				given_cnt: args_data_types.len(),
			} );
		}
		
		for i in 0..self.args.len() {
			if !self.args[i].type_check(args_data_types[i]) {
				return Err( FuncErr::ArgType { 
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FuncErr {
	ArgsCnt {
		actual_cnt: usize,
		given_cnt: usize,
	},
	ArgType { // TODO: provide token to keep wrong argument position
		actual_type: Option<DataType>,
		given_type: DataType,
	},
	NotDefined,
	AlreadyDefined,
}