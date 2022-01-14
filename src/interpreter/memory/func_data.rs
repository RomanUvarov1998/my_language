use super::*;

pub struct FuncData {
	name: String,
	args: Vec<FuncArg>,
	body_closure: BoxedFuncBodyClosure,
}

pub type BoxedFuncBodyClosure = Box<dyn Fn(Vec<VarValue>) -> ()>;

impl FuncData {
	pub fn new(name: String, args: Vec<FuncArg>, body_closure: BoxedFuncBodyClosure) -> Self {
		Self {
			name,
			args,
			body_closure,
		}
	}
	
	pub fn call(&self, args_data: Vec<VarValue>) -> Result<(), FuncErr> {
		if self.args.len() != args_data.len() {
			return Err( FuncErr::ParamsCnt { 
				func_name: self.name.clone(), 
				actual_cnt: self.args.len(),
				given_cnt: args_data.len(),
			} );
		}
		
		for i in 0..self.args.len() {
			if args_data[i].get_type() != self.args[i].data_type {
				return Err( FuncErr::ParamType { 
					param_name: self.args[i].name.clone(),
					actual_type: self.args[i].data_type,
					given_type: args_data[i].get_type(),
				} );
			}
		}
		
		(self.body_closure)(args_data);
		
		Ok(())
	}
	
	pub fn get_name(&self) -> &str {
		&self.name
	}
}

pub struct FuncArg {
	name: String,
	data_type: DataType,
}

impl FuncArg {
	pub fn new(name: String, data_type: DataType) -> Self {
		Self {
			name,
			data_type,
		}
	}
}

#[derive(Debug, PartialEq, Eq)]
pub enum FuncErr {
	ParamsCnt {
		func_name: String, 
		actual_cnt: usize,
		given_cnt: usize,
	},
	ParamType { 
		param_name: String,
		actual_type: DataType,
		given_type: DataType,
	},
	NotDefined { name: String },
}

impl std::fmt::Display for FuncErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			FuncErr::ParamsCnt { func_name, actual_cnt, given_cnt } => 
				write!(f, "Function '{}' has {} parameters but {} were given", &func_name, actual_cnt, given_cnt),
			FuncErr::ParamType { param_name, actual_type, given_type } => 
				write!(f, "Parameter '{}' must have {:?} type but {:?} were given", &param_name, actual_type, given_type),
			FuncErr::NotDefined { name } => 
				write!(f, "Function '{}' is not defined", &name),
		}
	}
}