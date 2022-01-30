use super::super::super::var_data::{VarData, Value, VarErr, DataType};
use super::super::super::function::{UserFuncErr, UserFuncDef, UserFuncArg};
use super::super::super::utils::NameToken;
use super::super::super::statement::ReturningBody;

#[derive(Debug)]
pub struct Scope {
	vars: Vec<VarData>,
	user_defined_funcs: Vec<UserFuncDef>,
}

impl Scope {
	pub fn new() -> Self {
		Self {
			vars: Vec::new(),
			user_defined_funcs: Vec::new(),
		}
	}
	
	pub fn add_variable(&mut self, name: NameToken, data_type: DataType, initial_value: Option<Value>) -> Result<(), VarErr> {
		if let Ok(..) = self.find_var(&name) {
			return Err( VarErr::AlreadyExists { name } );
		}
		
		if let Some(value) = initial_value {
			self.vars.push(VarData::new_with_value(name, data_type, value)?);
		} else {
			self.vars.push(VarData::new_uninit(name, data_type));
		}
		
		Ok(())
	}
	
	pub fn add_user_func(&mut self, name: NameToken, args: Vec<UserFuncArg>, return_type: DataType, body: ReturningBody) -> Result<(), UserFuncErr> {
		if let Ok(..) = self.find_func_def(&name) {
			return Err( UserFuncErr::AlreadyExists { name } );
		}
		
		self.user_defined_funcs.push(UserFuncDef::new(
			name, args, return_type, body));
		
		Ok(())
	}
	
	pub fn find_var(&self, name: &NameToken) -> Result<&VarData, VarErr> {
		match self.vars.iter().find(|var| var.get_name() == name) {
			Some(var) => Ok(var),
			None => Err( VarErr::NotDefined { name: name.clone() } ),
		}
	}
	
	pub fn find_var_mut(&mut self, name: &NameToken) -> Result<&mut VarData, VarErr> {
		match self.vars.iter_mut().find(|var| var.get_name() == name) {
			Some(var) => Ok(var),
			None => Err( VarErr::NotDefined { name: name.clone() } ),
		}
	}

	pub fn find_func_def(&self, name: &NameToken) -> Result<&UserFuncDef, UserFuncErr> {
		match self.user_defined_funcs.iter().find(|func| func.get_name() == name) {
			Some(func) => Ok(func),
			None => Err( UserFuncErr::NotDefined { name: name.clone() } ),
		}
	}
}
