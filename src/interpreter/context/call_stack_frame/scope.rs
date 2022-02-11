use super::super::super::value::Value;
use super::super::super::data_type::DataType;
use super::super::super::var_data::{VarData, VarErr};
use super::super::super::user_func::{UserFuncErr, UserFuncDef, UserFuncArg};
use super::super::super::utils::{NameToken, HashMapInsertPanic};
use super::super::super::statement::ReturningBody;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Scope {
	vars: HashMap<String, VarData>,
	user_defined_funcs: HashMap<String, UserFuncDef>,
}

impl Scope {
	pub fn new() -> Self {
		Self {
			vars: HashMap::new(),
			user_defined_funcs: HashMap::new(),
		}
	}
	
	pub fn add_variable(&mut self, name: NameToken, data_type: DataType, initial_value: Value) -> Result<(), VarErr> {
		if let Ok(..) = self.find_var(&name) {
			return Err( VarErr::AlreadyExists { name } );
		}
		
		self.vars.insert_assert_not_replace(
			name.value().to_string(),
			VarData::new_with_value(name, data_type, initial_value)?);
		
		Ok(())
	}
	
	pub fn add_user_func(&mut self, name: NameToken, args: Vec<UserFuncArg>, return_type: DataType, body: ReturningBody) -> Result<(), UserFuncErr> {
		if let Ok(..) = self.find_func_def(&name) {
			return Err( UserFuncErr::AlreadyExists { name } );
		}
		
		self.user_defined_funcs.insert_assert_not_replace(
			name.value().to_string(),
			UserFuncDef::new(name, args, return_type, body));
		
		Ok(())
	}
	
	pub fn find_var(&self, name: &NameToken) -> Result<&VarData, VarErr> {
		match self.vars.get(name.value()) {
			Some(var) => Ok(var),
			None => Err( VarErr::NotDefined { name: name.clone() } ),
		}
	}
	
	pub fn find_var_mut(&mut self, name: &NameToken) -> Result<&mut VarData, VarErr> {
		match self.vars.get_mut(name.value()) {
			Some(var) => Ok(var),
			None => Err( VarErr::NotDefined { name: name.clone() } ),
		}
	}

	pub fn find_func_def(&self, name: &NameToken) -> Result<&UserFuncDef, UserFuncErr> {
		match self.user_defined_funcs.get(name.value()) {
			Some(func) => Ok(func),
			None => Err( UserFuncErr::NotDefined { name: name.clone() } ),
		}
	}
}
