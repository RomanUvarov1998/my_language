use super::var_data::{VarData, DataType, VarErr, Value};
//use super::func_data::{FuncsDataList, FuncData, BoxedFuncBodyClosure, FuncArg, FuncErr};

use super::expr::Expr;
use super::InterpErr;
//use super::func_data::FuncsDefList;

pub struct Memory {
	vars: Vec<VarData>,
	//user_defined_funcs: FuncsDefList,
}

#[allow(unused)]
impl Memory {
	pub fn new() -> Self {
		Self {
			vars: Vec::new(),
			//user_defined_funcs: FuncsDefList::new(),
		}
	}
	
	pub fn add_variable(&mut self, name: String, data_type: DataType, initial_value: Option<Value>) -> Result<(), VarErr> {
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
	
	pub fn set_variable(&mut self, name: &str, value: Value) -> Result<(), VarErr> {
		let var = self.find_var_mut(name)?;
		var.set(value)?;
		Ok(())
	}
	
	pub fn get_variable_value<'mem>(&'mem self, name: &str) -> Result<&'mem Value, VarErr> {
		let var = self.find_var(name)?;
		match var.get_value() {
			Some(value) => Ok(value),
			None => Err( VarErr::NotSet { name: name.to_string() } ),
		}
	}
	
	pub fn get_variable_type(&self, name: &str) -> Result<DataType, VarErr> {
		let var = self.find_var(name)?;
		Ok(var.get_type())
	}
	
	pub fn call_func(&mut self, name: String, arg_exprs: Vec<Expr>) -> Result<(), InterpErr> {
		todo!();
		//self.user_defined_funcs.try_call(&name, arg_exprs)
	}
	
	fn find_var<'mem>(&'mem self, name: &str) -> Result<&'mem VarData, VarErr> {
		match self.vars.iter().find(|var| var.get_name() == name) {
			Some(var) => Ok(var),
			None => Err( VarErr::NotDefined { name: name.to_string() } ),
		}
	}
	
	fn find_var_mut<'mem>(&'mem mut self, name: &str) -> Result<&'mem mut VarData, VarErr> {
		match self.vars.iter_mut().find(|var| var.get_name() == name) {
			Some(var) => Ok(var),
			None => Err( VarErr::NotDefined { name: name.to_string() } ),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use super::super::*;
	
	#[test]
	fn add_uninit_variable() {
		let mut mem = Memory::new();
		mem.add_variable(String::from("a"), DataType::Float32, None).unwrap();
		
		assert_eq!(mem.vars[0], VarData::new_uninit(String::from("a"), DataType::Float32));
		
		assert_eq!(mem.get_variable_value("a"), Err(VarErr::NotSet { name: String::from("a") }));
		
		assert_eq!(mem.get_variable_type("a"), Ok(DataType::Float32));
	}
	
	#[test]
	fn add_variable_with_value() {
		let mut mem = Memory::new();
		mem.add_variable(String::from("a"), DataType::Float32, Some(Value::Float32(2_f32))).unwrap();
		
		assert_eq!(mem.vars[0], VarData::new_with_value(String::from("a"), DataType::Float32, Value::Float32(2_f32)).unwrap());
		
		assert_eq!(mem.get_variable_value("a"), Ok(&Value::Float32(2_f32)));
		
		assert_eq!(mem.get_variable_type("a"), Ok(DataType::Float32));
	}
}