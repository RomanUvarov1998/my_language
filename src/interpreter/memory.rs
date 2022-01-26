use super::var_data::{VarData, DataType, VarErr, Value};
//use super::func_data::{FuncsDataList, FuncData, BoxedFuncBodyClosure, FuncArg, FuncErr};

use super::expr::Expr;
use super::InterpErr;
//use super::func_data::FuncsDefList;
use super::utils::NameToken;

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
	
	pub fn set_variable(&mut self, name: &NameToken, value: Value) -> Result<(), VarErr> {
		let var = self.find_var_mut(name)?;
		var.set(value)?;
		Ok(())
	}
	
	pub fn get_variable_value<'mem>(&'mem self, name: &NameToken) -> Result<&'mem Value, VarErr> {
		let var = self.find_var(name)?;
		match var.get_value() {
			Some(value) => Ok(value),
			None => Err( VarErr::NotSet { name: name.clone() } ),
		}
	}
	
	pub fn get_variable_type(&self, name: &NameToken) -> Result<DataType, VarErr> {
		let var = self.find_var(name)?;
		Ok(var.get_type())
	}
	
	pub fn call_func(&mut self, name: &NameToken, arg_exprs: Vec<Expr>) -> Result<(), InterpErr> {
		todo!();
		//self.user_defined_funcs.try_call(&name, arg_exprs)
	}
	
	fn find_var<'mem>(&'mem self, name: &NameToken) -> Result<&'mem VarData, VarErr> {
		match self.vars.iter().find(|var| var.get_name() == name) {
			Some(var) => Ok(var),
			None => Err( VarErr::NotDefined { name: name.clone() } ),
		}
	}
	
	fn find_var_mut<'mem>(&'mem mut self, name: &NameToken) -> Result<&'mem mut VarData, VarErr> {
		match self.vars.iter_mut().find(|var| var.get_name() == name) {
			Some(var) => Ok(var),
			None => Err( VarErr::NotDefined { name: name.clone() } ),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use super::super::utils::NameToken;
	
	#[test]
	fn add_uninit_variable() {
		let mut mem = Memory::new();
		
		let nt = NameToken::new("a");
		
		mem.add_variable(nt.clone(), DataType::Float32, None).unwrap();
		
		assert_eq!(mem.vars[0], VarData::new_uninit(nt.clone(), DataType::Float32));
		
		assert_eq!(mem.get_variable_value(&nt), Err(VarErr::NotSet { name: nt.clone() }));
		
		assert_eq!(mem.get_variable_type(&nt), Ok(DataType::Float32));
	}
	
	#[test]
	fn add_variable_with_value() {
		let mut mem = Memory::new();
		
		let nt = NameToken::new("a");
		
		mem.add_variable(nt.clone(), DataType::Float32, Some(Value::Float32(2_f32))).unwrap();
		
		assert_eq!(mem.vars[0], VarData::new_with_value(nt.clone(), DataType::Float32, Value::Float32(2_f32)).unwrap());
		
		assert_eq!(mem.get_variable_value(&nt), Ok(&Value::Float32(2_f32)));
		
		assert_eq!(mem.get_variable_type(&nt), Ok(DataType::Float32));
	}
}