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
	pub fn can_run_declare_than_set_variable() {
		println!("start"); 
		let mut int = Interpreter::new();
		
		println!("run 1");
		int.run("var a: f32;").unwrap();
		
		assert_eq!(
			int.memory.find_var("a").unwrap(), 
			&VarData::new_uninit(String::from("a"), DataType::Float32));
		
		println!("run 2");
		int.run("a = 0.3 + 0.5;").unwrap();
		
		assert_eq!(
			int.memory.find_var("a").unwrap(), 
			&VarData::new_with_value(String::from("a"), DataType::Float32, Value::Float32 (0.8_f32)).unwrap());
		 
		println!("run 3");
		int.run("a = a + 0.5;").unwrap();
		
		assert_eq!(
			int.memory.find_var("a").unwrap(), 
			&VarData::new_with_value(String::from("a"), DataType::Float32, Value::Float32 (1.3_f32)).unwrap());
		
		println!("run 4");
		int.run("a = a * 2 + 1.4;").unwrap();
		
		assert_eq!(
			int.memory.find_var("a").unwrap(), 
			&VarData::new_with_value(String::from("a"), DataType::Float32, Value::Float32 (4_f32)).unwrap());
	}
}