mod var_data;
mod func_data;

pub use var_data::*;
pub use func_data::*;

use super::expr::Expr;
use super::InterpErr;

pub struct Memory {
	vars: Vec<VarData>,
	funcs: Vec<FuncData>,
}

impl Memory {
	pub fn new() -> Self {
		Self {
			vars: Vec::new(),
			funcs: vec![
				FuncData::new(
					String::from("print"), 
					vec![
						FuncArg::new(String::from("value"), DataType::Float32),
					], 
					Box::new(
						|args_data: Vec<VarValue>| -> () {
							println!(">>> {:?}", args_data[0]);
						}
					) as BoxedFuncBodyClosure,
				),
			]
		}
	}
	
	pub fn add_variable(&mut self, name: String, data_type: DataType) -> Result<(), VarErr> {
		if let Ok(..) = self.find_var(&name) {
			Err( VarErr::AlreadyExists { name: name.to_string() } )
		} else {
			self.vars.push(VarData::new(name, data_type));
			Ok(())
		}
	}
	
	pub fn set_variable(&mut self, name: &str, value: VarValue) -> Result<(), VarErr> {
		let var = self.find_var_mut(name)?;
		var.set(value)?;
		Ok(())
	}
	
	pub fn get_variable<'mem>(&'mem self, name: &str) -> Result<&'mem VarValue, VarErr> {
		let var = self.find_var(name)?;
		match var.get_value() {
			Some(value) => Ok(value),
			None => Err( VarErr::NotSet { name: name.to_string() } ),
		}
	}
	
	pub fn call_func(&mut self, name: String, arg_exprs: Vec<Expr>) -> Result<(), InterpErr> {
		let func = self.find_func(&name)?;
		
		let mut args_data = Vec::<VarValue>::new();
		
		for expr in arg_exprs {
			args_data.push(expr.calc(&self)?);
		}
		
		func.call(args_data)?;
		
		Ok(())
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
	
	fn find_func<'mem>(&'mem self, name: &str) -> Result<&'mem FuncData, FuncErr> {
		match self.funcs.iter().find(|func| func.get_name() == name) {
			Some(func) => Ok(func),
			None => Err( FuncErr::NotDefined { name: name.to_string() } ),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use super::super::*;
	
	#[test]
	pub fn can_run_declare_than_set_variable() {
		let mut int = Interpreter::new();
		
		int.run("var a: f32;").unwrap();
		
		assert_eq!(
			int.memory.find_var("a").unwrap(), 
			&VarData::new(String::from("a"), DataType::Float32));
		
		int.run("a = 0.3 + 0.5;").unwrap();
		
		assert_eq!(
			int.memory.find_var("a").unwrap(), 
			&VarData::new(String::from("a"), DataType::Float32).with_value(VarValue::Float32 (0.8_f32)).unwrap());
		
		int.run("a = a + 0.5;").unwrap();
		
		assert_eq!(
			int.memory.find_var("a").unwrap(), 
			&VarData::new(String::from("a"), DataType::Float32).with_value(VarValue::Float32 (1.3_f32)).unwrap());
		
		int.run("a = a * 2 + 1.4;").unwrap();
		
		assert_eq!(
			int.memory.find_var("a").unwrap(), 
			&VarData::new(String::from("a"), DataType::Float32).with_value(VarValue::Float32 (4_f32)).unwrap());
	}
}