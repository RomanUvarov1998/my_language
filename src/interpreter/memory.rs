use super::var_data::{VarData, DataType, VarErr, Value};
//use super::func_data::{FuncsDataList, FuncData, BoxedFuncBodyClosure, FuncArg, FuncErr};

use super::expr::Expr;
use super::InterpErr;
//use super::func_data::FuncsDefList;
use super::utils::NameToken;

//----------------------------------- Memory -----------------------

#[derive(Debug)]
pub struct Memory {
	global_scope: Scope,
	local_scopes_stack: Vec<Scope>,
}

#[allow(unused)]
impl Memory {
	pub fn new() -> Self {
		Self {
			global_scope: Scope::new(),
			local_scopes_stack: Vec::<Scope>::new(),
		}
	}
	
	pub fn add_variable(&mut self, name: NameToken, data_type: DataType, initial_value: Option<Value>) -> Result<(), VarErr> {
		let scope: &mut Scope = match self.local_scopes_stack.last_mut() {
			Some(scope) => scope,
			None => &mut self.global_scope,
		};
		scope.add_variable(name, data_type, initial_value)
	}
	
	pub fn set_variable(&mut self, name: &NameToken, value: Value) -> Result<(), VarErr> {
		let var: &mut VarData = self.find_var_mut(name)?;
		var.set(value)
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
	
	pub fn push_scope(&mut self) {
		self.local_scopes_stack.push(Scope::new());
	}
	
	pub fn pop_scope(&mut self) {
		self.local_scopes_stack.pop().unwrap();
	}
	
	fn find_var<'mem>(&'mem self, name: &NameToken) -> Result<&'mem VarData, VarErr> {
		for scope in self.local_scopes_stack.iter().rev() {
			match scope.find_var(name) {
				res @ Ok(_) => return res,
				Err(_) => {},
			}
		}
				
		match self.global_scope.find_var(name) {
			res @ Ok(_) => res,
			Err(_) => Err( VarErr::NotDefined { name: name.clone() } ),
		}
	}
	
	fn find_var_mut<'mem>(&'mem mut self, name: &NameToken) -> Result<&'mem mut VarData, VarErr> {
		for scope in self.local_scopes_stack.iter_mut().rev() {
			match scope.find_var_mut(name) {
				res @ Ok(_) => return res,
				Err(_) => {},
			}
		}
				
		match self.global_scope.find_var_mut(name) {
			res @ Ok(_) => res,
			Err(_) => Err( VarErr::NotDefined { name: name.clone() } ),
		}
	}
}

//----------------------------------- Scope -----------------------

#[derive(Debug)]
struct Scope {
	vars: Vec<VarData>,
	//user_defined_funcs: FuncsDefList,
}

impl Scope {
	fn new() -> Self {
		Self {
			vars: Vec::new(),
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

//----------------------------------- Tests -----------------------

#[cfg(test)]
mod tests {
	use super::*;
	use super::super::utils::NameToken;
	
	#[test]
	fn add_uninit_variable() {
		let mut mem = Memory::new();
		
		let nt = new_name_token("a");
		let nt_b = new_name_token("b");
		
		mem.add_variable(nt.clone(), DataType::Float32, None).unwrap();			
		assert_eq!(mem.local_scopes_stack.len(), 0);		
		assert_eq!(mem.global_scope.vars[0], VarData::new_uninit(nt.clone(), DataType::Float32));		
		assert_eq!(mem.get_variable_value(&nt), Err(VarErr::NotSet { name: nt.clone() }));		
		assert_eq!(mem.get_variable_type(&nt), Ok(DataType::Float32));
		
		mem.push_scope();
		mem.add_variable(nt.clone(), DataType::Float32, None).unwrap();	
		assert_eq!(mem.local_scopes_stack.len(), 1);		
		assert_eq!(mem.local_scopes_stack[0].vars[0], VarData::new_uninit(nt.clone(), DataType::Float32));		
		assert_eq!(mem.get_variable_value(&nt), Err(VarErr::NotSet { name: nt.clone() }));		
		assert_eq!(mem.get_variable_type(&nt), Ok(DataType::Float32));
		
		assert_eq!(
			mem.add_variable(nt.clone(), DataType::Float32, None),
			Err( VarErr::AlreadyExists { name: nt.clone() } ));	
			
		mem.add_variable(nt_b.clone(), DataType::Float32, None).unwrap();
		assert_eq!(mem.local_scopes_stack[0].vars[1], VarData::new_uninit(nt_b.clone(), DataType::Float32));		
		assert_eq!(
			mem.get_variable_type(&nt_b),
			Ok(DataType::Float32));
		mem.set_variable(&nt_b, Value::from(1_f32)).unwrap();
		assert_eq!(
			mem.get_variable_value(&nt_b),
			Ok(&Value::from(1_f32)));
			
		mem.pop_scope();
		assert_eq!(mem.local_scopes_stack.len(), 0);		
		assert_eq!(
			mem.get_variable_type(&nt_b),
			Err( VarErr::NotDefined { name: nt_b.clone() } ));	
		assert_eq!(
			mem.get_variable_value(&nt_b),
			Err( VarErr::NotDefined { name: nt_b.clone() } ));	
	}
	
	#[test]
	fn add_variable_with_value() {
		let mut mem = Memory::new();
		
		let nt = new_name_token("a");
		
		mem.add_variable(nt.clone(), DataType::Float32, Some(Value::Float32(2_f32))).unwrap();
		
		assert_eq!(mem.global_scope.vars[0], VarData::new_with_value(nt.clone(), DataType::Float32, Value::Float32(2_f32)).unwrap());
		
		assert_eq!(mem.get_variable_value(&nt), Ok(&Value::Float32(2_f32)));
		
		assert_eq!(mem.get_variable_type(&nt), Ok(DataType::Float32));
	}

	fn new_name_token(name: &str) -> NameToken {
		use super::super::utils::{CodePos, CharPos};
		NameToken::new_with_pos(name, CodePos::from(CharPos::new()))
	}
}