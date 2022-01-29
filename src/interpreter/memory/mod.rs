mod call_stack_frame;

use call_stack_frame::CallStackFrame;

use super::var_data::{VarData, DataType, VarErr, Value};
use super::InterpErr;
use super::utils::NameToken;
use super::func_data::{UserFuncErr, UserFuncArg, UserFuncDef, BuiltinFuncsDefList};
use super::statement::ReturningBody;

//----------------------------------- Memory -----------------------

#[derive(Debug)]
pub struct Memory {
	call_stack: Vec<CallStackFrame>,
}

#[allow(unused)]
impl Memory {
	pub fn new() -> Self {
		Self {
			call_stack: vec![CallStackFrame::new()],
		}
	}
	
	pub fn add_variable(&mut self, name: NameToken, data_type: DataType, initial_value: Option<Value>) -> Result<(), VarErr> {
		self.get_upper_frame_mut()
			.get_upper_scope_mut()
			.add_variable(name, data_type, initial_value)
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
	
	pub fn add_user_func(&mut self, name: NameToken, args: Vec<UserFuncArg>, return_type: DataType, body: ReturningBody) -> Result<(), UserFuncErr> {
		self.get_upper_frame_mut()
			.get_upper_scope_mut()
			.add_user_func(name, args, return_type, body)
	}
		
	pub fn push_scope(&mut self) {
		self.get_upper_frame_mut()
			.push_scope();
	}
	
	pub fn pop_scope(&mut self) {
		self.get_upper_frame_mut()
			.pop_scope();
	}
	
	pub fn push_frame(&mut self) {
		self.call_stack.push(CallStackFrame::new());
	}
	
	pub fn pop_frame(&mut self) {
		self.call_stack.pop().unwrap();
	}
	
	pub fn find_func_def(&self, name: &NameToken) -> Result<&UserFuncDef, UserFuncErr> {
		let frame: &CallStackFrame = self.call_stack.last().unwrap();
		frame.find_func_def(name)
	}
	
	fn get_upper_frame_mut(&mut self) -> &mut CallStackFrame {
		self.call_stack.last_mut().unwrap()
	}
	
	fn find_var(&self, name: &NameToken) -> Result<&VarData, VarErr> {
		let frame: &CallStackFrame = self.call_stack.last().unwrap();
		frame.find_var(name)
	}
	
	fn find_var_mut(&mut self, name: &NameToken) -> Result<&mut VarData, VarErr> {
		let frame: &mut CallStackFrame = self.call_stack.last_mut().unwrap();
		frame.find_var_mut(name)
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
		
		let nt_a = new_name_token("a");
		let nt_b = new_name_token("b");
		
		mem.add_variable(nt_a.clone(), DataType::Float32, None).unwrap();
		assert_eq!(mem.get_variable_value(&nt_a), Err(VarErr::NotSet { name: nt_a.clone() }));
		assert_eq!(mem.get_variable_type(&nt_a), Ok(DataType::Float32));
		mem.set_variable(&nt_a, Value::from(1_f32)).unwrap();
		assert_eq!(mem.get_variable_value(&nt_a), Ok(&Value::Float32(1_f32)));
		assert_eq!(mem.get_variable_type(&nt_a), Ok(DataType::Float32));
		
		assert_eq!(
			mem.add_variable(nt_a.clone(), DataType::Float32, None),
			Err( VarErr::AlreadyExists { name: nt_a.clone() } ));	
		
		mem.push_scope();
		mem.add_variable(nt_a.clone(), DataType::Float32, None).unwrap();
		assert_eq!(mem.get_variable_value(&nt_a), Err(VarErr::NotSet { name: nt_a.clone() }));		
		assert_eq!(mem.get_variable_type(&nt_a), Ok(DataType::Float32));
		mem.set_variable(&nt_a, Value::from(3_f32)).unwrap();
		assert_eq!(mem.get_variable_value(&nt_a), Ok(&Value::Float32(3_f32)));
		assert_eq!(mem.get_variable_type(&nt_a), Ok(DataType::Float32));
		
		assert_eq!(
			mem.add_variable(nt_a.clone(), DataType::Float32, None),
			Err( VarErr::AlreadyExists { name: nt_a.clone() } ));	
			
		mem.add_variable(nt_b.clone(), DataType::Float32, None).unwrap();
		assert_eq!(mem.get_variable_value(&nt_b), Err(VarErr::NotSet { name: nt_b.clone() }));		
		assert_eq!(mem.get_variable_type(&nt_b), Ok(DataType::Float32));
		mem.set_variable(&nt_b, Value::from(5_f32)).unwrap();
		assert_eq!(mem.get_variable_value(&nt_b), Ok(&Value::Float32(5_f32)));
		assert_eq!(mem.get_variable_type(&nt_b), Ok(DataType::Float32));
		
		assert_eq!(mem.get_variable_value(&nt_a), Ok(&Value::Float32(3_f32)));
		assert_eq!(mem.get_variable_type(&nt_a), Ok(DataType::Float32));
			
		mem.pop_scope();	
		assert_eq!(
			mem.get_variable_type(&nt_b),
			Err( VarErr::NotDefined { name: nt_b.clone() } ));	
		assert_eq!(
			mem.get_variable_value(&nt_b),
			Err( VarErr::NotDefined { name: nt_b.clone() } ));	
		
		assert_eq!(mem.get_variable_value(&nt_a), Ok(&Value::Float32(1_f32)));
		assert_eq!(mem.get_variable_type(&nt_a), Ok(DataType::Float32));
	}
	
	#[test]
	fn add_variable_with_value() {
		let mut mem = Memory::new();
		
		let nt = new_name_token("a");
		
		mem.add_variable(nt.clone(), DataType::Float32, Some(Value::Float32(2_f32))).unwrap();
		
		assert_eq!(mem.get_variable_value(&nt), Ok(&Value::Float32(2_f32)));
		
		assert_eq!(mem.get_variable_type(&nt), Ok(DataType::Float32));
	}

	fn new_name_token(name: &str) -> NameToken {
		use super::super::utils::{CodePos, CharPos};
		NameToken::new_with_pos(name, CodePos::from(CharPos::new()))
	}
}