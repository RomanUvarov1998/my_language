mod call_stack_frame;

use call_stack_frame::CallStackFrame;
use super::builtin_func::{BuiltinFuncDef, BuiltinFuncErr};
use super::utils::NameToken;
use super::value::Value;
use super::data_type::DataType;
use super::var_data::{VarData, VarErr};
use super::user_func::{UserFuncErr, UserFuncArg, UserFuncDef};
use super::statement::ReturningBody;
use super::primitive_type_member_funcs_list::PrimitiveTypeMemberFuncsList;

//---------------------- Context -----------------------

pub struct Context<'prev_context> {
	prev_frame_context: Option<&'prev_context CallStackFrame>,
	frame: CallStackFrame,
	builtin_func_defs: &'prev_context Vec<BuiltinFuncDef>,
	primitive_type_member_funcs_list: &'prev_context PrimitiveTypeMemberFuncsList,
}

impl<'prev_context> Context<'prev_context> {
	pub fn new(
		builtin_func_defs: &'prev_context Vec<BuiltinFuncDef>,
		primitive_type_member_funcs_list: &'prev_context PrimitiveTypeMemberFuncsList
	) -> Self {
		Self {
			prev_frame_context: None,
			frame: CallStackFrame::new(),
			builtin_func_defs,
			primitive_type_member_funcs_list,
		}
	}
	
	pub fn new_stack_frame_context(&'prev_context self) -> Self {
		Self {
			prev_frame_context: Some(&self.frame),
			frame: CallStackFrame::new(),
			builtin_func_defs: &self.builtin_func_defs,
			primitive_type_member_funcs_list: &self.primitive_type_member_funcs_list,
		}
	}
	
	pub fn add_variable(&mut self, name: NameToken, data_type: DataType, initial_value: Option<Value>) -> Result<(), VarErr> {
		self.frame.get_upper_scope_mut()
			.add_variable(name, data_type, initial_value)
	}
	
	pub fn set_variable(&mut self, name: &NameToken, value: Value) -> Result<(), VarErr> {
		let var: &mut VarData = self.frame.find_var_mut(name)?;
		var.set(value)
	}
	
	pub fn get_variable_value(&self, name: &NameToken) -> Result<&Value, VarErr> {
		let var = self.frame.find_var(name)?;
		match var.get_value() {
			Some(value) => Ok(value),
			None => Err( VarErr::NotSet { name: name.clone() } ),
		}
	}
	
	pub fn get_variable_mut(&mut self, name: &NameToken) -> Result<&mut VarData, VarErr> {
		self.frame.find_var_mut(name)
	}
	
	pub fn add_user_func(&mut self, name: NameToken, args: Vec<UserFuncArg>, return_type: DataType, body: ReturningBody) -> Result<(), UserFuncErr> {
		self.frame.get_upper_scope_mut()
			.add_user_func(name, args, return_type, body)
	}
		
	pub fn push_scope(&mut self) {
		self.frame.push_scope();
	}
	
	pub fn pop_scope(&mut self) {
		self.frame.pop_scope();
	}
	
	pub fn find_func_def(&self, name: &NameToken) -> Result<&UserFuncDef, UserFuncErr> {
		if let Ok(func) = self.frame.find_func_def(name) {
			Ok(func)
		} else if let Some(frame) = self.prev_frame_context {
			if let Ok(func) = frame.find_func_def(name) {
				Ok(func)
			} else {
				Err( UserFuncErr::NotDefined { name: name.clone() } )
			}
		} else {
			Err( UserFuncErr::NotDefined { name: name.clone() } )
		}
	}
	
	pub fn find_builtin_func_def(&'prev_context self, name: &'prev_context NameToken) -> Result<& 'prev_context BuiltinFuncDef, BuiltinFuncErr> {
		match self.builtin_func_defs.iter().find(|func| func.name() == name.value()) {
			Some(func) => Ok(func),
			None => Err( BuiltinFuncErr::NotDefined { name_pos: name.pos() } ),
		}
	}

	pub fn find_member_builtin_func_def(&'prev_context self, name: &'prev_context NameToken) -> Result<& 'prev_context BuiltinFuncDef, BuiltinFuncErr> {
		todo!();
	}
}

//---------------------- Tests -----------------------

#[cfg(test)]
mod tests {
	use super::Context;
	use super::super::utils::NameToken;
	use super::super::builtin_func::BuiltinFuncDef;
	use super::super::data_type::{DataType, Primitive};
	use super::super::var_data::VarErr;
	use super::super::value::Value;
	use super::super::primitive_type_member_funcs_list::PrimitiveTypeMemberFuncsList;
	
	#[test]
	fn add_uninit_variable() {
		let builtin_func_defs = Vec::<BuiltinFuncDef>::new();
		let primitive_type_member_funcs_list = PrimitiveTypeMemberFuncsList::new();
		let mut context = Context::new(
			&builtin_func_defs,
			&primitive_type_member_funcs_list);
		
		let nt_a = new_name_token("a");
		let nt_b = new_name_token("b");
		
		context.add_variable(nt_a.clone(), DataType::Primitive (Primitive::Float32), None).unwrap();
		assert_eq!(context.get_variable_value(&nt_a), Err(VarErr::NotSet { name: nt_a.clone() }));
		assert_eq!(context.get_variable_mut(&nt_a).unwrap().get_type(), &DataType::Primitive (Primitive::Float32));
		context.set_variable(&nt_a, Value::from(1_f32)).unwrap();
		assert_eq!(context.get_variable_value(&nt_a), Ok(&Value::Float32(1_f32)));
		assert_eq!(context.get_variable_mut(&nt_a).unwrap().get_type(), &DataType::Primitive (Primitive::Float32));
		
		assert_eq!(
			context.add_variable(nt_a.clone(), DataType::Primitive (Primitive::Float32), None),
			Err( VarErr::AlreadyExists { name: nt_a.clone() } ));	
		
		context.push_scope();
		context.add_variable(nt_a.clone(), DataType::Primitive (Primitive::Float32), None).unwrap();
		assert_eq!(context.get_variable_value(&nt_a), Err(VarErr::NotSet { name: nt_a.clone() }));		
		assert_eq!(context.get_variable_mut(&nt_a).unwrap().get_type(), &DataType::Primitive (Primitive::Float32));
		context.set_variable(&nt_a, Value::from(3_f32)).unwrap();
		assert_eq!(context.get_variable_value(&nt_a), Ok(&Value::Float32(3_f32)));
		assert_eq!(context.get_variable_mut(&nt_a).unwrap().get_type(), &DataType::Primitive (Primitive::Float32));
		
		assert_eq!(
			context.add_variable(nt_a.clone(), DataType::Primitive (Primitive::Float32), None),
			Err( VarErr::AlreadyExists { name: nt_a.clone() } ));	
			
		context.add_variable(nt_b.clone(), DataType::Primitive (Primitive::Float32), None).unwrap();
		assert_eq!(context.get_variable_value(&nt_b), Err(VarErr::NotSet { name: nt_b.clone() }));		
		assert_eq!(context.get_variable_mut(&nt_b).unwrap().get_type(), &DataType::Primitive (Primitive::Float32));
		context.set_variable(&nt_b, Value::from(5_f32)).unwrap();
		assert_eq!(context.get_variable_value(&nt_b), Ok(&Value::Float32(5_f32)));
		assert_eq!(context.get_variable_mut(&nt_b).unwrap().get_type(), &DataType::Primitive (Primitive::Float32));
		
		assert_eq!(context.get_variable_value(&nt_a), Ok(&Value::Float32(3_f32)));
		assert_eq!(context.get_variable_mut(&nt_a).unwrap().get_type(), &DataType::Primitive (Primitive::Float32));
			
		context.pop_scope();	
		assert_eq!(
			context.get_variable_mut(&nt_b),
			Err( VarErr::NotDefined { name: nt_b.clone() } ));	
		assert_eq!(
			context.get_variable_value(&nt_b),
			Err( VarErr::NotDefined { name: nt_b.clone() } ));	
		
		assert_eq!(context.get_variable_value(&nt_a), Ok(&Value::Float32(1_f32)));
		assert_eq!(context.get_variable_mut(&nt_a).unwrap().get_type(), &DataType::Primitive (Primitive::Float32));
	}
	
	#[test]
	fn add_variable_with_value() {
		let builtin_func_defs = Vec::<BuiltinFuncDef>::new();
		let primitive_type_member_funcs_list = PrimitiveTypeMemberFuncsList::new();
		let mut context = Context::new(
			&builtin_func_defs,
			&primitive_type_member_funcs_list);
		
		let nt = new_name_token("a");
		
		context.add_variable(nt.clone(), DataType::Primitive (Primitive::Float32), Some(Value::Float32(2_f32))).unwrap();
		
		assert_eq!(context.get_variable_value(&nt), Ok(&Value::Float32(2_f32)));
		
		assert_eq!(context.get_variable_mut(&nt).unwrap().get_type(), &DataType::Primitive (Primitive::Float32));
	}

	fn new_name_token(name: &str) -> NameToken {
		use super::super::utils::{CodePos, CharPos};
		NameToken::new_with_pos(name, CodePos::from(CharPos::new()))
	}
}