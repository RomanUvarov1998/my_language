mod call_stack_frame;

use call_stack_frame::CallStackFrame;
use super::builtin_func::{BuiltinFuncDef, BuiltinFuncErr};
use super::utils::{NameToken, CodePos, CharPos, HashMapInsertPanic};
use super::value::Value;
use super::data_type::{DataType, BuiltinType, DataTypeErr};
use super::var_data::{VarData, VarErr};
use super::user_func::{UserFuncErr, UserFuncArg, UserFuncDef};
use super::statement::{ReturningBody, ParsedFuncArgDef};
use super::primitive_type_member_builtin_funcs_list::PrimitiveTypeMemberBuiltinFuncsList;
use super::struct_def::{StructDef, StructFieldDef, StructDefErr};
use super::data_type_template::{DataTypeTemplate, UserFuncDefTemplate, StructFieldDefTemplate};
use super::statement::{Statement, StatementKind};
use super::expr::{Expr, Symbol, SymbolKind, Operand, ExprOperator};
use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;

//---------------------- Context -----------------------

pub struct Context<'prev_context> {
	prev_frame_context: Option<&'prev_context Context<'prev_context>>,
	frame: CallStackFrame,
	builtin_func_defs: &'prev_context Vec<BuiltinFuncDef>,
	primitive_type_member_builtin_funcs_list: &'prev_context PrimitiveTypeMemberBuiltinFuncsList,
	struct_defs: Rc<RefCell<Vec<StructDef>>>,
	struct_def_templates: Rc<RefCell<HashMap<String, DataTypeTemplate>>>,
	is_root: bool,
}

impl<'prev_context> Context<'prev_context> {
	pub fn new(
		builtin_func_defs: &'prev_context Vec<BuiltinFuncDef>,
		primitive_type_member_builtin_funcs_list: &'prev_context PrimitiveTypeMemberBuiltinFuncsList,
		mut struct_defs: Vec<StructDef>
	) -> Self {
		let null_pos = || { CodePos::from(CharPos::new()) };
		let new_nt = |name: &str, is_builtin: bool| -> NameToken {
			NameToken::new_with_pos(name.to_string(), null_pos(), is_builtin)
		};
		
		let self_param_name = "Self";
		let item_type_param_name = "ItemType";
		let mut array_template = DataTypeTemplate::new("arr");
		
		let user_func_len = UserFuncDefTemplate::new(
			new_nt("len", false),
			vec![
				ParsedFuncArgDef::new(new_nt("self", false), new_nt(self_param_name, false)),
			],
			Some(new_nt("f32", true)),
			vec![
				Statement::new(null_pos(),
					StatementKind::UserDefinedFuncReturn {
						return_expr: Some(Expr::new_from_stack(vec![
							Symbol {
								pos: null_pos(),
								kind: SymbolKind::Operand (Operand::Variable (new_nt("self", false))),
							},
							Symbol {
								pos: null_pos(),
								kind: SymbolKind::Operand (Operand::Variable (new_nt("inner", false))),
							},
							Symbol {
								pos: null_pos(),
								kind: SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
							},
							Symbol {
								pos: null_pos(),
								kind: SymbolKind::Operand (Operand::FuncCall {
									func_name: new_nt("len", true), 
									arg_exprs: Vec::new(),
								}),
							},
							Symbol {
								pos: null_pos(),
								kind: SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
							},
						])),
					}),
			]);
		array_template.add_user_func_template(user_func_len);
		
		let user_func_add = UserFuncDefTemplate::new(
			new_nt("add", false),
			vec![
				ParsedFuncArgDef::new(new_nt("self", false), new_nt(self_param_name, false)),
				ParsedFuncArgDef::new(new_nt("item", false), new_nt(item_type_param_name, false)),
			],
			None,
			vec![
				Statement::new(null_pos(),
					StatementKind::FuncCall {
						left_expr: Expr::new_from_stack(vec![
							Symbol {
								pos: null_pos(),
								kind: SymbolKind::Operand (Operand::Variable (new_nt("self", false))),
							},
							Symbol {
								pos: null_pos(),
								kind: SymbolKind::Operand (Operand::Variable (new_nt("inner", false))),
							},
							Symbol {
								pos: null_pos(),
								kind: SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
							},
							Symbol {
								pos: null_pos(),
								kind: SymbolKind::Operand (Operand::FuncCall {
									func_name: new_nt("add", true), 
									arg_exprs: vec![
										Expr::new_from_stack(vec![
											Symbol {
												pos: null_pos(),
												kind: SymbolKind::Operand (Operand::Variable (new_nt("item", false))),
											},
										]),
									],
								}),
							},
							Symbol {
								pos: null_pos(),
								kind: SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
							}
						]),
					}),
			]);
		array_template.add_user_func_template(user_func_add);
		
		let user_func_get = UserFuncDefTemplate::new(
			new_nt("get", false),
			vec![
				ParsedFuncArgDef::new(new_nt("self", false), new_nt(self_param_name, false)),
				ParsedFuncArgDef::new(new_nt("index", false), new_nt("f32", true)),
			],
			Some(new_nt(item_type_param_name, false)),
			vec![
				Statement::new(null_pos(),
					StatementKind::UserDefinedFuncReturn {
						return_expr: Some(Expr::new_from_stack(vec![
							Symbol {
								pos: null_pos(),
								kind: SymbolKind::Operand (Operand::Variable (new_nt("self", false))),
							},
							Symbol {
								pos: null_pos(),
								kind: SymbolKind::Operand (Operand::Variable (new_nt("inner", false))),
							},
							Symbol {
								pos: null_pos(),
								kind: SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
							},
							Symbol {
								pos: null_pos(),
								kind: SymbolKind::Operand (Operand::FuncCall {
									func_name: new_nt("get", true), 
									arg_exprs: vec![
										Expr::new_from_stack(vec![
											Symbol {
												pos: null_pos(),
												kind: SymbolKind::Operand (Operand::Variable (new_nt("index", false))),
											},
										]),
									],
								}),
							},
							Symbol {
								pos: null_pos(),
								kind: SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
							}
						])),
					}),
			]);
		array_template.add_user_func_template(user_func_get);
		
		let array_inner = StructFieldDefTemplate::new(new_nt("inner", false), new_nt("untyped_array", true));
		
		array_template.add_field_template(array_inner);		
		
		let tmpls: HashMap<String, DataTypeTemplate> = [
			("arr".to_string(), array_template),
		].into();
		
		let mut ctx = Self {
			prev_frame_context: None,
			frame: CallStackFrame::new(),
			builtin_func_defs,
			primitive_type_member_builtin_funcs_list,
			struct_defs: Rc::new(RefCell::new(struct_defs)),
			struct_def_templates: Rc::new(RefCell::new(tmpls)),
			is_root: true,
		};
			
		let type_params: HashMap<String, NameToken> = [
			(item_type_param_name.to_string(), new_nt("f32", true)),
		].into();
		
		let mut templates = ctx.struct_def_templates.borrow();
		
		let templ = templates.get("arr").unwrap();
		
		let mut td = templ.generate_type_def(
			&type_params,
			&ctx);
			
		ctx.struct_defs.borrow_mut().push(td.clone());
		
		templ.generate_user_member_funcs(&mut td, type_params, &ctx);
		
		drop(templates);
		
		ctx
	}
	
	pub fn new_stack_frame_context(&'prev_context self) -> Self {
		Self {
			prev_frame_context: Some(&self),
			frame: CallStackFrame::new(),
			builtin_func_defs: &self.builtin_func_defs,
			primitive_type_member_builtin_funcs_list: &self.primitive_type_member_builtin_funcs_list,
			struct_defs: Rc::clone(&self.struct_defs),
			struct_def_templates: Rc::clone(&self.struct_def_templates),
			is_root: false,
		}
	}
	
	
	pub fn push_scope(&mut self) {
		self.frame.push_scope();
	}
	
	pub fn pop_scope(&mut self) {
		self.frame.pop_scope();
	}
	
	
	pub fn add_variable(&mut self, name: NameToken, data_type: DataType, initial_value: Value) -> Result<(), VarErr> {
		self.frame.get_upper_scope_mut()
			.add_variable(name, data_type, initial_value)
	}
	
	pub fn set_variable(&mut self, name: &NameToken, value: Value) -> Result<(), VarErr> {
		let var: &mut VarData = self.frame.find_var_mut(name)?;
		var.set(value)
	}
	
	pub fn get_variable_value(&self, name: &NameToken) -> Result<Rc<RefCell<Value>>, VarErr> {
		Ok(self.frame.find_var(name)?.get_value())
	}
	
	#[allow(dead_code)]
	pub fn get_variable_def_mut(&mut self, name: &NameToken) -> Result<&mut VarData, VarErr> {
		self.frame.find_var_mut(name)
	}
	
	pub fn get_variable_def(&self, name: &NameToken) -> Result<&VarData, VarErr> {
		self.frame.find_var(name)
	}
	
	pub fn add_user_func(&mut self, name: NameToken, args: Vec<UserFuncArg>, return_type: DataType, body: ReturningBody) -> Result<(), UserFuncErr> {
		self.frame.get_upper_scope_mut()
			.add_user_func(name, args, return_type, body)
	}
	
	pub fn find_type_by_name(&self, name: &NameToken) -> Result<DataType, DataTypeErr> {
		if name.is_builtin() {
			match name.value() {
				"f32" => Ok( DataType::Builtin (BuiltinType::Float32) ),
				"str" => Ok( DataType::Builtin (BuiltinType::String) ),
				"bool" => Ok( DataType::Builtin (BuiltinType::Bool) ),
				"char" => Ok( DataType::Builtin (BuiltinType::Char) ),
				"untyped_array" => Ok( DataType::Builtin (BuiltinType::UntypedArray) ),
				_ => Err( DataTypeErr::NotDefined { name: name.clone() } ),
			}
		} else {
			let defs = self.struct_defs.borrow();
			if let Some(dt) = defs.iter().find(|sd| sd.inner().name().value() == name.value()) {
				Ok( DataType::UserDefined (dt.clone()) )
			} else {
				Err( DataTypeErr::NotDefined { name: name.clone() } )
			}
		}
	}
	
	pub fn add_user_template(&self, template: DataTypeTemplate) {
		if self.is_root {
			let mut templ_defs = self.struct_def_templates.borrow_mut();
			templ_defs.insert_assert_not_replace(
				template.name().to_string(),
				template); // TODO: return Result instead of panicking
		} else {
			panic!("Template can only be defined in th global scope");
		}
	}
	
	pub fn add_user_struct(&mut self, name: NameToken, fields: Vec<StructFieldDef>) -> Result<(), StructDefErr> {
		if name.is_builtin() {
			return Err( StructDefErr::CannotDefineAsBuiltin { name: name.clone() } );
		}
		
		if self.is_root {
			let mut defs = self.struct_defs.borrow_mut();
			
			if let Some(struct_def) = defs.iter().find(|sd| sd.inner().name().value() == name.value()) {
				return Err( StructDefErr::StructDefIsAlreadyDefined {
					defined_name: struct_def.inner().name().clone(),
				} );
			}
			
			defs.push(StructDef::new(name, fields)?);
			
			Ok(())
		} else {
			Err( StructDefErr::StructDefNotInRootContext {
				struct_pos: name.pos(),
			} )
		}
	}
		
	pub fn find_user_func_def(&self, name: &NameToken) -> Result<&UserFuncDef, UserFuncErr> {
		if let Ok(func) = self.frame.find_user_func_def(name) {
			Ok(func)
		} else if let Some(prev_context) = self.prev_frame_context {
			if let Ok(func) = prev_context.find_user_func_def(name) {
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

	pub fn find_member_builtin_func_def(
		&'prev_context self, 
		data_type: &DataType,
		func_name: &'prev_context NameToken
	) -> Result<BuiltinFuncDef, StructDefErr> 
	{
		assert!(func_name.is_builtin());
		match data_type {
			DataType::Builtin (dt) => self.primitive_type_member_builtin_funcs_list.find_func(*dt, func_name),
			DataType::UserDefined (struct_def) => struct_def.inner().find_builtin_func_def(func_name),
		}
	}
	
	pub fn find_member_user_func_def(
		&self, 
		data_type: &DataType,
		func_name: &NameToken
	) -> Result<UserFuncDef, StructDefErr> 
	{
		assert!(!func_name.is_builtin());
		match data_type {
			DataType::Builtin (_) => todo!(),
			DataType::UserDefined (struct_def) => struct_def.inner().find_user_func_def(func_name),
		}
	}
	
	pub fn find_member_field_def(
		&self, 
		data_type: &DataType,
		field_name: &NameToken
	) -> Result<StructFieldDef, StructDefErr> 
	{
		match data_type {
			DataType::Builtin (_) => return Err( StructDefErr::NotAStruct {
				value_pos: field_name.pos(),
			} ),
			DataType::UserDefined (struct_def) => match field_name.is_builtin() {
				true => return Err( StructDefErr::ComplexDataTypeHasNoBuiltinFields { 
				name_in_code: field_name.clone() } ),
				false => struct_def.inner().member_field(field_name),
			},
		}
	}
}

//---------------------- Tests -----------------------

#[cfg(test)]
mod tests {
	use super::Context;
	use super::super::utils::NameToken;
	use super::super::builtin_func::BuiltinFuncDef;
	use super::super::data_type::{DataType, BuiltinType};
	use super::super::var_data::VarErr;
	use super::super::value::Value;
	use super::super::primitive_type_member_builtin_funcs_list::PrimitiveTypeMemberBuiltinFuncsList;
	use super::super::struct_def::StructDef;
	
	#[test]
	fn add_uninit_variable() {
		let builtin_func_defs = Vec::<BuiltinFuncDef>::new();
		let primitive_type_member_builtin_funcs_list = PrimitiveTypeMemberBuiltinFuncsList::new();
		let mut context = Context::new(
			&builtin_func_defs,
			&primitive_type_member_builtin_funcs_list,
			Vec::<StructDef>::new());
		
		let nt_a = new_name_token("a", false);
		let nt_b = new_name_token("b", false);
		
		context.add_variable(nt_a.clone(), DataType::Builtin (BuiltinType::Float32), Value::from(3_f32)).unwrap();
		assert_eq!(*context.get_variable_value(&nt_a).unwrap().borrow(), Value::from(3_f32));
		assert_eq!(context.get_variable_def_mut(&nt_a).unwrap().get_type(), &DataType::Builtin (BuiltinType::Float32));
		context.set_variable(&nt_a, Value::from(1_f32)).unwrap();
		assert_eq!(*context.get_variable_value(&nt_a).unwrap().borrow(), Value::Float32(1_f32));
		assert_eq!(context.get_variable_def_mut(&nt_a).unwrap().get_type(), &DataType::Builtin (BuiltinType::Float32));
		
		assert_eq!(
			context.add_variable(nt_a.clone(), DataType::Builtin (BuiltinType::Float32), Value::from(1_f32)),
			Err( VarErr::AlreadyExists { name: nt_a.clone() } ));	
		
		context.push_scope();
		context.add_variable(nt_a.clone(), DataType::Builtin (BuiltinType::Float32), Value::from(3_f32)).unwrap();
		assert_eq!(*context.get_variable_value(&nt_a).unwrap().borrow(), Value::from(3_f32));		
		assert_eq!(context.get_variable_def_mut(&nt_a).unwrap().get_type(), &DataType::Builtin (BuiltinType::Float32));
		context.set_variable(&nt_a, Value::from(3_f32)).unwrap();
		assert_eq!(*context.get_variable_value(&nt_a).unwrap().borrow(), Value::Float32(3_f32));
		assert_eq!(context.get_variable_def_mut(&nt_a).unwrap().get_type(), &DataType::Builtin (BuiltinType::Float32));
		
		assert_eq!(
			context.add_variable(nt_a.clone(), DataType::Builtin (BuiltinType::Float32), Value::from(4_f32)),
			Err( VarErr::AlreadyExists { name: nt_a.clone() } ));	
			
		context.add_variable(nt_b.clone(), DataType::Builtin (BuiltinType::Float32), Value::from(3_f32)).unwrap();
		assert_eq!(*context.get_variable_value(&nt_b).unwrap().borrow(), Value::from(3_f32));
		assert_eq!(context.get_variable_def_mut(&nt_b).unwrap().get_type(), &DataType::Builtin (BuiltinType::Float32));
		context.set_variable(&nt_b, Value::from(5_f32)).unwrap();
		assert_eq!(*context.get_variable_value(&nt_b).unwrap().borrow(), Value::Float32(5_f32));
		assert_eq!(context.get_variable_def_mut(&nt_b).unwrap().get_type(), &DataType::Builtin (BuiltinType::Float32));
		
		assert_eq!(*context.get_variable_value(&nt_a).unwrap().borrow(), Value::Float32(3_f32));
		assert_eq!(context.get_variable_def_mut(&nt_a).unwrap().get_type(), &DataType::Builtin (BuiltinType::Float32));
			
		context.pop_scope();	
		assert_eq!(
			context.get_variable_def_mut(&nt_b),
			Err( VarErr::NotDefined { name: nt_b.clone() } ));	
		assert_eq!(
			context.get_variable_value(&nt_b),
			Err( VarErr::NotDefined { name: nt_b.clone() } ));	
		
		assert_eq!(*context.get_variable_value(&nt_a).unwrap().borrow(), Value::Float32(1_f32));
		assert_eq!(context.get_variable_def_mut(&nt_a).unwrap().get_type(), &DataType::Builtin (BuiltinType::Float32));
	}
	
	#[test]
	fn add_variable_with_value() {
		let builtin_func_defs = Vec::<BuiltinFuncDef>::new();
		let primitive_type_member_builtin_funcs_list = PrimitiveTypeMemberBuiltinFuncsList::new();
		let mut context = Context::new(
			&builtin_func_defs,
			&primitive_type_member_builtin_funcs_list,
			Vec::<StructDef>::new());
		
		let nt = new_name_token("a", false);
		
		context.add_variable(nt.clone(), DataType::Builtin (BuiltinType::Float32), Value::Float32(2_f32)).unwrap();
		
		assert_eq!(*context.get_variable_value(&nt).unwrap().borrow(), Value::Float32(2_f32));
		
		assert_eq!(context.get_variable_def_mut(&nt).unwrap().get_type(), &DataType::Builtin (BuiltinType::Float32));
	}

	fn new_name_token(name: &str, is_builtin: bool) -> NameToken {
		use super::super::utils::{CodePos, CharPos};
		NameToken::new_with_pos(name.to_string(), CodePos::from(CharPos::new()), is_builtin)
	}
}