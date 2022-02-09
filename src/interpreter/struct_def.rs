use super::data_type::DataType;
use super::user_func::UserFuncDef;
use super::builtin_func::BuiltinFuncDef;
use super::utils::{CodePos, NameToken};
use super::context::Context;
use super::expr::StructLiteralField;
use super::InterpErr;
use std::cell::{RefCell, Ref, RefMut};
use std::rc::Rc;
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Values;

//------------------ StructDef ---------------------

pub struct StructDef {
	inner: Rc<RefCell<StructDefInner>>
}

impl StructDef {
	pub fn new(name: NameToken, fields: Vec<StructFieldDef>) -> Result<Self, StructDefErr> {
		let mut struct_fields = HashMap::<String, StructFieldDef>::new();
		
		for field in fields.iter() {
			if let Some(existing_field) = struct_fields.get(field.name.value()) {
				return Err( StructDefErr::FieldAlreadyDefined {
					name_in_code: existing_field.name.clone(),
				} );
			}
			
			// TODO: avoid cloning field
			struct_fields.insert(field.name.to_string(), field.clone());
		}
		
		Ok( Self {
			inner: Rc::new(RefCell::new(StructDefInner::new(name, struct_fields))),
		} )
	}
	
	pub fn inner(&self) -> Ref<'_, StructDefInner> {
		self.inner.borrow()
	}
	
	#[allow(dead_code)]
	pub fn inner_mut(&self) -> RefMut<'_, StructDefInner> {
		self.inner.borrow_mut()
	}
	
	pub fn check_fields_set(&self, struct_name_in_code: &NameToken, fields: &Vec<StructLiteralField>, check_context: &Context) -> Result<(), InterpErr> {
		let mut defined_fields = HashSet::<String>::new();
		let inner = self.inner();
		for field in fields {
			// TODO: try use &str or Cow to not to copy String
			let field_name: String = field.field_name().value().to_string();
			match inner.fields.get(&field_name) {
				Some(field_def_ref) => {
					if !defined_fields.insert(field_name.clone()) {
						return Err( StructDefErr::FieldAlreadySet { name_in_code: field.field_name().clone() }.into() );
					}
					let expr_data_type: DataType = field.value_expr().check_and_calc_data_type(check_context)?;
					if field_def_ref.data_type != expr_data_type {
						return Err( StructDefErr::FieldSetTypeErr { 
							name_in_code: field.field_name().clone(),
							declared_type: field_def_ref.data_type.clone(),
							set_type: expr_data_type,
						}.into() );
					}
				},
				None => return Err( StructDefErr::FieldDoesNotExist { name_in_code: field.field_name().clone() }.into() ),
			}
		}
		
		if defined_fields.len() != fields.len() {
			return Err( StructDefErr::NotAllFieldsSet { name_in_code: struct_name_in_code.clone() }.into() );
		}
		
		Ok(())
	}
}

impl Clone for StructDef {
	fn clone(&self) -> Self {
		Self {
			inner: Rc::clone(&self.inner),
		}
	}
}

impl Eq for StructDef {}
impl PartialEq for StructDef {
	fn eq(&self, other: &Self) -> bool {
		self.inner.borrow().name().value() == other.inner.borrow().name().value()
	}
}

impl std::fmt::Debug for StructDef {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let inner: &StructDefInner = &self.inner.borrow();
		write!(f, "{:?}", inner)
	}
}

//------------------ StructDefInner ---------------------

#[derive(Debug)]
pub struct StructDefInner {
	name: NameToken,
	fields: HashMap<String, StructFieldDef>, // TODO: try using &str
	user_funcs: Vec<UserFuncDef>,	// TODO: use HashMap for user_funcs and builtin_funcs
	builtin_funcs: Vec<BuiltinFuncDef>,
}

impl StructDefInner {
	pub fn new(name: NameToken, fields: HashMap<String, StructFieldDef>) -> Self {
		Self {
			name,
			fields,
			user_funcs: Vec::new(),
			builtin_funcs: Vec::new(),
		}
	}
	
	pub fn name(&self) -> &NameToken {
		&self.name
	}
	
	pub fn field_defs(&self) -> Values<'_, String, StructFieldDef> {
		self.fields.values()
	}
	
	pub fn member_field(&self, name: &NameToken) -> Result<StructFieldDef, StructDefErr> {
		match self.fields.get(name.value()) {
			Some(field_def) => Ok(field_def.clone()),
			None => Err( StructDefErr::FieldDoesNotExist {
				name_in_code: name.clone(),
			} ),
		}
	}
	
	#[allow(dead_code)]
	pub fn add_builtin_func_def(&mut self, func_def: BuiltinFuncDef) {
		if let Some(_) = self.builtin_funcs.iter().find(|fd| fd.name() == func_def.name()) {
			panic!("Builtin member function '{}' is already defined", func_def.name());
		}
		
		self.builtin_funcs.push(func_def);
	}
	
	#[allow(dead_code)]
	pub fn add_user_func_def(&mut self, func_def: UserFuncDef) {
		if let Some(_) = self.user_funcs.iter().find(|fd| fd.name() == func_def.name()) {
			panic!("User-defined member function '{}' is already defined", func_def.name());
		}
		
		self.user_funcs.push(func_def);
	}
	
	pub fn find_builtin_func_def(&self, name: &NameToken) -> Result<BuiltinFuncDef, StructDefErr> {
		match self.builtin_funcs.iter().find(|fd| fd.name() == name.value()) {
			Some(func_def) => Ok(func_def.clone()),
			None => Err( StructDefErr::BuiltinMemberFuncIsNotDefined { name: name.clone() } )
		}
	}

	pub fn find_user_func_def(&self, name: &NameToken) -> Result<UserFuncDef, StructDefErr> {
		match self.user_funcs.iter().find(|fd| fd.name().value() == name.value()) {
			Some(func_def) => Ok(func_def.clone()),
			None => Err( StructDefErr::UserMemberFuncIsNotDefined { name: name.clone() } )
		}
	}
}

//------------------------- StructFieldDef -----------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructFieldDef {
	name: NameToken,
	data_type: DataType,
}

impl StructFieldDef {
	pub fn new(name: NameToken, data_type: DataType) -> Self {
		Self {
			name,
			data_type,
		}
	}
	
	pub fn name(&self) -> &NameToken {
		&self.name
	}

	pub fn data_type(&self) -> &DataType {
		&self.data_type
	}
}

//------------------ StructDefErr ---------------------

// TODO: refactor fields' names
#[derive(Debug, PartialEq, Eq)]
pub enum StructDefErr {
	NotAStruct {
		value_pos: CodePos,
	},
	StructDefNotInRootContext {
		struct_pos: CodePos,
	},
	StructDefIsAlreadyDefined {
		defined_name: NameToken,
	},
	FieldAlreadyDefined {
		name_in_code: NameToken,
	},
	FieldAlreadySet {
		name_in_code: NameToken,
	},
	FieldSetTypeErr {
		name_in_code: NameToken,
		declared_type: DataType,
		set_type: DataType,
	},
	NotAllFieldsSet {
		name_in_code: NameToken,
	},
	FieldDoesNotExist {
		name_in_code: NameToken,
	},
	BuiltinMemberFuncIsNotDefined {
		name: NameToken,
	},
	UserMemberFuncIsNotDefined {
		name: NameToken,
	},
	ComplexDataTypeHasNoBuiltinFields {
		name_in_code: NameToken,
	},
}

impl std::fmt::Display for StructDefErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			StructDefErr::NotAStruct { .. } =>
				write!(f, "Struct definitions can only be in the global scope"),
			StructDefErr::StructDefNotInRootContext { .. } =>
				write!(f, "Struct definitions can only be in the global scope"),
			StructDefErr::StructDefIsAlreadyDefined { defined_name } => 
				write!(f, "Struct '{}' is alresdy defined", defined_name.value()),
			StructDefErr::FieldAlreadyDefined { name_in_code } => 
				write!(f, "Member field '{}' is already defined", &name_in_code),
			StructDefErr::FieldAlreadySet { name_in_code } => 
				write!(f, "Member field '{}' is already set", &name_in_code),
			StructDefErr::FieldSetTypeErr { name_in_code, declared_type, set_type } => 
				write!(f, "Member field '{}' has '{}' type buts its expression is '{}'", 
					&name_in_code, declared_type, set_type),
			StructDefErr::NotAllFieldsSet { name_in_code } => 
				write!(f, "Not all fields of struct '{}' are set", &name_in_code),
			StructDefErr::FieldDoesNotExist { name_in_code } => 
				write!(f, "Member field '{}' is not declared", &name_in_code),
			StructDefErr::BuiltinMemberFuncIsNotDefined { name } => 
				write!(f, "Builtin member function '{}' is not defined", &name),
			StructDefErr::UserMemberFuncIsNotDefined { name } => 
				write!(f, "User member function '{}' is not defined", &name),
			StructDefErr::ComplexDataTypeHasNoBuiltinFields { .. } => 
				write!(f, "Complex data type has no builtin fields"),
		}
	}
}