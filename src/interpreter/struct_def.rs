use super::data_type::DataType;
use super::user_func::UserFuncDef;
use super::builtin_func::BuiltinFuncDef;
use super::utils::{CodePos, NameToken};
use std::cell::{RefCell, Ref, RefMut};
use std::rc::Rc;
use std::collections::HashMap;

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
	
	pub fn inner_mut(&self) -> RefMut<'_, StructDefInner> {
		self.inner.borrow_mut()
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
	fields: HashMap<String, StructFieldDef>,
	member_funcs: Vec<UserFuncDef>,	// TODO: use HashMap for member_funcs and builtin_funcs
	builtin_funcs: Vec<BuiltinFuncDef>,
}

impl StructDefInner {
	pub fn new(name: NameToken, fields: HashMap<String, StructFieldDef>) -> Self {
		Self {
			name,
			fields,
			member_funcs: Vec::new(),
			builtin_funcs: Vec::new(),
		}
	}
	
	pub fn name(&self) -> &NameToken {
		&self.name
	}
		
	pub fn add_builtin_func_def(&mut self, func_def: BuiltinFuncDef) {
		if let Some(_) = self.builtin_funcs.iter().find(|fd| fd.name() == func_def.name()) {
			panic!("Builtin member function '{}' is already defined", func_def.name());
		}
		
		self.builtin_funcs.push(func_def);
	}
	
	pub fn find_builtin_func_def(&self, name: &NameToken) -> Result<&BuiltinFuncDef, StructDefErr> {
		match self.builtin_funcs.iter().find(|fd| fd.name() == name.value()) {
			Some(func_def) => Ok(func_def),
			None => Err( StructDefErr::BuiltinMemberFuncIsNotDefined { name: name.clone() } )
		}
	}
}

//------------------------- StructFieldDef -----------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructFieldDef {
	name: NameToken,
	data_type: DataType,
}

//------------------ StructDefErr ---------------------

#[derive(Debug, PartialEq, Eq)]
pub enum StructDefErr {
	StructDefNotInRootContext {
		struct_pos: CodePos,
	},
	StructDefIsAlreadyDefined {
		defined_name: NameToken,
	},
	FieldAlreadyDefined {
		name_in_code: NameToken,
	},
	BuiltinMemberFuncIsNotDefined {
		name: NameToken,
	},
	StructIsAlreadyDefined {
		name: NameToken,
	},
}

impl std::fmt::Display for StructDefErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			StructDefErr::StructDefNotInRootContext { .. } =>
				write!(f, "Struct definitions can only be in the global scope"),
			StructDefErr::StructDefIsAlreadyDefined { defined_name } => 
				write!(f, "Struct '{}' is alresdy defined", defined_name.value()),
			StructDefErr::FieldAlreadyDefined { name_in_code } => 
				write!(f, "Member field '{}' is already defined", &name_in_code),
			StructDefErr::BuiltinMemberFuncIsNotDefined { name } => 
				write!(f, "Builtin member function '{}' is not defined", &name),
			StructDefErr::StructIsAlreadyDefined { name } => 
				write!(f, "Struct '{}' is already defined", &name),
		}
	}
}