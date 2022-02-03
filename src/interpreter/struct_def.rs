use super::utils::NameToken;
use super::data_type::DataType;
use super::user_func::UserFuncDef;
use super::builtin_func::BuiltinFuncDef;
use super::value::Value;
use std::cell::{RefCell, Ref, RefMut};
use std::rc::Rc;

//------------------ StructDef ---------------------

pub struct StructDef {
	inner: Rc<RefCell<StructDefInner>>
}

impl StructDef {
	pub fn new(name: NameToken, default_value: Value) -> Self {
		Self {
			inner: Rc::new(RefCell::new(StructDefInner::new(name, default_value))),
		}
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
	fields: Vec<StructFieldDef>,
	member_funcs: Vec<UserFuncDef>,
	builtin_funcs: Vec<BuiltinFuncDef>,
	default_value: Value,
}

impl StructDefInner {
	pub fn new(name: NameToken, default_value: Value) -> Self {
		Self {
			name,
			fields: Vec::new(),
			member_funcs: Vec::new(),
			builtin_funcs: Vec::new(),
			default_value,
		}
	}
	
	pub fn name(&self) -> &NameToken {
		&self.name
	}
	
	pub fn default_value(&self) -> Value {
		self.default_value.clone()
	}
	
	pub fn add_builtin_func_def(&mut self, func_def: BuiltinFuncDef) -> Result<(), StructDefErr> {
		if let Some(_) = self.builtin_funcs.iter().find(|fd| fd.name() == func_def.name()) {
			return Err( StructDefErr::BuiltinMemberFuncAlreadyDefined { name: func_def.name() } );
		}
		
		self.builtin_funcs.push(func_def);
		
		Ok(())
	}
	
	pub fn find_builtin_func_def(&self, name: &str) -> Result<&BuiltinFuncDef, StructDefErr> {
		match self.builtin_funcs.iter().find(|fd| fd.name() == name) {
			Some(func_def) => Ok(func_def),
			None => Err( StructDefErr::BuiltinMemberFuncIsNotDefined { name: name.to_string() } )
		}
	}
}

//------------------ StructFieldDef ---------------------

#[derive(Debug, Clone)]
pub struct StructFieldDef {
	name: NameToken,
	data_type: DataType,
}

//------------------ StructDefErr ---------------------

#[derive(Debug, PartialEq, Eq)]
pub enum StructDefErr {
	FieldAlreadyDefined {
		name: String,
	},
	BuiltinMemberFuncAlreadyDefined {
		name: &'static str,
	},
	BuiltinMemberFuncIsNotDefined {
		name: String,
	},
}

impl std::fmt::Display for StructDefErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			StructDefErr::FieldAlreadyDefined { name } => 
				write!(f, "Member field '{}' is already defined", &name),
			StructDefErr::BuiltinMemberFuncAlreadyDefined { name } => 
				write!(f, "Builtin member function '{}' is already defined", name),
			StructDefErr::BuiltinMemberFuncIsNotDefined { name } => 
				write!(f, "Builtin member function '{}' is not defined", &name),
		}
	}
}