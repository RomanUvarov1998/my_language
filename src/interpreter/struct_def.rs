use super::utils::NameToken;
use super::data_type::DataType;
use super::user_func::UserFuncDef;
use super::builtin_func::BuiltinFuncDef;
use super::value::Value;
use std::cell::{RefCell, Ref, RefMut};
use std::rc::Rc;

//------------------ StructDef ---------------------

#[allow(dead_code)]
pub struct StructDef {
	inner: Rc<RefCell<StructDefInner>>
}

impl StructDef {
	#[allow(dead_code)]
	pub fn new(name: NameToken, default_value: Value) -> Self {
		Self {
			inner: Rc::new(RefCell::new(StructDefInner::new(name, default_value))),
		}
	}
	
	#[allow(dead_code)]
	pub fn inner(&self) -> Ref<'_, StructDefInner> {
		self.inner.borrow()
	}
	
	#[allow(dead_code)]
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

#[allow(dead_code)]
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
	
	#[allow(dead_code)]
	pub fn name(&self) -> &NameToken {
		&self.name
	}
	
	#[allow(dead_code)]
	pub fn default_value(&self) -> Value {
		self.default_value.clone()
	}
	
	#[allow(dead_code)]
	pub fn add_builtin_func_def(&mut self, func_def: BuiltinFuncDef) {
		if let Some(_) = self.builtin_funcs.iter().find(|fd| fd.name() == func_def.name()) {
			panic!("Builtin member function '{}' is already defined", func_def.name());
		}
		
		self.builtin_funcs.push(func_def);
	}
	
	#[allow(dead_code)]
	pub fn find_builtin_func_def(&self, name: &NameToken) -> Result<&BuiltinFuncDef, StructDefErr> {
		match self.builtin_funcs.iter().find(|fd| fd.name() == name.value()) {
			Some(func_def) => Ok(func_def),
			None => Err( StructDefErr::BuiltinMemberFuncIsNotDefined { name: name.clone() } )
		}
	}
}

//------------------ StructFieldDef ---------------------

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct StructFieldDef {
	name: NameToken,
	data_type: DataType,
}

//------------------ StructDefErr ---------------------

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
pub enum StructDefErr {
	FieldAlreadyDefined {
		name_in_code: NameToken,
	},
	BuiltinMemberFuncIsNotDefined {
		name: NameToken,
	},
}

impl std::fmt::Display for StructDefErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			StructDefErr::FieldAlreadyDefined { name_in_code } => 
				write!(f, "Member field '{}' is already defined", &name_in_code),
			StructDefErr::BuiltinMemberFuncIsNotDefined { name } => 
				write!(f, "Builtin member function '{}' is not defined", &name),
		}
	}
}