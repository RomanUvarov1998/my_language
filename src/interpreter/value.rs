use super::data_type::{DataType, BuiltinType};
use super::struct_def::StructDef;
use super::utils::NameToken;
use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

//------------------- Value -------------------

#[derive(Debug)]
pub enum Value {
	Float32 (f32),
	String (Rc<RefCell<Vec<char>>>), // TODO: implement string using Value::Array and template for Array<Value::Char>
	Bool (bool),
	Char (char),
	Struct {
		struct_def: StructDef,
		fields: HashMap<String, Rc<RefCell<Value>>>,
	},
	Array {
		values: Rc<RefCell<Vec<Value>>>,
	},
	Any, // TODO: added for typechecking purposes, remove it later and use another context for code checking. With such another context instead of setting variables to default value using DataType::default_value(), just mark them as 'initialized'
	None,
}

impl Value {
	pub fn get_type(&self) -> DataType {
		match self {
			Value::Float32 (_) => DataType::Builtin (BuiltinType::Float32),
			Value::String (_) => DataType::Builtin (BuiltinType::String),
			Value::Bool (_) => DataType::Builtin (BuiltinType::Bool),
			Value::Char (_) => DataType::Builtin (BuiltinType::Char),
			Value::Struct { struct_def, .. } => DataType::UserDefined(struct_def.clone()), // TODO: try avoid cloning and return reference
			Value::Array { .. } => DataType::Builtin (BuiltinType::Array),
			Value::Any => DataType::Builtin (BuiltinType::Any),
			Value::None => DataType::Builtin (BuiltinType::None),
		}
	}
	
	pub fn get_clone_with_the_same_inner(&self) -> Value {
		match self {
			Value::Float32 (val) => Value::Float32 (*val),
			Value::String (ref chars_vec_rc) => Value::String(Rc::clone(chars_vec_rc)),
			Value::Bool (val) => Value::Bool (*val),
			Value::Char (val) => Value::Char(*val),
			Value::Struct { struct_def, fields } => {
				let mut fields_cloned = HashMap::<String, Rc<RefCell<Value>>>::with_capacity(fields.capacity());
				
				for (key, value_rc) in fields.iter() {
					fields_cloned.insert(
						key.clone(), 
						Rc::clone(value_rc));
				}
				
				Value::Struct {
					struct_def: struct_def.clone(),
					fields: fields_cloned,
				}
			},
			Value::Array { ref values } => {
				let elems_vec_cloned: Vec<Value> = values.borrow().clone();
				Value::Array {
					values: Rc::clone(values),
				}
			},
			Value::Any => Value::Any,
			Value::None => Value::None,
		}
	}
	
	pub fn unwrap_struct_clone_field(&self, field_name: &NameToken) -> Rc<RefCell<Value>> {
		if let Value::Struct { ref fields, .. } = self {
			Rc::clone(&fields.get(field_name.value()).unwrap())
		} else { unreachable!(); }
	}
	
	// TODO: add and use more of such functions in 'run' code
	pub fn unwrap_f32(&self) -> f32 {
		if let Value::Float32 (v) = self {
			*v
		} else { unreachable!(); }
	}
	
	pub fn unwrap_str_clone_inner_rc(&self) -> Rc<RefCell<Vec<char>>> {
		if let Value::String (chars) = self {
			Rc::clone(&chars)
		} else { unreachable!(); }
	}
	
	pub fn unwrap_str_get_char(&self, index: usize) -> char {
		if let Value::String (chars) = self {
			chars.borrow()[index]
		} else { unreachable!(); }
	}
}

impl Clone for Value {
	fn clone(&self) -> Self {
		match self {
			Value::Float32 (val) => Value::Float32 (*val),
			Value::String (chars_vec_rc) => {
				let vec_cloned: Vec<char> = chars_vec_rc.borrow().clone();
				Value::String(Rc::new(RefCell::new(vec_cloned)))
			},
			Value::Bool (val) => Value::Bool (*val),
			Value::Char (val) => Value::Char(*val),
			Value::Struct { struct_def, fields } => {
				let mut fields_cloned = HashMap::<String, Rc<RefCell<Value>>>::with_capacity(fields.capacity());
				
				for (key, value_rc) in fields.iter() {
					let value_cloned: Value = value_rc.borrow().clone();
					fields_cloned.insert(
						key.clone(), 
						Rc::new(RefCell::new(value_cloned)));
				}
				
				Value::Struct {
					struct_def: struct_def.clone(),
					fields: fields_cloned,
				}
			},
			Value::Array { values } => {
				let elems_vec_cloned: Vec<Value> = values.borrow().clone();
				Value::Array {
					values: Rc::new(RefCell::new(elems_vec_cloned)),
				}
			},
			Value::Any => Value::Any,
			Value::None => Value::None,
		}
	}
}

impl Eq for Value {}

impl PartialEq for Value {
	fn eq(&self, other: &Self) -> bool {
		match self {
			Value::Float32 (f1) => match other {
				Value::Float32 (f2) => (f1 - f2).abs() <= std::f32::EPSILON,
				_ => false,
			},
			Value::String (s1) => match other {
				Value::String (s2) => s1 == s2,
				_ => false,
			},
			Value::Bool (b1) => match other {
				Value::Bool (b2) => b1 == b2,
				_ => false,
			},
			Value::Char (c1) => match other {
				Value::Char (c2) => c1 == c2,
				_ => false,
			},
			Value::Struct { struct_def: sd1, .. } => match other {
				Value::Struct { struct_def: sd2, .. } => sd1 == sd2,
				_ => false,
			},
			Value::Array { values: vs1 } => match other {
				Value::Array { values: vs2 } => vs1 == vs2,
				_ => false,
			},
			Value::Any => match other {
				Value::Any => true,
				_ => false,
			},
			Value::None => match other {
				Value::None => true,
				_ => false,
			},
		}
	}
}

impl std::fmt::Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Value::Float32 (v) => write!(f, "{}", v),
			Value::String (v) => {
				for ch in v.borrow().iter() {
					write!(f, "{}", ch)?;
				}
				Ok(())
			},
			Value::Bool (v) => if *v {
				write!(f, "True")
			} else {
				write!(f, "False")
			},
			Value::Char (c) => write!(f, "{}", c),
			Value::Struct { struct_def, fields } => {
				writeln!(f, "Value of struct '{}' {{", struct_def.inner().name().value())?;
				for (key, val) in fields {
					writeln!(f, "\t{}: {},", key, val.borrow())?;
				}
				writeln!(f, "}}")
			},
			Value::Array { values } => {
				write!(f, "Array [")?;
				let mut is_first = true;
				for v in values.borrow().iter() {
					if !is_first { write!(f, ", ")?; }
					is_first = false;
					write!(f, "{}", v)?;
				}
				write!(f, "]")
			},
			Value::Any => write!(f, "Any"),
			Value::None => write!(f, "None"),
		}
	}
}

impl From<f32> for Value {
	fn from(val: f32) -> Self {
		Value::Float32(val)
	}
}

impl From<String> for Value {
	fn from(val: String) -> Self {
		let chars_vec: Vec<char> = val.chars().collect();
		Value::String(Rc::new(RefCell::new(chars_vec)))
	}
}

impl From<&str> for Value {
	fn from(val: &str) -> Self {
		let chars_vec: Vec<char> = val.chars().collect();
		Value::String(Rc::new(RefCell::new(chars_vec)))
	}
}

impl From<char> for Value {
	fn from(val: char) -> Self {
		Value::Char(val)
	}
}

impl From<bool> for Value {
	fn from(val: bool) -> Self {
		Value::Bool(val)
	}
}