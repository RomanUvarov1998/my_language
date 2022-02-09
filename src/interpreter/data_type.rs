use super::value::Value;
use super::struct_def::{StructDef, StructDefErr};
use super::utils::{NameToken, CodePos};
use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

//--------------------------- DataType -------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataType {
	Primitive (Primitive),
	Complex (StructDef),
}

impl DataType {	
	pub fn default_value(&self) -> Value {
		match self {
			DataType::Primitive (dt) => dt.default_value(),
			DataType::Complex (sd) => {
				let mut fields = HashMap::<String, Rc<RefCell<Value>>>::new();
				let sd_inner = sd.inner();
				for field_def in sd_inner.field_defs() {
					fields.insert(
						field_def.name().value().to_string(),
						Rc::new(RefCell::new(field_def.data_type().default_value())));
				}
				Value::Struct {
					struct_def: sd.clone(),
					fields, 
				}
			}, // TODO: do not use this function for check purposes
		}
	}
}

impl std::fmt::Display for DataType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			DataType::Primitive (dt) => write!(f, "{}", dt),
			DataType::Complex (dt) => write!(f, "Struct '{}'", dt.inner().name().value()),
		}
	}
}

//--------------------------- Primitive -------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
pub enum Primitive {
	Float32,
	String,
	Bool,
	Char,
	Any,
	None,
}

impl Primitive {
	pub fn default_value(&self) -> Value {
		match self {
			Primitive::Float32 => Value::from(0_f32),
			Primitive::String => Value::from(String::new()),
			Primitive::Bool => Value::from(false),
			Primitive::Char => Value::from('a'),
			Primitive::Any => unreachable!(),
			Primitive::None => unreachable!(),
		}
	}
}

impl std::fmt::Display for Primitive {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Primitive::Float32 => write!(f, "f32"),
			Primitive::String => write!(f, "str"),
			Primitive::Bool => write!(f, "bool"),
			Primitive::Char => write!(f, "char"),
			Primitive::Any => write!(f, "Any"),
			Primitive::None => write!(f, "None"),
		}
	}
}

//--------------------------- DataTypeErr -------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataTypeErr {
	NotDefined {
		name: NameToken,
	},
	PrimitiveTypeInitializedAsComplex {
		type_name_in_code: NameToken,
	},
}

impl std::fmt::Display for DataTypeErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			DataTypeErr::NotDefined { name } => 
				write!(f, "{} dataType '{}' is not defined", 
					if name.is_builtin() { "Builtin" } else { "User-defined" }, 
					name.value()),
			DataTypeErr::PrimitiveTypeInitializedAsComplex { type_name_in_code } => 
				write!(f, "DataType '{}' value is primitive, but declared as struct", type_name_in_code.value()),
		}
	}
}