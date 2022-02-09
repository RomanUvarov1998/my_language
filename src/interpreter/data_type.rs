use super::value::Value;
use super::struct_def::StructDef;
use super::utils::NameToken;
use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

//--------------------------- DataType -------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataType {
	Builtin (BuiltinType),
	UserDefined (StructDef),
}

impl DataType {	
	pub fn default_value(&self) -> Value {
		match self {
			DataType::Builtin (dt) => dt.default_value(),
			DataType::UserDefined (sd) => {
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
			DataType::Builtin (dt) => write!(f, "{}", dt),
			DataType::UserDefined (dt) => write!(f, "Struct '{}'", dt.inner().name().value()),
		}
	}
}

//--------------------------- BuiltinType -------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
pub enum BuiltinType {
	Float32,
	String,
	Bool,
	Char,
	Array,
	Any,
	None,
}

impl BuiltinType {
	pub fn default_value(&self) -> Value {
		match self {
			BuiltinType::Float32 => Value::from(0_f32),
			BuiltinType::String => Value::from(String::new()),
			BuiltinType::Bool => Value::from(false),
			BuiltinType::Char => Value::from('a'),
			BuiltinType::Array => Value::Array {
				elem_type: DataType::Builtin (BuiltinType::Any),
				values: Rc::new(RefCell::new(Vec::new())),
			},
			BuiltinType::Any => unreachable!(),
			BuiltinType::None => unreachable!(),
		}
	}
}

impl std::fmt::Display for BuiltinType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			BuiltinType::Float32 => write!(f, "f32"),
			BuiltinType::String => write!(f, "str"),
			BuiltinType::Bool => write!(f, "bool"),
			BuiltinType::Char => write!(f, "char"),
			BuiltinType::Array => write!(f, "array"),
			BuiltinType::Any => write!(f, "Any"),
			BuiltinType::None => write!(f, "None"),
		}
	}
}

//--------------------------- DataTypeErr -------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataTypeErr {
	NotDefined {
		name: NameToken,
	},
}

impl std::fmt::Display for DataTypeErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			DataTypeErr::NotDefined { name } => 
				write!(f, "{} dataType '{}' is not defined", 
					if name.is_builtin() { "Builtin" } else { "User-defined" }, 
					name.value()),
		}
	}
}