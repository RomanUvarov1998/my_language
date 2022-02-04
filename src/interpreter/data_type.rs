use super::value::Value;
use super::struct_def::{StructDef, StructDefErr};
use super::utils::NameToken;
use super::context::Context;
use super::builtin_func::BuiltinFuncDef;

//--------------------------- DataType -------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataType {
	Primitive (Primitive),
	Complex (StructDef),
}

impl DataType {
	pub fn primitive_from_name(name: &NameToken) -> Option<Self> {
		match name.value() {
			"f32" => Some( DataType::Primitive (Primitive::Float32) ),
			"str" => Some( DataType::Primitive (Primitive::String) ),
			"bool" => Some( DataType::Primitive (Primitive::Bool) ),
			_ => None,
		}
	}
	
	pub fn default_value(&self) -> Value {
		match self {
			DataType::Primitive (dt) => dt.default_value(),
			DataType::Complex (dt) => todo!(), // TODO: do not use this function for check purposes
		}
	}
	
	pub fn find_member_builtin_func<'context>(
		&self, 
		func_name: &'context NameToken, 
		context: &'context Context
	) -> Result<&'context BuiltinFuncDef, StructDefErr> {
		match self {
			DataType::Primitive (dt) => context.find_member_builtin_func_def(*dt, func_name),
			DataType::Complex (_) => todo!(),
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
	Any,
	None,
}

impl Primitive {
	pub fn default_value(&self) -> Value {
		match self {
			Primitive::Float32 => Value::from(0_f32),
			Primitive::String => Value::from(String::new()),
			Primitive::Bool => Value::from(false),
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
			Primitive::Any => write!(f, "Any"),
			Primitive::None => write!(f, "None"),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataTypeErr {
	NotDefined {
		name: NameToken,
	}
}