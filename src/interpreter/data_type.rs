use super::value::Value;

//--------------------------- DataType -------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataType {
	Primitive (Primitive),
}

impl DataType {	
	pub fn default_value(&self) -> Value {
		match self {
			DataType::Primitive (dt) => dt.default_value(),
		}
	}
}

impl std::fmt::Display for DataType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			DataType::Primitive (dt) => write!(f, "{}", dt),
		}
	}
}

//--------------------------- Primitive -------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
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