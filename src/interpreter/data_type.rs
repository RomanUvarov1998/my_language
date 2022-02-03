use super::utils::NameToken;
use super::var_data::VarErr;
use super::value::Value;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataType {
	Float32,
	String,
	Bool,
	Any,
	None,
}

impl DataType {
	pub fn parse(name: &NameToken) -> Result<Self, VarErr> {
		match name.value() {
			"f32" => Ok( DataType::Float32 ),
			"str" => Ok( DataType::String ),
			"bool" => Ok( DataType::Bool ),
			_ => Err( VarErr::UnknownType { name: name.clone() } ),
		}
	}
	
	pub fn default_value(&self) -> Value {
		match self {
			DataType::Float32 => Value::from(0_f32),
			DataType::String => Value::from(String::new()),
			DataType::Bool => Value::from(false),
			DataType::Any => unreachable!(),
			DataType::None => unreachable!(),
		}
	}
}

impl std::fmt::Display for DataType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			DataType::Float32 => write!(f, "f32"),
			DataType::String => write!(f, "str"),
			DataType::Bool => write!(f, "bool"),
			DataType::Any => write!(f, "Any"),
			DataType::None => write!(f, "None"),
		}
	}
}
