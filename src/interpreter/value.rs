use super::data_type::{DataType, Primitive};

#[derive(Debug, Clone)]
pub enum Value {
	Float32 (f32),
	String (String),
	Bool (bool),
	Struct {
		data_type: DataType,
		fields: Vec<Value>,
	},
	None,
}

impl Value {
	pub fn get_type(&self) -> DataType {
		match self {
			Value::Float32 (_) => DataType::Primitive (Primitive::Float32),
			Value::String (_) => DataType::Primitive (Primitive::String),
			Value::Bool (_) => DataType::Primitive (Primitive::Bool),
			Value::Struct { data_type, .. } => data_type.clone(),
			Value::None => DataType::Primitive (Primitive::None),
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
			Value::Struct { data_type: data_type1, .. } => match other {
				Value::Struct { data_type: data_type2, .. } => data_type1 == data_type2,
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
			Value::String (v) => write!(f, "{}", v),
			Value::Bool (v) => if *v {
				write!(f, "True")
			} else {
				write!(f, "False")
			},
			Value::Struct { data_type, .. } => write!(f, "Struct '{}'", data_type),
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
		Value::String(val)
	}
}

impl From<&str> for Value {
	fn from(val: &str) -> Self {
		Value::String(val.to_string())
	}
}

impl From<bool> for Value {
	fn from(val: bool) -> Self {
		Value::Bool(val)
	}
}
