use super::utils::NameToken;

//-------------------- VarData --------------

#[derive(Debug, Eq, PartialEq)]
pub struct VarData {
	name: NameToken,
	var_value: Option<Value>,
	data_type: DataType,
}

impl VarData {
	pub fn new_uninit(name: NameToken, data_type: DataType) -> Self {
		VarData { name, var_value: None, data_type }
	}
	
	pub fn new_with_value(name: NameToken, data_type: DataType, new_value: Value) -> Result<Self, VarErr>{
		let mut vd: VarData = VarData::new_uninit(name, data_type);
		vd.set(new_value)?;
		Ok(vd)
	}
	
	pub fn set(&mut self, new_var_value: Value) -> Result<(), VarErr> {
		if self.data_type == new_var_value.get_type() {
			self.var_value = Some(new_var_value);
			Ok(())
		} else {
			Err( VarErr::WrongValue { 
					new_var_value, 
					var_data_type: self.data_type,
					var_name: self.name.clone(),
				} )
		}
	}
	
	pub fn get_value(&self) -> Option<&Value> {
		self.var_value.as_ref()
	}
	
	pub fn get_type(&self) -> DataType {
		self.data_type
	}
	
	pub fn get_name(&self) -> &NameToken {
		&self.name
	}	
}

//-------------------- DataType --------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataType {
	Float32,
	String,
	Bool,
	Untyped,
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
			DataType::Untyped => unreachable!(),
		}
	}
}

impl std::fmt::Display for DataType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			DataType::Float32 => write!(f, "f32"),
			DataType::String => write!(f, "str"),
			DataType::Bool => write!(f, "bool"),
			DataType::Untyped => write!(f, "Untyped"),
		}
	}
}

//-------------------- Value --------------

#[derive(Debug, Clone)]
pub enum Value {
	Float32 (f32),
	String (String),
	Bool (bool),
	// TODO: add struct
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
		}
	}
}
impl Value {
	pub fn get_type(&self) -> DataType {
		match self {
			Value::Float32 (_) => DataType::Float32,
			Value::String (_) => DataType::String,		
			Value::Bool (_) => DataType::Bool,		
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

//-------------------- VarErr --------------

#[derive(Debug, PartialEq, Eq)]
pub enum VarErr {
	NotDefined { name: NameToken },
	NotSet { name: NameToken },
	UnknownType { name: NameToken },
	AlreadyExists { name: NameToken },
	WrongValue { 
		new_var_value: Value, 
		var_data_type: DataType,
		var_name: NameToken,
	},
	WrongType { 
		value_data_type: DataType, 
		var_data_type: DataType,
		var_name: NameToken,
	},
}

impl std::fmt::Display for VarErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			VarErr::NotDefined { name } => write!(f, "Variable '{}' is not defined", &name),
			VarErr::NotSet { name } => 
				write!(f, "Variable '{}' is not set", &name),
			VarErr::UnknownType { name } => 
				write!(f, "Unknown type '{}'", &name),
			VarErr::AlreadyExists { name } => 
				write!(f, "Variable already exists '{}'", &name),
			VarErr::WrongValue { new_var_value, var_data_type, .. } =>
				write!(f, "Wrong value '{:?}' for type '{:?}'", new_var_value, var_data_type),
			VarErr::WrongType { value_data_type, var_data_type, .. } =>
				write!(f, "Incompatible types: '{:?}' and '{:?}'", var_data_type, value_data_type),
		}
	}
}

//-------------------- Tests --------------

#[cfg(test)]
mod tests {
	//#[test]
	//fn 
}