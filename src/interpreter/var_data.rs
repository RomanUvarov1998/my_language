#[derive(Debug, Eq, PartialEq)]
pub struct VarData {
	name: String,
	var_value: Option<Value>,
	data_type: DataType,
}
impl VarData {
	pub fn new(name: String, data_type: DataType) -> Self {
		VarData { name, var_value: None, data_type }
	}
	
	#[allow(unused)]
	pub fn with_value(mut self, new_value: Value) -> Result<Self, VarErr>{
		self.set(new_value)?;
		Ok(self)
	}
	
	pub fn set(&mut self, new_var_value: Value) -> Result<(), VarErr> {
		if self.data_type == new_var_value.get_type() {
			self.var_value = Some(new_var_value);
			Ok(())
		} else {
			Err( VarErr::WrongType { 
					new_var_value, 
					var_data_type: self.data_type 
				} )
		}
	}
	
	pub fn get_value(&self) -> Option<&Value> {
		self.var_value.as_ref()
	}
	
	pub fn get_name(&self) -> &str {
		&self.name
	}	
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum DataType {
	Float32,
	String,
}
impl DataType {
	pub fn parse(name: &str) -> Result<Self, VarErr> {
		match name {
			"f32" => Ok( DataType::Float32 ),
			"str" => Ok( DataType::String ),
			_ => Err( VarErr::UnknownType { name: name.to_string() } ),
		}
	}
}

#[derive(Debug, Clone)]
pub enum Value {
	Float32 (f32),
	String (String),
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
		}
	}
}
impl Value {
	pub fn get_type(&self) -> DataType {
		match self {
			Value::Float32 (_) => DataType::Float32,
			Value::String (_) => DataType::String,		
		}
	}
}

#[derive(Debug, PartialEq, Eq)]
pub enum VarErr {
	NotDefined { name: String },
	NotSet { name: String },
	UnknownType { name: String },
	AlreadyExists { name: String },
	WrongType { new_var_value: Value, var_data_type: DataType },
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
				write!(f, "Already exists '{}'", &name),
			VarErr::WrongType { new_var_value, var_data_type } =>
				write!(f, "Wrong value '{:?}' for type '{:?}'", new_var_value, var_data_type),
		}
	}
}