#[derive(Debug, Eq, PartialEq)]
pub struct VarData {
	name: String,
	var_value: Option<VarValue>,
	data_type: DataType,
}
impl VarData {
	pub fn new(name: String, data_type: DataType) -> Self {
		VarData { name, var_value: None, data_type }
	}
	
	#[allow(unused)]
	pub fn with_value(mut self, new_value: VarValue) -> Result<Self, VarErr>{
		self.set(new_value)?;
		Ok(self)
	}
	
	pub fn set(&mut self, new_value: VarValue) -> Result<(), VarErr> {
		match self.data_type {
			DataType::Float32 => match new_value {
				VarValue::Float32 (_) => {
					self.var_value = Some(new_value);
					Ok(())
				},
			}
		}
	}
	
	pub fn get_value(&self) -> Option<&VarValue> {
		self.var_value.as_ref()
	}
	
	pub fn get_name(&self) -> &str {
		&self.name
	}	
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum DataType {
	Float32,
}
impl DataType {
	pub fn parse(name: &str) -> Result<Self, VarErr> {
		match name {
			"f32" => Ok( DataType::Float32 ),
			_ => Err( VarErr::UnknownType { name: name.to_string() } ),
		}
	}
}

#[derive(Debug, Clone)]
pub enum VarValue {
	Float32 (f32),
}
impl Eq for VarValue {}
impl PartialEq for VarValue {
	fn eq(&self, other: &Self) -> bool {
		match self {
			VarValue::Float32 (f1) => match other {
				VarValue::Float32 (f2) => (f1 - f2).abs() <= std::f32::EPSILON,
			},
		}
	}
}
impl VarValue {
	pub fn get_type(&self) -> DataType {
		match self {
			VarValue::Float32 (_) => DataType::Float32,
		}
	}
}

#[derive(Debug, PartialEq, Eq)]
pub enum VarErr {
	NotDefined { name: String },
	NotSet { name: String },
	UnknownType { name: String },
	AlreadyExists { name: String },
	#[allow(unused)]
	WrongType { new_value: VarValue, data_type: DataType },
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
			VarErr::WrongType { new_value, data_type } =>
				write!(f, "Wrong value '{:?}' for type '{:?}'", new_value, data_type),
		}
	}
}