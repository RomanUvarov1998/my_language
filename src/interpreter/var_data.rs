use super::utils::NameToken;
use super::value::Value;
use super::data_type::DataType;

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
					variable_type: self.data_type,
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

//-------------------- VarErr --------------

#[derive(Debug, PartialEq, Eq)]
pub enum VarErr {
	NotDefined { name: NameToken },
	NotSet { name: NameToken },
	UnknownType { name: NameToken },
	AlreadyExists { name: NameToken },
	WrongValue { 
		new_var_value: Value, 
		variable_type: DataType,
		var_name: NameToken,
	},
	WrongType { 
		value_data_type: DataType, 
		variable_type: DataType,
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
			VarErr::WrongValue { new_var_value, variable_type, .. } =>
				write!(f, "Wrong value '{:?}' for type '{:?}'", new_var_value, variable_type),
			VarErr::WrongType { value_data_type, variable_type, .. } =>
				write!(f, "Incompatible types: '{:?}' and '{:?}'", variable_type, value_data_type),
		}
	}
}
