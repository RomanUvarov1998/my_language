use super::utils::NameToken;
use super::value::Value;
use super::data_type::DataType;
use super::context::Context;
use super::InterpErr;

//-------------------- VarData --------------

#[derive(Debug, Eq, PartialEq)]
pub struct VarData {
	name: NameToken,
	var_value: Value,
	data_type: DataType,
}

impl VarData {
	pub fn new_with_value(name: NameToken, data_type: DataType, var_value: Value) -> Result<Self, VarErr>{
		if data_type == var_value.get_type() {
			Ok( VarData {
				name,
				var_value,
				data_type,
			} )
		} else {
			Err( VarErr::WrongValue { 
					new_var_value: var_value, 
					variable_type: data_type,
					var_name: name,
				}.into() )
		}
	}
	
	pub fn set(&mut self, new_var_value: Value) -> Result<(), VarErr> {
		if self.data_type == new_var_value.get_type() {
			self.var_value = new_var_value;
			Ok(())
		} else {
			Err( VarErr::WrongValue { 
					new_var_value, 
					variable_type: self.data_type.clone(),
					var_name: self.name.clone(),
				}.into() )
		}
	}
	
	pub fn get_value(&self) -> &Value {
		&self.var_value
	}
	
	pub fn get_type(&self) -> &DataType {
		&self.data_type
	}
	
	pub fn get_name(&self) -> &NameToken {
		&self.name
	}	
}

//-------------------- VarErr --------------

#[derive(Debug, PartialEq, Eq)]
pub enum VarErr {
	NotDefined { name: NameToken },
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
			VarErr::AlreadyExists { name } => 
				write!(f, "Variable already exists '{}'", &name),
			VarErr::WrongValue { new_var_value, variable_type, .. } =>
				write!(f, "Wrong value '{:?}' for type '{:?}'", new_var_value, variable_type),
			VarErr::WrongType { value_data_type, variable_type, .. } =>
				write!(f, "Incompatible types: '{:?}' and '{:?}'", variable_type, value_data_type),
		}
	}
}
