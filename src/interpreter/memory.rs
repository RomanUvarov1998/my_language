pub struct Memory {
	vars: Vec<VarData>,
}

impl Memory {
	pub fn new() -> Self {
		Self {
			vars: Vec::new(),
		}
	}
	
	pub fn add_variable(&mut self, name: String, var_type: VarType) -> Result<(), VarErr> {
		if let Ok(..) = self.find_var(&name) {
			Err( VarErr::AlreadyExists { name: name.to_string() } )
		} else {
			self.vars.push(VarData::new(name, var_type));
			Ok(())
		}
	}
	
	pub fn set_variable(&mut self, name: &str, value: VarValue) -> Result<(), VarErr> {
		let var = self.find_var_mut(name)?;
		var.set(value)?;
		Ok(())
	}
	
	pub fn get_variable<'mem>(&'mem self, name: &str) -> Result<&'mem VarValue, VarErr> {
		let var = self.find_var(name)?;
		match var.var_value {
			Some(ref value) => Ok(value),
			None => Err( VarErr::NotSet { name: name.to_string() } ),
		}
	}
	
	pub fn find_var<'mem>(&'mem self, name: &str) -> Result<&'mem VarData, VarErr> {
		match self.vars.iter().find(|var| var.name == name) {
			Some(var) => Ok(var),
			None => Err( VarErr::NotDefined { name: name.to_string() } ),
		}
	}
	
	pub fn find_var_mut<'mem>(&'mem mut self, name: &str) -> Result<&'mem mut VarData, VarErr> {
		match self.vars.iter_mut().find(|var| var.name == name) {
			Some(var) => Ok(var),
			None => Err( VarErr::NotDefined { name: name.to_string() } ),
		}
	}
}

#[derive(Debug, Eq, PartialEq)]
pub struct VarData {
	name: String,
	var_value: Option<VarValue>,
	var_type: VarType,
}
impl VarData {
	fn new(name: String, var_type: VarType) -> Self {
		VarData { name, var_value: None, var_type }
	}
	
	fn set(&mut self, new_value: VarValue) -> Result<(), VarErr> {
		match self.var_type {
			VarType::Float32 => match new_value {
				VarValue::Float32 (_) => {
					self.var_value = Some(new_value);
					Ok(())
				},
			}
		}
	}
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum VarType {
	Float32,
}
impl VarType {
	pub fn parse(name: &str) -> Result<Self, VarErr> {
		match name {
			"f32" => Ok( VarType::Float32 ),
			_ => Err( VarErr::UnknownType { name: name.to_string() } ),
		}
	}
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum VarErr {
	NotDefined { name: String },
	NotSet { name: String },
	UnknownType { name: String },
	AlreadyExists { name: String },
	#[allow(unused)]
	WrongType { new_value: VarValue, var_type: VarType },
}

#[cfg(test)]
mod tests {
	use super::*;
	use super::super::*;
	
	#[test]
	pub fn can_run_declare_than_set_variable() {
		let mut int = Interpreter::new();
		
		int.run("var a: f32;").unwrap();
		
		assert_eq!(int.memory.find_var("a").unwrap(), &VarData { 
			name: String::from("a"),
			var_value: None,
			var_type: VarType::Float32,
		} );
		
		int.run("a = 0.3 + 0.5;").unwrap();
		
		assert_eq!(int.memory.find_var("a").unwrap(), &VarData { 
			name: String::from("a"),
			var_value: Some( VarValue::Float32 (0.8_f32) ),
			var_type: VarType::Float32,
		} );
		
		int.run("a = a + 0.5;").unwrap();
		
		assert_eq!(int.memory.find_var("a").unwrap(), &VarData { 
			name: String::from("a"),
			var_value: Some( VarValue::Float32 (1.3_f32) ),
			var_type: VarType::Float32,
		} );
		
		int.run("a = a * 2 + 1.4;").unwrap();
		
		assert_eq!(int.memory.find_var("a").unwrap(), &VarData { 
			name: String::from("a"),
			var_value: Some( VarValue::Float32 (4_f32) ),
			var_type: VarType::Float32,
		} );
	}
}