use super::builtin_func::{BuiltinFuncDef, BuiltinFuncArg, BuiltinFuncBody};
use super::data_type::{DataType, BuiltinType};
use super::utils::NameToken;
use super::value::Value;
use super::struct_def::StructDefErr;

//------------------- PrimitiveTypeMemberFuncsList -----------------------

pub struct PrimitiveTypeMemberBuiltinFuncsList {
	lookup: Vec<Vec<BuiltinFuncDef>>,
}

impl PrimitiveTypeMemberBuiltinFuncsList {
	pub fn new() -> Self {		
		let lookup_float32 = vec![
			BuiltinFuncDef::new(
				"abs",
				vec![
					BuiltinFuncArg::new("self".to_string(), DataType::Builtin (BuiltinType::Float32)),
				],
				Box::new(|args_values: Vec<Value>| -> Option<Value> {
					if let Value::Float32 (val) = &args_values[0] {
						Some( Value::Float32( val.abs() ) )
					} else {
						unreachable!();
					}
				}) as BuiltinFuncBody,
				DataType::Builtin (BuiltinType::Float32)
			),
			BuiltinFuncDef::new(
				"sign",
				vec![
					BuiltinFuncArg::new("self".to_string(), DataType::Builtin (BuiltinType::Float32)),
				],
				Box::new(|args_values: Vec<Value>| -> Option<Value> {
					if let Value::Float32 (val) = &args_values[0] {
						if *val < 0_f32 {
							Some( Value::Float32( -1_f32 ) )
						} else if *val > 0_f32 {
							Some( Value::Float32( 1_f32 ) )
						} else {
							Some( Value::Float32( 0_f32 ) )
						}
					} else {
						unreachable!();
					}
				}) as BuiltinFuncBody,
				DataType::Builtin (BuiltinType::Float32)
			),
			BuiltinFuncDef::new(
				"to_string",
				vec![
					BuiltinFuncArg::new("self".to_string(), DataType::Builtin (BuiltinType::Float32)),
				],
				Box::new(|args_values: Vec<Value>| -> Option<Value> {
					if let Value::Float32 (val) = &args_values[0] {
						Some( Value::from(format!("{}", val)) )
					} else {
						unreachable!();
					}
				}) as BuiltinFuncBody,
				DataType::Builtin (BuiltinType::String)
			),
		];
		
		let lookup_string = vec![
			BuiltinFuncDef::new(
				"len",
				vec![
					BuiltinFuncArg::new("self".to_string(), DataType::Builtin (BuiltinType::String)),
				],
				Box::new(|args_values: Vec<Value>| -> Option<Value> {
					if let Value::String (chars_rc) = &args_values[0] {
						Some( Value::Float32( chars_rc.borrow().len() as f32 ) )
					} else {
						unreachable!();
					}
				}) as BuiltinFuncBody,
				DataType::Builtin (BuiltinType::Float32)
			),
		];
		
		let lookup_bool = vec![
			BuiltinFuncDef::new(
				"to_string",
				vec![
					BuiltinFuncArg::new("self".to_string(), DataType::Builtin (BuiltinType::Bool)),
				],
				Box::new(|args_values: Vec<Value>| -> Option<Value> {
					if let Value::Bool (val) = &args_values[0] {
						match *val {
							true => Some( Value::from("True") ),
							false => Some( Value::from("False") ),
						}
					} else {
						unreachable!();
					}
				}) as BuiltinFuncBody,
				DataType::Builtin (BuiltinType::String)
			),
		];
		
		let lookup_char = vec![
			BuiltinFuncDef::new(
				"to_string",
				vec![
					BuiltinFuncArg::new("self".to_string(), DataType::Builtin (BuiltinType::Bool)),
				],
				Box::new(|args_values: Vec<Value>| -> Option<Value> {
					if let Value::Char (val) = &args_values[0] {
						Some( Value::from(val.to_string()) )
					} else {
						unreachable!();
					}
				}) as BuiltinFuncBody,
				DataType::Builtin (BuiltinType::String)
			),
		];
		
		let lookup_array = vec![
			BuiltinFuncDef::new(
				"to_string",
				vec![
					BuiltinFuncArg::new("self".to_string(), DataType::Builtin (BuiltinType::Array)),
				],
				Box::new(|args_values: Vec<Value>| -> Option<Value> {
					if let Value::Array { elem_type, values } = &args_values[0] {
						let mut s: String = format!("Array of '{}' [", elem_type);
						
						let mut is_first = true;
						for v in values.borrow().iter() {
							if !is_first { s.push_str(", "); }
							is_first = false;
							s.push_str(&format!("{}", v));
						}
						s.push_str("]");
				
						Some( Value::from(s) )
					} else {
						unreachable!();
					}
				}) as BuiltinFuncBody,
				DataType::Builtin (BuiltinType::String)
			),
			BuiltinFuncDef::new(
				"len",
				vec![
					BuiltinFuncArg::new("self".to_string(), DataType::Builtin (BuiltinType::Array)),
				],
				Box::new(|args_values: Vec<Value>| -> Option<Value> {
					if let Value::Array { values, .. } = &args_values[0] {
						Some( Value::from(values.borrow().len() as f32) )
					} else {
						unreachable!();
					}
				}) as BuiltinFuncBody,
				DataType::Builtin (BuiltinType::Float32)
			),
			BuiltinFuncDef::new(
				"add",
				vec![
					BuiltinFuncArg::new("self".to_string(), DataType::Builtin (BuiltinType::Array)),
					BuiltinFuncArg::new("item".to_string(), DataType::Builtin (BuiltinType::Any)),
				],
				Box::new(|args_values: Vec<Value>| -> Option<Value> {
					let item: Value = args_values[1].clone();
					if let Value::Array { values, .. } = &args_values[0] {
						values.borrow_mut().push(item);
						None
					} else {
						unreachable!();
					}
				}) as BuiltinFuncBody,
				DataType::Builtin (BuiltinType::None)
			),
			BuiltinFuncDef::new(
				"get",
				vec![
					BuiltinFuncArg::new("self".to_string(), DataType::Builtin (BuiltinType::Array)),
					BuiltinFuncArg::new("ind".to_string(), DataType::Builtin (BuiltinType::Float32)),
				],
				Box::new(|args_values: Vec<Value>| -> Option<Value> {
					let ind: f32 = args_values[1].unwrap_f32();
					let ind: usize = (ind.abs().floor() * ind.signum()) as usize;
					if let Value::Array { values, .. } = &args_values[0] {
						Some(values.borrow()[ind].clone())
					} else {
						unreachable!();
					}
				}) as BuiltinFuncBody,
				DataType::Builtin (BuiltinType::Any)
			),
			BuiltinFuncDef::new(
				"set",
				vec![
					BuiltinFuncArg::new("self".to_string(), DataType::Builtin (BuiltinType::Array)),
					BuiltinFuncArg::new("ind".to_string(), DataType::Builtin (BuiltinType::Float32)),
					BuiltinFuncArg::new("item".to_string(), DataType::Builtin (BuiltinType::Any)),
				],
				Box::new(|args_values: Vec<Value>| -> Option<Value> {
					let ind: f32 = args_values[1].unwrap_f32();
					let ind: usize = (ind.abs().floor() * ind.signum()) as usize;
					let item: Value = args_values[2].clone();
					if let Value::Array { values, .. } = &args_values[0] {
						values.borrow_mut()[ind] = item;
						None
					} else {
						unreachable!();
					}
				}) as BuiltinFuncBody,
				DataType::Builtin (BuiltinType::None)
			),
			BuiltinFuncDef::new(
				"remove",
				vec![
					BuiltinFuncArg::new("self".to_string(), DataType::Builtin (BuiltinType::Array)),
					BuiltinFuncArg::new("ind".to_string(), DataType::Builtin (BuiltinType::Float32)),
				],
				Box::new(|args_values: Vec<Value>| -> Option<Value> {
					let ind: f32 = args_values[1].unwrap_f32();
					let ind: usize = (ind.abs().floor() * ind.signum()) as usize;
					if let Value::Array { values, .. } = &args_values[0] {
						Some(values.borrow_mut().remove(ind))
					} else {
						unreachable!();
					}
				}) as BuiltinFuncBody,
				DataType::Builtin (BuiltinType::Any)
			),
			BuiltinFuncDef::new(
				"insert",
				vec![
					BuiltinFuncArg::new("self".to_string(), DataType::Builtin (BuiltinType::Array)),
					BuiltinFuncArg::new("ind".to_string(), DataType::Builtin (BuiltinType::Float32)),
					BuiltinFuncArg::new("item".to_string(), DataType::Builtin (BuiltinType::Any)),
				],
				Box::new(|args_values: Vec<Value>| -> Option<Value> {
					let ind: f32 = args_values[1].unwrap_f32();
					let ind: usize = (ind.abs().floor() * ind.signum()) as usize;
					let item: Value = args_values[2].clone();
					if let Value::Array { values, .. } = &args_values[0] {
						values.borrow_mut().insert(ind, item);
						None
					} else {
						unreachable!();
					}
				}) as BuiltinFuncBody,
				DataType::Builtin (BuiltinType::None)
			),
		];
		
		let lookup_any = Vec::new();
		
		let lookup_none = vec![
			BuiltinFuncDef::new(
				"to_string",
				vec![
					BuiltinFuncArg::new("self".to_string(), DataType::Builtin (BuiltinType::None)),
				],
				Box::new(|args_values: Vec<Value>| -> Option<Value> {
					if let Value::None = &args_values[0] {
						Some( Value::from("None") )
					} else {
						unreachable!();
					}
				}) as BuiltinFuncBody,
				DataType::Builtin (BuiltinType::String)
			),
		];
		
		let lookup = vec![
			lookup_float32,
			lookup_string,
			lookup_bool,
			lookup_char,
			lookup_array,
			lookup_any,
			lookup_none,
		];
		
		Self {
			lookup,
		}
	}
	
	pub fn find_func(&self, data_type: BuiltinType, name: &NameToken) -> Result<BuiltinFuncDef, StructDefErr> {
		match self.lookup[data_type as usize].iter().find(|fd| fd.name() == name.value()) {
			Some(func_def) => Ok(func_def.clone()),
			None => Err( StructDefErr::BuiltinMemberFuncIsNotDefined { name: name.clone() } )
		}
	}
}

//------------------- Tests ----------------------------------------------

#[cfg(test)]
mod tests {
	use super::PrimitiveTypeMemberBuiltinFuncsList;
	use super::super::data_type::BuiltinType;
	
	#[test]
	fn test_loopkup_table() {
		let list = PrimitiveTypeMemberBuiltinFuncsList::new();
		
		assert_eq!(list.lookup[BuiltinType::Float32 as usize].len(), 3);
		assert_eq!(list.lookup[BuiltinType::Float32 as usize][0].name(), "abs");
		assert_eq!(list.lookup[BuiltinType::Float32 as usize][1].name(), "sign");
		assert_eq!(list.lookup[BuiltinType::Float32 as usize][2].name(), "to_string");
		
		assert_eq!(list.lookup[BuiltinType::String as usize].len(), 1);
		assert_eq!(list.lookup[BuiltinType::String as usize][0].name(), "len");
		
		assert_eq!(list.lookup[BuiltinType::Bool as usize].len(), 1);
		assert_eq!(list.lookup[BuiltinType::Bool as usize][0].name(), "to_string");
		
		assert_eq!(list.lookup[BuiltinType::Char as usize].len(), 1);
		assert_eq!(list.lookup[BuiltinType::Char as usize][0].name(), "to_string");
		
		assert_eq!(list.lookup[BuiltinType::Array as usize].len(), 7);
		assert_eq!(list.lookup[BuiltinType::Array as usize][0].name(), "to_string");
		assert_eq!(list.lookup[BuiltinType::Array as usize][1].name(), "len");
		assert_eq!(list.lookup[BuiltinType::Array as usize][2].name(), "add");
		assert_eq!(list.lookup[BuiltinType::Array as usize][3].name(), "get");
		assert_eq!(list.lookup[BuiltinType::Array as usize][4].name(), "set");
		assert_eq!(list.lookup[BuiltinType::Array as usize][5].name(), "remove");
		assert_eq!(list.lookup[BuiltinType::Array as usize][6].name(), "insert");
		
		assert_eq!(list.lookup[BuiltinType::Any as usize].len(), 0);
		
		assert_eq!(list.lookup[BuiltinType::None as usize].len(), 1);
		assert_eq!(list.lookup[BuiltinType::None as usize][0].name(), "to_string");
	}
}