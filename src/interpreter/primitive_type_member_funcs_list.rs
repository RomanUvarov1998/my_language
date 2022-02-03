use super::builtin_func::{BuiltinFuncDef, BuiltinFuncArg, BuiltinFuncBody, BuiltinFuncErr};
use super::data_type::{DataType, Primitive};
use super::utils::NameToken;
use super::value::Value;
use super::struct_def::StructDefErr;

//------------------- PrimitiveTypeMemberFuncsList -----------------------

pub struct PrimitiveTypeMemberFuncsList {
	lookup: Vec<Vec<BuiltinFuncDef>>,
}

impl PrimitiveTypeMemberFuncsList {
	pub fn new() -> Self {		
		let lookup_float32 = vec![
			BuiltinFuncDef::new(
				"abs",
				vec![
					BuiltinFuncArg::new("value".to_string(), DataType::Primitive (Primitive::Float32)),
				],
				Box::new(|args_values: Vec<Value>| -> Option<Value> {
					if let Value::Float32 (val) = &args_values[0] {
						Some( Value::Float32( val.abs() ) )
					} else {
						unreachable!();
					}
				}) as BuiltinFuncBody,
				DataType::Primitive (Primitive::Float32)
			),
			BuiltinFuncDef::new(
				"sign",
				vec![
					BuiltinFuncArg::new("value".to_string(), DataType::Primitive (Primitive::Float32)),
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
				DataType::Primitive (Primitive::Float32)
			),
			BuiltinFuncDef::new(
				"to_string",
				vec![
					BuiltinFuncArg::new("value".to_string(), DataType::Primitive (Primitive::Float32)),
				],
				Box::new(|args_values: Vec<Value>| -> Option<Value> {
					if let Value::Float32 (val) = &args_values[0] {
						Some( Value::String( format!("{}", val) ) )
					} else {
						unreachable!();
					}
				}) as BuiltinFuncBody,
				DataType::Primitive (Primitive::String)
			),
		];
		
		let lookup_string = vec![
			BuiltinFuncDef::new(
				"len",
				vec![
					BuiltinFuncArg::new("value".to_string(), DataType::Primitive (Primitive::String)),
				],
				Box::new(|args_values: Vec<Value>| -> Option<Value> {
					if let Value::String (val) = &args_values[0] {
						Some( Value::Float32( val.len() as f32 ) )
					} else {
						unreachable!();
					}
				}) as BuiltinFuncBody,
				DataType::Primitive (Primitive::Float32)
			),
		];
		
		let lookup_bool = vec![
			BuiltinFuncDef::new(
				"to_string",
				vec![
					BuiltinFuncArg::new("value".to_string(), DataType::Primitive (Primitive::Bool)),
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
				DataType::Primitive (Primitive::String)
			),
		];
		
		let lookup_any = vec![
			BuiltinFuncDef::new(
				"to_string",
				vec![
					BuiltinFuncArg::new("value".to_string(), DataType::Primitive (Primitive::Bool)),
				],
				Box::new(|args_values: Vec<Value>| -> Option<Value> {
					if let Value::Bool (val) = &args_values[0] {
						Some( Value::from("Any") )
					} else {
						unreachable!();
					}
				}) as BuiltinFuncBody,
				DataType::Primitive (Primitive::String)
			),
		];
		
		let lookup_none = vec![
			BuiltinFuncDef::new(
				"to_string",
				vec![
					BuiltinFuncArg::new("value".to_string(), DataType::Primitive (Primitive::Bool)),
				],
				Box::new(|args_values: Vec<Value>| -> Option<Value> {
					if let Value::Bool (val) = &args_values[0] {
						Some( Value::from("None") )
					} else {
						unreachable!();
					}
				}) as BuiltinFuncBody,
				DataType::Primitive (Primitive::String)
			),
		];
		
		let lookup = vec![
			lookup_float32,
			lookup_string,
			lookup_bool,
			lookup_any,
			lookup_none,
		];
		
		Self {
			lookup,
		}
	}
	
	pub fn find_func(&self, data_type: Primitive, name: &NameToken) -> Result<&BuiltinFuncDef, StructDefErr> {
		match self.lookup[data_type as usize].iter().find(|fd| fd.name() == name.value()) {
			Some(func_def) => Ok(func_def),
			None => Err( StructDefErr::BuiltinMemberFuncIsNotDefined { name: name.value().to_string() } )
		}
	}
}

//------------------- Tests ----------------------------------------------

#[cfg(test)]
mod tests {
	use super::PrimitiveTypeMemberFuncsList;
	use super::super::data_type::Primitive;
	
	#[test]
	fn test_loopkup_table() {
		let list = PrimitiveTypeMemberFuncsList::new();
		
		assert_eq!(list.lookup[Primitive::Float32 as usize].len(), 3);
		assert_eq!(list.lookup[Primitive::Float32 as usize][0].name(), "abs");
		assert_eq!(list.lookup[Primitive::Float32 as usize][1].name(), "sign");
		assert_eq!(list.lookup[Primitive::Float32 as usize][2].name(), "to_string");
		
		assert_eq!(list.lookup[Primitive::String as usize].len(), 1);
		assert_eq!(list.lookup[Primitive::String as usize][0].name(), "len");
		
		assert_eq!(list.lookup[Primitive::Bool as usize].len(), 1);
		assert_eq!(list.lookup[Primitive::Bool as usize][0].name(), "to_string");
		
		assert_eq!(list.lookup[Primitive::Any as usize].len(), 1);
		assert_eq!(list.lookup[Primitive::Any as usize][0].name(), "to_string");
		
		assert_eq!(list.lookup[Primitive::None as usize].len(), 1);
		assert_eq!(list.lookup[Primitive::None as usize][0].name(), "to_string");
	}
}