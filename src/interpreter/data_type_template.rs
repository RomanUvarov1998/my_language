use super::utils::NameToken;
use super::statement::Statement;
use std::collections::HashMap;

//------------------------- DataTypeTemplate -----------------------

pub struct DataTypeTemplate {
	name: &'static str,
	fields: HashMap<String, StructFieldDefTemplate>, // TODO: try using &str
	user_funcs: Vec<UserFuncDefTemplate>,	// TODO: use HashMap for user_funcs and builtin_funcs
}

//------------------------- StructFieldDefTemplate -----------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructFieldDefTemplate {
	name: NameToken,
	data_type_name: NameToken,
}

//------------------------- UserFuncDefTemplate -----------------------

pub struct UserFuncDefTemplate {
	name: NameToken,
	args: Vec<UserFuncArgTemplate>,
	return_type_name: NameToken,
	body: Vec<Statement>,
}

//------------------------- UserFuncArgTemplate -----------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UserFuncArgTemplate {
	name: NameToken,
	data_type_name: NameToken,
}