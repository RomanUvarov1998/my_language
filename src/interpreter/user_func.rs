use super::data_type::{DataType, Primitive};
use super::value::Value;
use super::InterpErr;
use super::utils::{CodePos, NameToken};
use super::expr::Expr;
use super::statement::ReturningBody;
use super::context::Context;

//------------------------- UserFuncDef -----------------------

#[derive(Debug, Clone)]
pub struct UserFuncDef {
	name: NameToken,
	args: Vec<UserFuncArg>,
	return_type: DataType,
	body: ReturningBody,
}

impl UserFuncDef {
	pub fn new(name: NameToken, args: Vec<UserFuncArg>, return_type: DataType, body: ReturningBody) -> Self {
		Self {
			name,
			args,
			return_type,
			body,
		}
	}
	
	pub fn check_args(&self, args_exprs: &Vec<Expr>, check_context: &Context) -> Result<(), InterpErr> {
		if self.args.len() != args_exprs.len() {
			return Err(UserFuncErr::ArgsCnt {
				func_signature: format!("{}", self),
				func_name_pos: self.name.pos(),
				actual_cnt: self.args.len(),
				given_cnt: args_exprs.len(),
			}.into());
		}
						
		let args_data_types_result: Result<Vec<DataType>, InterpErr> = args_exprs.iter()
			.map(|expr| expr.check_and_calc_data_type(check_context))
			.collect();
			
		let args_data_types: Vec<DataType> = args_data_types_result?;
		
		for i in 0..self.args.len() {
			if !self.args[i].type_check(&args_data_types[i]) {
				return Err(UserFuncErr::ArgType {
					func_signature: format!("{}", self),
					arg_name: self.args[i].name.clone(),
					arg_expr_pos: args_exprs[i].pos(),
					actual_type: self.args[i].data_type.clone(),
					given_type: args_data_types[i].clone(),
				}.into());
			}
		}
		
		Ok(())
	}
	
	pub fn call(&self, context: &mut Context) -> Option<Value> {
		self.body.run(context)
	}
	
	pub fn args(&self) -> &Vec<UserFuncArg> {
		&self.args
	}
	
	pub fn get_name(&self) -> &NameToken {
		&self.name
	}

	pub fn return_type(&self) -> &DataType {
		&self.return_type
	}
}

impl std::fmt::Display for UserFuncDef {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "f {}(", self.name)?;
		
		let mut need_comma_before = false;
		for arg in self.args.iter() {
			if need_comma_before {
				write!(f, ", ")?; 
			}
			write!(f, "{}: {}", arg.name, arg.data_type)?; 
			if !need_comma_before {
				need_comma_before = true;
			}
		}
		
		match &self.return_type {
			DataType::Primitive (Primitive::None) => write!(f, ")"),
			dt @ _ => write!(f, ") -> {}", dt),
		}
	}
}

//------------------------- UserFuncArg -----------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UserFuncArg {
	name: NameToken,
	data_type: DataType,
}

impl UserFuncArg {
	pub fn new(name: NameToken, data_type: DataType) -> Self {
		Self {
			name,
			data_type,
		}
	}
	
	pub fn name(&self) -> &NameToken {
		&self.name
	}
	
	pub fn data_type(&self) -> &DataType {
		&self.data_type
	}

	pub fn type_check(&self, data_type: &DataType) -> bool {
		match &self.data_type {
			DataType::Primitive (Primitive::Any) => true,
			_ => self.data_type.eq(data_type),
		}
	}
}

//------------------------- UserFuncErr -----------------------

#[derive(Debug, PartialEq, Eq)]
pub enum UserFuncErr {
	ArgsCnt {
		func_signature: String,
		func_name_pos: CodePos,
		actual_cnt: usize,
		given_cnt: usize,
	},
	ArgType {
		func_signature: String,
		arg_name: NameToken,
		arg_expr_pos: CodePos,
		actual_type: DataType,
		given_type: DataType,
	},
	NotDefined {
		name: NameToken,
	},
	AlreadyExists {
		name: NameToken, 
	},
}

impl std::fmt::Display for UserFuncErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			UserFuncErr::ArgsCnt { func_signature, actual_cnt, given_cnt, .. } =>
				write!(f, "Wrong arguments count for user-defined function:\n{}\n{} expected but {} found", func_signature, actual_cnt, given_cnt),
			UserFuncErr::ArgType { func_signature, arg_name, actual_type, given_type, .. } =>
				write!(f, "Wrong type of argument '{}' for user-defined function:\n{}\n'{}' expected but '{}' found", arg_name, func_signature, actual_type, given_type),
			UserFuncErr::NotDefined { name } =>
				write!(f, "User-defined function {} is not defined", name.value()),
			UserFuncErr::AlreadyExists { name } => write!(f, "User-defined function {} already defined", name.value()),
		}
	}
}