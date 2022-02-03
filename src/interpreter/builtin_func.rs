use super::data_type::DataType;
use super::value::Value;
use super::InterpErr;
use super::utils::{CodePos, NameToken};
use super::expr::Expr;
use super::context::Context;

//------------------------- BuiltinFuncDef -----------------------

pub type BuiltinFuncBody = Box<dyn Fn(Vec<Value>) -> Option<Value>>;

pub struct BuiltinFuncDef {
	name: &'static str,
	args: Vec<BuiltinFuncArg>,
	body: BuiltinFuncBody,
	return_type: DataType,
}

impl BuiltinFuncDef {
	pub fn new(name: &'static str, args: Vec<BuiltinFuncArg>, body: BuiltinFuncBody, return_type: DataType) -> Self {
		Self {
			name,
			args,
			body,
			return_type,
		}
	}
	
	pub fn check_args(&self, func_name: &NameToken, args_exprs: &Vec<Expr>, check_context: &Context) -> Result<(), InterpErr> {
		if self.args.len() != args_exprs.len() {
			return Err( InterpErr::from( BuiltinFuncErr::ArgsCnt {
				func_signature: format!("{}", self),
				func_name_pos: func_name.pos(),
				actual_cnt: self.args.len(),
				given_cnt: args_exprs.len(),
			} ) );
		}
						
		let args_data_types_result: Result<Vec<DataType>, InterpErr> = args_exprs.iter()
			.map(|expr| expr.check_and_calc_data_type(check_context))
			.collect();
			
		let args_data_types: Vec<DataType> = args_data_types_result?;
		
		for i in 0..self.args.len() {
			if !self.args[i].type_check(args_data_types[i]) {
				return Err( InterpErr::from( BuiltinFuncErr::ArgType {
					func_signature: format!("{}", self),
					arg_name: self.args[i].name.clone(),
					arg_expr_pos: args_exprs[i].pos(),
					actual_type: self.args[i].data_type,
					given_type: args_data_types[i],
				} ) );
			}
		}
		
		Ok(())
	}
	
	pub fn get_name(&self) -> &'static str {
		self.name
	}
	
	pub fn return_type(&self) -> DataType {
		self.return_type
	}
	
	pub fn call(&self, args_values: Vec<Value>) -> Option<Value> {
		(self.body)(args_values)
	}
}

impl std::fmt::Display for BuiltinFuncDef {
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
		write!(f, ") -> {}", self.return_type)
	}
}

pub struct BuiltinFuncArg {
	name: String,
	data_type: DataType,
}

impl BuiltinFuncArg {
	pub fn new(name: String, data_type: DataType) -> Self {
		Self {
			name,
			data_type,
		}
	}
	
	pub fn type_check(&self, data_type: DataType) -> bool {
		match self.data_type {
			DataType::Any => true,
			_ => self.data_type == data_type,
		}
	}
}

//------------------------- BuiltinFuncErr -----------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuiltinFuncErr {
	ArgsCnt {
		func_signature: String,
		func_name_pos: CodePos,
		actual_cnt: usize,
		given_cnt: usize,
	},
	ArgType {
		func_signature: String,
		arg_name: String,
		arg_expr_pos: CodePos,
		actual_type: DataType,
		given_type: DataType,
	},
	NotDefined {
		name_pos: CodePos,
	},
}

impl std::fmt::Display for BuiltinFuncErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			BuiltinFuncErr::ArgsCnt { func_signature, actual_cnt, given_cnt, .. } =>
				write!(f, "Wrong arguments count for builtin function:\n{}\n{} expected but {} found", func_signature, actual_cnt, given_cnt),
			BuiltinFuncErr::ArgType { func_signature, arg_name, actual_type, given_type, .. } =>
				write!(f, "Wrong type of argument '{}' for builtin function:\n{}\n'{}' expected but '{}' found", arg_name, func_signature, actual_type, given_type),
			BuiltinFuncErr::NotDefined { .. } =>
				write!(f, "Builtin function is not defined"),
		}
	}
}
