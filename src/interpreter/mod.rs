mod expr;
mod string_char;
mod token;
mod statement;
mod var_data;
mod data_type;
mod value;
mod struct_def;
mod builtin_func;
mod user_func;
mod utils;
mod context;
mod primitive_type_member_funcs_list;

use statement::{StatementsIter, Statement, StatementErr};
use builtin_func::{BuiltinFuncDef, BuiltinFuncArg, BuiltinFuncBody, BuiltinFuncErr};
use user_func::UserFuncErr;
use var_data::VarErr;
use utils::CodePos;
use context::Context;
use data_type::{DataType, Primitive};
use value::Value;
use primitive_type_member_funcs_list::PrimitiveTypeMemberFuncsList;
use struct_def::StructDefErr;

//------------------------ Interpreter --------------------

pub struct Interpreter {
	statements_iter: StatementsIter,
	builtin_func_defs: Vec<BuiltinFuncDef>,
	primitive_type_member_funcs_list: PrimitiveTypeMemberFuncsList,
}

impl Interpreter {
	pub fn new() -> Self {
		let mut builtin_func_defs = Vec::<BuiltinFuncDef>::new();
		
		builtin_func_defs.push(BuiltinFuncDef::new(
			"print",
			vec![
				BuiltinFuncArg::new("value".to_string(), DataType::Primitive (Primitive::Any)),
			],
			Box::new(|args_values: Vec<Value>| -> Option<Value> {
				print!("{}", args_values[0]);
				None
			}) as BuiltinFuncBody,
			DataType::Primitive (Primitive::Any)
		));
		
		builtin_func_defs.push(BuiltinFuncDef::new(
			"println",
			vec![
				BuiltinFuncArg::new("value".to_string(), DataType::Primitive (Primitive::Any)),
			],
			Box::new(|args_values: Vec<Value>| -> Option<Value> {
				println!("{}", args_values[0]);
				None
			}) as BuiltinFuncBody,
			DataType::Primitive (Primitive::Any)
		));
		
		builtin_func_defs.push(BuiltinFuncDef::new(
			"exit",
			Vec::new(),
			Box::new(|_args_values: Vec<Value>| -> Option<Value> {
				std::process::exit(0)
			}) as BuiltinFuncBody,
			DataType::Primitive (Primitive::Any)
		));
		
		builtin_func_defs.push(BuiltinFuncDef::new(
			"input",
			vec![
				BuiltinFuncArg::new("prompt".to_string(), DataType::Primitive (Primitive::String)),
			],
			Box::new(|args_values: Vec<Value>| -> Option<Value> {
				use std::io::{self, Write};
				
				print!("{}", args_values[0]);
				io::stdout().flush().unwrap();
				
				let mut input = String::new();
				io::stdin().read_line(&mut input).unwrap();
				
				Some(Value::from(input.trim_end().clone()))
			}) as BuiltinFuncBody,
			DataType::Primitive (Primitive::String)
		));
		
		builtin_func_defs.push(BuiltinFuncDef::new(
			"str_to_f32",
			vec![
				BuiltinFuncArg::new("value".to_string(), DataType::Primitive (Primitive::String)),
			],
			Box::new(|args_values: Vec<Value>| -> Option<Value> {
				let str_value: String = match &args_values[0] {
					Value::String(val) => val.clone(),
					_ => unreachable!(),
				};
				let result: f32 = str_value.parse::<f32>().unwrap();
				Some(Value::from(result))
			}) as BuiltinFuncBody,
			DataType::Primitive (Primitive::Float32)
		));
		
		Self {
			statements_iter: StatementsIter::new(),
			builtin_func_defs,
			primitive_type_member_funcs_list: PrimitiveTypeMemberFuncsList::new(),
		}
	}
	
	pub fn check_and_run(&mut self, code: &str) -> Result<(), InterpErr> {
		self.statements_iter.push_string(code.to_string());
		
		let mut statements = Vec::<Statement>::new();
		let mut check_context = Context::new(
			&self.builtin_func_defs, 
			&self.primitive_type_member_funcs_list);
	
		while let Some(statement_result) = self.statements_iter.next() {
			let st: Statement = statement_result?;
			st.check(&mut check_context)?;
			statements.push(st);
		}
		
		let mut run_context = Context::new(
			&self.builtin_func_defs, 
			&self.primitive_type_member_funcs_list);
		
		for st_ref in &statements {
			st_ref.run(&mut run_context);
		}
		
		Ok(())
	}
}

//------------------------ InterpErr --------------------

#[derive(Debug, PartialEq, Eq)]
pub struct InterpErr {
	pos: CodePos,
	descr: String,
	inner: InnerErr,
}

#[derive(Debug, PartialEq, Eq)]
pub enum InnerErr {
	Token (TokenErr),
	Expr (ExprErr),
	Var (VarErr),
	BuiltinFunc (BuiltinFuncErr),
	UserFunc (UserFuncErr),
	Statement (StatementErr),
	StructDef (StructDefErr),
}

impl InterpErr {
	pub fn pos(&self) -> CodePos { self.pos }
	pub fn descr(&self) -> &str { &self.descr }
}

impl std::fmt::Display for InterpErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {		
		for _ in 0..self.pos.begin().col() {
			write!(f, "_")?;
		}
		for _ in self.pos.begin().col()..=self.pos.end().col() {
			write!(f, "^")?;
		}
		writeln!(f, "")?;
		
		writeln!(f, "{}", self.descr())
	}
}

use token::TokenErr;
impl From<TokenErr> for InterpErr {
	fn from(err: TokenErr) -> InterpErr {
		match err {
			TokenErr::Construct (ch) => InterpErr {
				pos: CodePos::from(ch.pos()),
				descr: format!("{}", err),
				inner: InnerErr::Token (err),
			},
			TokenErr::ExpectedButFound { ref found, .. } => InterpErr {
				pos: found.pos(),
				descr: format!("{}", err),
				inner: InnerErr::Token (err),
			},
			TokenErr::EndReached { pos } => InterpErr {
				pos: CodePos::from(pos),
				descr: format!("{}", err),
				inner: InnerErr::Token (err),
			},
		}
	}
}

use expr::ExprErr;
impl From<ExprErr> for InterpErr {
	fn from(err: ExprErr) -> InterpErr {
		match err {
			ExprErr::UnexpectedToken (pos) => InterpErr {
				pos,
				descr: format!("{}", err),
				inner: InnerErr::Expr (err),
			},
			ExprErr::UnpairedBracket (pos) => InterpErr {
				pos,
				descr: format!("{}", err),
				inner: InnerErr::Expr (err),
			},
			ExprErr::ExpectedExprButFound (pos) => InterpErr {
				pos,
				descr: format!("{}", err),
				inner: InnerErr::Expr (err),
			},
			ExprErr::WrongOperandsTypeForOperator { operator_pos, ref descr } => InterpErr {
				pos: operator_pos,
				descr: descr.clone(),
				inner: InnerErr::Expr (err),
			},
			ExprErr::NotEnoughOperandsForOperator { operator_pos, ref descr } => InterpErr {
				pos: operator_pos,
				descr: descr.clone(),
				inner: InnerErr::Expr (err),
			},
		}
	}
}

impl From<VarErr> for InterpErr {
	fn from(err: VarErr) -> InterpErr {
		match err {		
			VarErr::NotDefined { ref name } => InterpErr {
				pos: name.pos(),
				descr: format!("{}", err),
				inner: InnerErr::Var (err),
			},
			VarErr::NotSet { ref name } => InterpErr {
				pos: name.pos(),
				descr: format!("{}", err),
				inner: InnerErr::Var (err),
			},
			VarErr::UnknownType { ref name } => InterpErr {
				pos: name.pos(),
				descr: format!("{}", err),
				inner: InnerErr::Var (err),
			},
			VarErr::AlreadyExists { ref name } => InterpErr {
				pos: name.pos(),
				descr: format!("{}", err),
				inner: InnerErr::Var (err),
			},
			VarErr::WrongValue { ref var_name, .. } => InterpErr {
				pos: var_name.pos(),
				descr: format!("{}", err),
				inner: InnerErr::Var (err),
			},
			VarErr::WrongType { ref var_name, .. } => InterpErr {
				pos: var_name.pos(),
				descr: format!("{}", err),
				inner: InnerErr::Var (err),
			},
		}
	}
}

impl From<UserFuncErr> for InterpErr {
	fn from(err: UserFuncErr) -> InterpErr {
		match err {	
			UserFuncErr::ArgsCnt { func_name_pos, .. } => InterpErr {
				pos: func_name_pos,
				descr: format!("{}", err),
				inner: InnerErr::UserFunc (err),
			},
			UserFuncErr::ArgType { arg_expr_pos, .. } => InterpErr {
				pos: arg_expr_pos,
				descr: format!("{}", err),
				inner: InnerErr::UserFunc (err),
			},	
			UserFuncErr::NotDefined { ref name } => InterpErr {
				pos: name.pos(),
				descr: format!("{}", err),
				inner: InnerErr::UserFunc (err),
			},
			UserFuncErr::AlreadyExists { ref name } => InterpErr {
				pos: name.pos(),
				descr: format!("{}", err),
				inner: InnerErr::UserFunc (err),
			},
		}
	}
}

impl From<StatementErr> for InterpErr {
	fn from(err: StatementErr) -> InterpErr {
		let descr: String = format!("{}", err);

		match err {
			StatementErr::UnfinishedBody (pos) => InterpErr {
				pos: CodePos::from(pos),
				descr,
				inner: InnerErr::Statement (err),
			},
			StatementErr::IfConditionType { pos } => InterpErr {
				pos,
				descr,
				inner: InnerErr::Statement (err),
			},
			StatementErr::UserFuncReturnType { return_expr_pos, .. } => InterpErr {
				pos: return_expr_pos,
				descr,
				inner: InnerErr::Statement (err),
			},
			StatementErr::UserFuncNotAllFuncPathsReturn { last_statement_pos } => InterpErr {
				pos: last_statement_pos,
				descr,
				inner: InnerErr::Statement (err),
			},
		}
	}
}

impl From<BuiltinFuncErr> for InterpErr {
	fn from(err: BuiltinFuncErr) -> InterpErr {
		let descr: String = format!("{}", err);
		match err {
			BuiltinFuncErr::ArgsCnt { func_name_pos, .. } => InterpErr {
				pos: func_name_pos,
				descr,
				inner: InnerErr::BuiltinFunc (err),
			},
			BuiltinFuncErr::ArgType { arg_expr_pos, .. } => InterpErr {
				pos: arg_expr_pos,
				descr,
				inner: InnerErr::BuiltinFunc (err),
			},
			BuiltinFuncErr::NotDefined { name_pos } => InterpErr {
				pos: name_pos,
				descr,
				inner: InnerErr::BuiltinFunc (err),
			},
		}
	}
}

impl From<StructDefErr> for InterpErr {
	fn from(err: StructDefErr) -> InterpErr {
		let descr: String = format!("{}", err);
		match err {
			StructDefErr::FieldAlreadyDefined { ref name_in_code } => InterpErr {
				pos: name_in_code.pos(),
				descr,
				inner: InnerErr::StructDef (err),
			},
			StructDefErr::BuiltinMemberFuncIsNotDefined { ref name } => InterpErr {
				pos: name.pos(),
				descr,
				inner: InnerErr::StructDef (err),
			},
		}
	}
}

//------------------------ Tests --------------------

#[cfg(test)]
mod tests {
	use super::*;
	use super::utils::{NameToken, CharPos};
	use super::data_type::DataType;
	
	#[test]
	fn check_err_if_redeclare_variable() {
		let mut int = Interpreter::new();
		
		match int.check_and_run(r#"
		var a: f32 = 3;
		var a: f32 = 4;
		"#
		) {
			Err(InterpErr { inner: InnerErr::Var(VarErr::AlreadyExists { name }), .. } ) if name.value() == String::from("a") => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}
	
	#[test]
	fn check_err_if_set_var_to_data_of_wrong_type() {		
		let mut int = Interpreter::new();
		
		let nt = new_name_token("a");
		
		match int.check_and_run("var a: f32 = \"hello\";") {
			Err(InterpErr { inner: InnerErr::Var(VarErr::WrongType { 
				value_data_type: DataType::Primitive (Primitive::String), 
				variable_type: DataType::Primitive (Primitive::Float32),
				var_name,
			}), .. } ) if var_name == nt => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
		
		match int.check_and_run("var a: str = 4;") {
			Err(InterpErr { inner: InnerErr::Var(VarErr::WrongType { 
				value_data_type: DataType::Primitive (Primitive::Float32), 
				variable_type: DataType::Primitive (Primitive::String),
				var_name,
			}), .. } ) if var_name == nt => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}

	#[test]
	fn builtin_functions() {
		let mut int = Interpreter::new();
		
		match int.check_and_run("@print(\"hello\");") {
			Ok(()) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
		
		match int.check_and_run("@print(3);") {
			Ok(()) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
		
		match int.check_and_run("@print(False);") {
			Ok(()) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}

	#[test]
	fn if_statement() {
		let mut int = Interpreter::new();		
		match int.check_and_run(r#"
		if 2 == 2 { 
			@print("2 == 2"); 
			@print("Cool!"); 
		} 
		@print("end");
		"#) {
			Ok(_) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
		
		let mut int = Interpreter::new();
		match int.check_and_run(r#"
		if 4 { 
			@print("2 == 2"); 
			@print("Cool!"); 
		} 
		@print("end");
		"#) {
			Err(InterpErr { inner: InnerErr::Statement(StatementErr::IfConditionType { .. }), .. } ) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}

	#[test]
	fn check_not_all_func_paths_return() {
		let mut int = Interpreter::new();		
		match int.check_and_run(r#"
f add2(a: f32, b: f32) -> f32 {
	if a < b { // 1.1
		a = 4;
		return a;
	} else {
		b = 4;
		if a < b {
			return 4;
		} else {
			return 3;
		}
	}
}
		"#) {
			Ok(_) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
		
		let mut int = Interpreter::new();		
		match int.check_and_run(r#"
f add2(a: f32, b: f32) -> f32 {
	if a < b { // 1.1
		a = 4;
		// Not returning
	} else {
		b = 4;
		if a < b {
			return 4;
		} else {
			return 3;
		}
	}
}
		"#) {
			Err(InterpErr { inner: InnerErr::Statement (StatementErr::UserFuncNotAllFuncPathsReturn { .. } ), .. } ) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
		
		let mut int = Interpreter::new();		
		match int.check_and_run(r#"
f add2(a: f32, b: f32) -> f32 {
	if a < b { // 1.1
		a = 4;
		return a;
	} else {
		b = 4;
		if a < b {
			a = 4; 
			// Not returning
		} else {
			return 3;
		}
	}
}
		"#) {
			Err(InterpErr { inner: InnerErr::Statement (StatementErr::UserFuncNotAllFuncPathsReturn { .. }), .. } ) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
		
		let mut int = Interpreter::new();		
		match int.check_and_run(r#"
f add2(a: f32, b: f32) -> f32 {
	if a < b { // 1.1
		a = 4;
		return a;
	} else {
		b = 4;
		if a < b {
			a = 4; 
			return 3;
		} else {
			b = 4; 
			// Not returning
		}
	}
}
		"#) {
			Err(InterpErr { inner: InnerErr::Statement (StatementErr::UserFuncNotAllFuncPathsReturn { .. }), .. } ) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
		
		let mut int = Interpreter::new();		
		match int.check_and_run(r#"
f add2(a: f32, b: f32) -> f32 {
	return a;
	if a < b { // 1.1
		a = 4;
		// Not returning
	} else {
		b = 4;
		// Not returning
		if a < b {
			a = 4; 
			// Not returning
		} else {
			b = 4; 
			// Not returning
		}
	}
}
		"#) {
			Ok(_) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}

	#[test]
	fn can_call_primitive_type_builtin_member_functions() {
		let mut int = Interpreter::new();		
		match int.check_and_run(r#"
			var s: str = "Hello";
			var a: f32 = s.@len();
			var b: f32 = "Hi!".@len();
			var abs: f32 = (-123.45).@abs();
			var sign: f32 = (-123.45).@sign();
		"#) {
			Ok(_) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}

	fn new_name_token(name: &str) -> NameToken {
		NameToken::new_with_pos(name.to_string(), CodePos::from(CharPos::new()))
	}
}