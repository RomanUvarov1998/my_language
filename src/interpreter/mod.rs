mod expr;
mod token;
mod statement;
mod var_data;
mod data_type;
mod data_type_template;
mod value;
mod struct_def;
mod builtin_func;
mod user_func;
mod utils;
mod context;
mod primitive_type_member_builtin_funcs_list;

use statement::{StatementsIter, Statement, StatementErr};
use builtin_func::{BuiltinFuncDef, BuiltinFuncArg, BuiltinFuncBody, BuiltinFuncErr};
use user_func::UserFuncErr;
use var_data::VarErr;
use utils::CodePos;
use context::Context;
use data_type::{DataType, BuiltinType};
use value::Value;
use primitive_type_member_builtin_funcs_list::PrimitiveTypeMemberBuiltinFuncsList;
use struct_def::{StructDef, StructDefErr};
use data_type_template::TemplateErr;
use std::collections::HashMap;

//------------------------ Interpreter --------------------

pub struct Interpreter {
	statements_iter: StatementsIter,
	builtin_func_defs: Vec<BuiltinFuncDef>,
	primitive_type_member_funcs_list: PrimitiveTypeMemberBuiltinFuncsList,
}

impl Interpreter {
	pub fn new() -> Self {
		let mut builtin_func_defs = Vec::<BuiltinFuncDef>::new();
		
		builtin_func_defs.push(BuiltinFuncDef::new(
			"print",
			vec![
				BuiltinFuncArg::new("value".to_string(), DataType::Builtin (BuiltinType::Any)),
			],
			Box::new(|args_values: Vec<Value>| -> Value {
				print!("{}", args_values[0]);
				Value::None
			}) as BuiltinFuncBody,
			DataType::Builtin (BuiltinType::None)
		));
		
		builtin_func_defs.push(BuiltinFuncDef::new(
			"println",
			vec![
				BuiltinFuncArg::new("value".to_string(), DataType::Builtin (BuiltinType::Any)),
			],
			Box::new(|args_values: Vec<Value>| -> Value {
				println!("{}", args_values[0]);
				Value::None
			}) as BuiltinFuncBody,
			DataType::Builtin (BuiltinType::None)
		));
		
		builtin_func_defs.push(BuiltinFuncDef::new(
			"exit",
			Vec::new(),
			Box::new(|_args_values: Vec<Value>| -> Value {
				std::process::exit(0)
			}) as BuiltinFuncBody,
			DataType::Builtin (BuiltinType::None)
		));
		
		builtin_func_defs.push(BuiltinFuncDef::new(
			"input",
			vec![
				BuiltinFuncArg::new("prompt".to_string(), DataType::Builtin (BuiltinType::String)),
			],
			Box::new(|args_values: Vec<Value>| -> Value {
				use std::io::{self, Write};
				
				print!("{}", args_values[0]);
				io::stdout().flush().unwrap();
				
				let mut input = String::new();
				io::stdin().read_line(&mut input).unwrap();
				
				Value::from(input.trim_end().clone())
			}) as BuiltinFuncBody,
			DataType::Builtin (BuiltinType::String)
		));
		
		builtin_func_defs.push(BuiltinFuncDef::new(
			"str_to_f32",
			vec![
				BuiltinFuncArg::new("value".to_string(), DataType::Builtin (BuiltinType::String)),
			],
			Box::new(|args_values: Vec<Value>| -> Value {
				let str_value: String = match &args_values[0] {
					val @ Value::String(_) => format!("{}", val),
					_ => unreachable!(),
				};
				let result: f32 = str_value.parse::<f32>().unwrap();
				Value::from(result)
			}) as BuiltinFuncBody,
			DataType::Builtin (BuiltinType::Float32)
		));
		
		Self {
			statements_iter: StatementsIter::new(),
			builtin_func_defs,
			primitive_type_member_funcs_list: PrimitiveTypeMemberBuiltinFuncsList::new(),
		}
	}
	
	pub fn check_and_run(&mut self, code: &str) -> Result<(), InterpErr> {
		self.statements_iter.push_string(code.to_string());
		
		let mut statements = Vec::<Statement>::new();
		let struct_defs = Vec::<StructDef>::new();
		let mut check_context = Context::new(
			&self.builtin_func_defs, 
			&self.primitive_type_member_funcs_list,
			struct_defs);
	
		while let Some(statement_result) = self.statements_iter.next() {
			let st: Statement = statement_result?;
			st.check(&mut check_context)?; // TODO: make check() functions collect all errors and then print them all
			statements.push(st);
		}
		
		let struct_defs = Vec::<StructDef>::new();
		let mut run_context = Context::new(
			&self.builtin_func_defs, 
			&self.primitive_type_member_funcs_list,
			struct_defs);
		
		for st_ref in &statements {
			st_ref.run(&mut run_context); // TODO: make run() functions return Result, which propagates from inside and delivers Err() when index is out of array bounds
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
	DataType (DataTypeErr),
	Template (TemplateErr)
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
			ExprErr::EmptyBracketsPair (pos) => InterpErr {
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
			ExprErr::WrongOperandsForOperator { operator_pos, ref descr } => InterpErr {
				pos: operator_pos,
				descr: descr.clone(),
				inner: InnerErr::Expr (err),
			},
			ExprErr::NotEnoughOperandsForOperator { operator_pos, ref descr } => InterpErr {
				pos: operator_pos,
				descr: descr.clone(),
				inner: InnerErr::Expr (err),
			},
			ExprErr::NotLhsExprSymbol (pos) => InterpErr {
				pos,
				descr: format!("{}", err),
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
			StatementErr::WrongAssignedType { statement_pos, .. } => InterpErr {
				pos: statement_pos,
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
			StructDefErr::NotAStruct { value_pos } => InterpErr {
				pos: value_pos,
				descr,
				inner: InnerErr::StructDef (err),
			},
			StructDefErr::CannotDefineAsBuiltin { ref name } => InterpErr {
				pos: name.pos(),
				descr,
				inner: InnerErr::StructDef (err),
			},
			StructDefErr::StructDefNotInRootContext { struct_pos } => InterpErr {
				pos: struct_pos,
				descr,
				inner: InnerErr::StructDef (err),
			},
			StructDefErr::StructDefIsAlreadyDefined { ref defined_name } => InterpErr {
				pos: defined_name.pos(),
				descr,
				inner: InnerErr::StructDef (err),
			},
			StructDefErr::FieldAlreadyDefined { ref name_in_code } => InterpErr {
				pos: name_in_code.pos(),
				descr,
				inner: InnerErr::StructDef (err),
			},
			StructDefErr::FieldAlreadySet { ref name_in_code } => InterpErr {
				pos: name_in_code.pos(),
				descr,
				inner: InnerErr::StructDef (err),
			},
			StructDefErr::FieldSetTypeErr { ref name_in_code, .. } => InterpErr {
				pos: name_in_code.pos(),
				descr,
				inner: InnerErr::StructDef (err),
			},
			StructDefErr::NotAllFieldsSet { ref name_in_code, .. } => InterpErr {
				pos: name_in_code.pos(),
				descr,
				inner: InnerErr::StructDef (err),
			},
			StructDefErr::FieldDoesNotExist { ref name_in_code } => InterpErr {
				pos: name_in_code.pos(),
				descr,
				inner: InnerErr::StructDef (err),
			},
			StructDefErr::BuiltinMemberFuncIsNotDefined { ref name } => InterpErr {
				pos: name.pos(),
				descr,
				inner: InnerErr::StructDef (err),
			},
			StructDefErr::UserMemberFuncIsNotDefined { ref name } => InterpErr {
				pos: name.pos(),
				descr,
				inner: InnerErr::StructDef (err),
			},
			StructDefErr::ComplexDataTypeHasNoBuiltinFields { ref name_in_code } => InterpErr {
				pos: name_in_code.pos(),
				descr,
				inner: InnerErr::StructDef (err),
			},
		}
	}
}

use data_type::DataTypeErr;
impl From<DataTypeErr> for InterpErr {
	fn from(err: DataTypeErr) -> InterpErr {
		let descr: String = format!("{}", err);
		match err {
			DataTypeErr::NotDefined { ref name } => InterpErr {
				pos: name.pos(),
				descr,
				inner: InnerErr::DataType (err),
			},
		}
	}
}

impl From<TemplateErr> for InterpErr {
	fn from(err: TemplateErr) -> InterpErr {
		let descr: String = format!("{}", err);
		match err {
			TemplateErr::NotSatisfiedTypeParam { ref name } => InterpErr {
				pos: name.pos(),
				descr,
				inner: InnerErr::Template (err),
			},
		}
	}
}

//------------------------ Tests --------------------

// TODO: add tests for all errors

#[cfg(test)]
mod tests {
	use super::*;
	use super::utils::{NameToken, CharPos};
	use super::data_type::DataType;
	
	#[test]
	fn check_err_if_redeclare_variable() {
		let mut int = Interpreter::new();
		
		match int.check_and_run(r#"
		var a: @f32 = 3;
		var a: @f32 = 4;
		"#
		) {
			Err(InterpErr { inner: InnerErr::Var(VarErr::AlreadyExists { name }), .. } ) if name.value() == String::from("a") => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}
	
	#[test]
	fn check_err_if_set_var_to_data_of_wrong_type() {		
		let mut int = Interpreter::new();
		
		let nt = new_name_token("a", false);
		
		match int.check_and_run("var a: @f32 = \"hello\";") {
			Err(InterpErr { inner: InnerErr::Var(VarErr::WrongType { 
				value_data_type: DataType::Builtin (BuiltinType::String), 
				variable_type: DataType::Builtin (BuiltinType::Float32),
				var_name,
			}), .. } ) if var_name == nt => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
		
		match int.check_and_run("var a: @str = 4;") {
			Err(InterpErr { inner: InnerErr::Var(VarErr::WrongType { 
				value_data_type: DataType::Builtin (BuiltinType::Float32), 
				variable_type: DataType::Builtin (BuiltinType::String),
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
		if (2 == 2) { 
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
		if (4) { 
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
f add2(a: @f32, b: @f32) -> @f32 {
	if (a < b) { // 1.1
		a = 4;
		return a;
	} else {
		b = 4;
		if (a < b) {
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
f add2(a: @f32, b: @f32) -> @f32 {
	if (a < b) { // 1.1
		a = 4;
		// Not returning
	} else {
		b = 4;
		if (a < b) {
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
f add2(a: @f32, b: @f32) -> @f32 {
	if (a < b) { // 1.1
		a = 4;
		return a;
	} else {
		b = 4;
		if (a < b) {
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
f add2(a: @f32, b: @f32) -> @f32 {
	if (a < b) { // 1.1
		a = 4;
		return a;
	} else {
		b = 4;
		if (a < b) {
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
f add2(a: @f32, b: @f32) -> @f32 {
	return a;
	if (a < b) { // 1.1
		a = 4;
		// Not returning
	} else {
		b = 4;
		// Not returning
		if (a < b) {
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
			var s: @str = "Hello";
			var a: @f32 = s.@len();
			var b: @f32 = "Hi!".@len();
			var abs: @f32 = (-123.45).@abs();
			var sign: @f32 = (-123.45).@sign();
			abs = a.@abs();
			sign = a.@sign();
		"#) {
			Ok(_) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}

	#[test]
	fn can_define_struct() {
		// TODO: make possible to create struct with a field of a type of itself:
		// struct A { a: A }
		let mut int = Interpreter::new();		
		match int.check_and_run(r#"
struct A {
	a: @f32,
	b: @bool
}

struct B {
	a: A,
	b: @bool
}
		"#) {
			Ok(_) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}

	#[test]
	fn can_assign_defined_struct() {
		let mut int = Interpreter::new();		
		match int.check_and_run(r#"
struct A {
	a: @f32,
	b: @bool
}

var a: A = A { a: 4.5, b: False, };
		"#) {
			Ok(_) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}

	#[test]
	fn can_use_fields_of_defined_struct_in_expr() {
		let mut int = Interpreter::new();		
		match int.check_and_run(r#"
struct A {
	a: @f32,
	b: @bool
}

var a: A = A { a: 4.5, b: False, };

var b: @f32 = a.a + 4;
		"#) {
			Ok(_) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}
	
	#[test]
	fn can_assign_to_struct_member_field() {
		let mut int = Interpreter::new();		
		match int.check_and_run(r#"
struct A {
	a: @f32,
	b: @bool
}

var a: A = A { a: 4.5, b: False, };

a.a = 5;
		"#) {
			Ok(_) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}
		
	#[test]
	fn can_assign_to_struct_member_field_complex() {
		let mut int = Interpreter::new();		
		match int.check_and_run(r#"
struct A {
	a1: @f32,
	a2: @bool,
}

struct B {
	b1: A,
	b2: @bool,
}

struct C {
	c1: B,
	c2: @bool,
}

var c: C = C { 
	c1: B {
		b1: A {
			a1: 3,
			a2: False,
		},
		b2: True,
	}, 
	c2: False, 
};

c.c1.b1.a1 = 6;
		"#) {
			Ok(_) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}
	
	#[test]
	fn can_chain_finc_calls() {
		let mut int = Interpreter::new();		
		match int.check_and_run(r#"
var a: @f32 = "Hello".@len().@abs();
		"#) {
			Ok(_) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}
	
	#[test]
	fn can_get_fields_of_struct_literals() {
		let mut int = Interpreter::new();		
		match int.check_and_run(r#"
struct A {
	a: @f32,
	b: @bool
}

var a: @f32 = A { a: 4.5, b: False, }.a;
		"#) {
			Ok(_) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}
	
	#[test]
	fn can_get_member_field_of_a_value() {
		let mut int = Interpreter::new();		
		match int.check_and_run(r#"
struct A {
	a: @f32,
	b: @bool
}

f create_A(a: @f32, b: @bool) -> A {
	return A { a: a, b: b, };
}

var a: @f32 = create_A(3, False).a;
		"#) {
			Ok(_) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}
	
	#[test]
	fn can_call_member_func_of_struct_field() {
		let mut int = Interpreter::new();		
		match int.check_and_run(r#"
struct A {
	a: @f32,
	b: @bool
}

var a: @f32 = A { a: 43, b: False, }.a.@abs();
		"#) {
			Ok(_) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}
	
	#[test]
	fn can_read_string_char_by_index() {
		let mut int = Interpreter::new();		
		match int.check_and_run(r#"
var a: @str = "Hello!";
var b: @char = a[0];
		"#) {
			Ok(_) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}
	
	#[test]
	fn can_write_string_char_by_index() {
		let mut int = Interpreter::new();		
		match int.check_and_run(r#"
var a: @str = "Hello!";
a[1] = 'F';
		"#) {
			Ok(_) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}
	
	#[test]
	fn can_add_strings_and_chars() {
		let mut int = Interpreter::new();		
		match int.check_and_run(r#"
var a: @str = "Hello";
var b: @char = '!';
@println(a + a);
@println(a + b);
@println(b + a);
@println(b + b);
		"#) {
			Ok(_) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}
	
	#[test]
	fn untyped_array_literal() {
		let mut int = Interpreter::new();		
		match int.check_and_run(r#"
var a: @untyped_array = [1, 2, 3];

@println("Should print '[1, 2, 3]':");

f print_array(arr: @untyped_array) {
	@print("[");
	
	var i: @f32 = 0;
	var is_first: @bool = True;
	while (i < arr.@len()) {
		if (!is_first) { @print(", "); }
		is_first = False;
		@print(arr[i]);
		//@print(1);
		i = i + 1;
	}
	
	@println("]");
}

print_array(a);

@println("");
@println("Should print '[1, 2, 3]':");
@println(a);

@println("");
@println("Should print '[1, 2, 3, 4]':");
a.@add(4);
@println(a);

@println("");
@println("Should print '1':");
@println(a.@get(0));

@println("");
@println("Should print '[6, 2, 3, 4]':");
a.@set(0, 6);
@println(a);

@println("");
@println("Should print '6':");
@println(a.@remove(0));

@println("");
@println("Should print '[2, 3, 4]':");
@println(a);

@println("");
@println("Should print '[2, 7, 3, 4]':");
a.@insert(1, 7);
@println(a);

@println("");
@println("Should print '[1, 2, 3, 4, 5]':");
@println(1 + [2, 3, 4] + 5);

@println("");
@println("Should print '[1, 2, 3, 4, 5]':");
@println([1, 2, 3] + [4, 5]);
		"#) {
			Ok(_) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}
	
	// TODO: add test ans implement member func call of a user-defined struct
	
	fn new_name_token(name: &str, is_builtin: bool) -> NameToken {
		NameToken::new_with_pos(name.to_string(), CodePos::from(CharPos::new()), is_builtin)
	}
}