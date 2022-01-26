mod memory;
mod expr;
mod string_char;
mod token;
mod statement;
mod var_data;
mod func_data;
mod utils;

use statement::{StatementsIter, Statement, WithVariable, FuncKind, Branching, StatementErr};
use memory::*;
use func_data::{BuiltinFuncsDefList, BuiltinFuncDef, BuiltinFuncArg, BuiltinFuncBody, BuiltinFuncErr};
use var_data::{VarErr, Value, DataType};
use utils::{CharPos, CodePos};

//------------------------ Interpreter --------------------

pub struct Interpreter {
	memory: Memory,
	builtin_func_defs: BuiltinFuncsDefList,
	statements_iter: StatementsIter,
}

impl Interpreter {
	pub fn new() -> Self {
		let mut builtin_func_defs = BuiltinFuncsDefList::new();
		
		builtin_func_defs.add(BuiltinFuncDef::new(
			"print",
			vec![
				BuiltinFuncArg::new("value".to_string(), DataType::Untyped),
			],
			Box::new(|args_values: Vec<Value>| -> Result<Option<Value>, InterpErr> {
				println!("{}", args_values[0]);
				Ok(None)
			}) as BuiltinFuncBody,
			DataType::Untyped
		));
		
		builtin_func_defs.add(BuiltinFuncDef::new(
			"exit",
			Vec::new(),
			Box::new(|_args_values: Vec<Value>| -> Result<Option<Value>, InterpErr> {
				Err( InterpErr::new_halt_request() )
			}) as BuiltinFuncBody,
			DataType::Untyped
		));
		
		builtin_func_defs.add(BuiltinFuncDef::new(
			"input",
			vec![
				BuiltinFuncArg::new("prompt".to_string(), DataType::Untyped),
			],
			Box::new(|args_values: Vec<Value>| -> Result<Option<Value>, InterpErr> {
				use std::io::{self, Write};
				
				print!("{}", args_values[0]);
				io::stdout().flush().unwrap();
				
				let mut input = String::new();
				io::stdin().read_line(&mut input).unwrap();
				
				Ok(Some(Value::from(input.trim_end().clone())))
			}) as BuiltinFuncBody,
			DataType::String
		));
		
		builtin_func_defs.add(BuiltinFuncDef::new(
			"str_to_f32",
			vec![
				BuiltinFuncArg::new("value".to_string(), DataType::String),
			],
			Box::new(|args_values: Vec<Value>| -> Result<Option<Value>, InterpErr> {
				let str_value: String = match &args_values[0] {
					Value::String(val) => val.clone(),
					_ => unreachable!(),
				};
				let result: f32 = str_value.parse::<f32>().unwrap();
				Ok(Some(Value::from(result)))
			}) as BuiltinFuncBody,
			DataType::Float32
		));
		
		Self {
			memory: Memory::new(),
			builtin_func_defs,
			statements_iter: StatementsIter::new(),
		}
	}
	
	pub fn check_and_run(&mut self, code: &str) -> Result<(), InterpErr> {
		self.statements_iter.push_string(code.to_string());
		
		let mut statements = Vec::<Statement>::new();
		let mut check_memory = Memory::new();
	
		while let Some(statement_result) = self.statements_iter.next() {
			let st: Statement = statement_result?;
			st.check(&mut check_memory, &self.builtin_func_defs)?;
			statements.push(st);
		}
		
		for st_ref in &statements {
			self.run_statement(st_ref)?;
		}
		
		Ok(())
	}
	
	fn run_statement(&mut self, statement: &Statement) -> Result<(), InterpErr> {
		match statement {
			Statement::Comment (_) => {},
			
			Statement::WithVariable (st) => match st {
				WithVariable::Declare { var_name, data_type } => 
					self.memory.add_variable(var_name.clone(), *data_type, None)?,
					
				WithVariable::DeclareSet { var_name, data_type, value_expr } =>
					self.memory.add_variable(
						var_name.clone(), 
						*data_type,
						Some(value_expr.calc(&self.memory, &self.builtin_func_defs)?))?,
				
				WithVariable::Set { var_name, value_expr } => 
					self.memory.set_variable(&var_name, value_expr.calc(&self.memory, &self.builtin_func_defs)?)?,
			},
			
			Statement::FuncCall { kind, func_name, arg_exprs } => {
				match kind {
					FuncKind::Builtin => {
						let f = self.builtin_func_defs.find(func_name).unwrap();
						
						let mut arg_vals = Vec::<Value>::with_capacity(arg_exprs.len());
						for expr in arg_exprs {
							arg_vals.push(expr.calc(&self.memory, &self.builtin_func_defs)?);
						}
						
						f.call(arg_vals)?;
					},
					FuncKind::UserDefined => todo!(),
				}
			},
			
			Statement::Branching (br) => match br {
				Branching::IfElse { if_bodies, else_body } => {
					// TODO: make local variables to be kept in local scope memory of 'if' statement
					
					let mut got_true = false;
					
					'if_out: for body in if_bodies {
						match body.condition_expr().calc(&self.memory, &self.builtin_func_defs)? {
							Value::Bool(true) => {
								for st_ref in body.statements() {
									self.run_statement(st_ref)?;
								}
								got_true = true;
								break 'if_out;
							},
							Value::Bool(false) => {},
							result @ _ => panic!("Wrong condition expr data type: {:?}", result.get_type()),
						}
					}
					
					if !got_true {
						for st_ref in else_body.statements() {
							self.run_statement(st_ref)?;
						}
					}
				},
				Branching::While { body } => {
					while let Value::Bool(true) = body.condition_expr().calc(&self.memory, &self.builtin_func_defs)? {
						for st_ref in body.statements() {
							self.run_statement(st_ref)?;
						}
					}
				},
			},
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
	Statement (StatementErr),
	HaltRequest,
}

impl InterpErr {
	fn new_halt_request() -> Self {
		InterpErr {
			pos: CodePos::from(CharPos::new()), // TODO: do not use error to halt program execution from the middle
			descr: String::new(),
			inner: InnerErr::HaltRequest,
		}
	}
	pub fn pos(&self) -> CodePos { self.pos }
	pub fn descr(&self) -> &str { &self.descr }
	pub fn inner(&self) -> &InnerErr { &self.inner }
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
			ExprErr::Operator { pos, .. } => InterpErr {
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

//------------------------ Tests --------------------

#[cfg(test)]
mod tests {
	use super::*;
	use super::utils::NameToken;
	use super::var_data::DataType;
	
	#[test]
	fn check_err_if_redeclare_variable() {
		let mut int = Interpreter::new();
		
		match int.check_and_run("var a: f32 = 3;") {
			Ok(()) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
		
		match int.check_and_run("var a: f32 = 4;") {
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
				value_data_type: DataType::String, 
				var_data_type: DataType::Float32,
				var_name,
			}), .. } ) if var_name == nt => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
		
		match int.check_and_run("var a: str = 4;") {
			Err(InterpErr { inner: InnerErr::Var(VarErr::WrongType { 
				value_data_type: DataType::Float32, 
				var_data_type: DataType::String,
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
		
		match int.check_and_run("if 2 == 2 { @print(\"2 == 2\"); @print(\"Cool!\"); } @exit();") {
			Err(err) => assert_eq!(*err.inner(), InnerErr::HaltRequest),
			res @ _ => panic!("Wrong result: {:?}", res),
		}
		
		match int.check_and_run("if 4 { @print(\"2 == 2\"); @print(\"Cool!\"); } @exit();") {
			Err(InterpErr { inner: InnerErr::Statement(StatementErr::IfConditionType { .. }), .. } ) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}

	fn new_name_token(name: &str) -> NameToken {
		NameToken::new_with_pos(name, CodePos::from(CharPos::new()))
	}
}