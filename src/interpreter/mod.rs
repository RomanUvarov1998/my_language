mod memory;
mod expr;
mod string_char;
mod token;
mod statement;
mod var_data;
mod func_data;

use statement::{StatementsIter, NameToken, Statement, WithVariable, FuncKind, Branching, StatementErr};
use memory::*;
use func_data::{BuiltinFuncsDefList, BuiltinFuncDef, BuiltinFuncArg, BuiltinFuncErr};
use var_data::{VarErr, Value};
use string_char::CharPos;

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
				BuiltinFuncArg::new("value".to_string(), None),
			]
		)).unwrap();
		
		builtin_func_defs.add(BuiltinFuncDef::new(
			"exit",
			Vec::new()
		)).unwrap();
		
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
						Some(value_expr.calc(&self.memory)?))?,
				
				WithVariable::Set { var_name, value_expr } => 
					self.memory.set_variable(&var_name, value_expr.calc(&self.memory)?)?,
			},
			
			Statement::FuncCall { kind, func_name, arg_exprs } => {
				match kind {
					FuncKind::Builtin => {						
						let mut arg_vals = Vec::<Value>::with_capacity(arg_exprs.len());
						for expr in arg_exprs {
							arg_vals.push(expr.calc(&self.memory)?);
						}
						
						self.call_builtin_func(&func_name, arg_vals)?;
					},
					FuncKind::UserDefined => {
						todo!();
						//self.memory.call_func(name, args)?;
					},
				}
			},
			
			Statement::Branching (br) => match br {
				Branching::IfElse { if_bodies, else_body } => {
					// TODO: make local variables to be kept in local scope memory of 'if' statement
					
					let mut got_true = false;
					
					'if_out: for body in if_bodies {
						match body.condition_expr().calc(&self.memory)? {
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
					while let Value::Bool(true) = body.condition_expr().calc(&self.memory)? {
						for st_ref in body.statements() {
							self.run_statement(st_ref)?;
						}
					}
				},
			},
		}
		Ok(())
	}
	
	fn call_builtin_func(&self, name: &NameToken, args_values: Vec<Value>) -> Result<(), InterpErr> {
		match name.value() {
			"print" => {
				println!("{}", args_values[0]);
				Ok(())
			},
			"exit" => {
				Err( InterpErr::new_halt_request() )
			},
			"to_str" => {
				
				Ok(())
			},
			// TODO: add @read() func to get input from console
			_ => unreachable!(),
		}
	}
}

//------------------------ InterpErr --------------------

#[derive(Debug, PartialEq, Eq)]
pub struct InterpErr {
	pos_begin: CharPos,
	pos_end: CharPos,
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
			pos_begin: CharPos::new(),
			pos_end: CharPos::new(),
			descr: String::new(),
			inner: InnerErr::HaltRequest,
		}
	}
	pub fn pos_begin(&self) -> CharPos { self.pos_begin }
	pub fn pos_end(&self) -> CharPos { self.pos_end }
	pub fn descr(&self) -> &str { &self.descr }
	pub fn inner(&self) -> &InnerErr { &self.inner }
}

impl std::fmt::Display for InterpErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {		
		for _ in 0..self.pos_begin.col() {
			write!(f, "_")?;
		}
		for _ in self.pos_begin.col()..=self.pos_end.col() {
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
				pos_begin: ch.pos(),
				pos_end: ch.pos(),
				descr: format!("{}", err),
				inner: InnerErr::Token (err),
			},
			TokenErr::ExpectedButFound { ref found, .. } => InterpErr {
				pos_begin: found.pos_begin(),
				pos_end: found.pos_end(),
				descr: format!("{}", err),
				inner: InnerErr::Token (err),
			},
			TokenErr::EndReached { pos } => InterpErr {
				pos_begin: pos,
				pos_end: pos,
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
			ExprErr::UnexpectedToken (ref tok) => InterpErr {
				pos_begin: tok.pos_begin(),
				pos_end: tok.pos_end(),
				descr: format!("{}", err),
				inner: InnerErr::Expr (err),
			},
			ExprErr::UnpairedBracket (ref tok) => InterpErr {
				pos_begin: tok.pos_begin(),
				pos_end: tok.pos_end(),
				descr: format!("{}", err),
				inner: InnerErr::Expr (err),
			},
			ExprErr::ExpectedExprButFound (ref tok) => InterpErr {
				pos_begin: tok.pos_begin(),
				pos_end: tok.pos_end(),
				descr: format!("{}", err),
				inner: InnerErr::Expr (err),
			},
			ExprErr::Operator { ref tok, .. } => InterpErr {
				pos_begin: tok.pos_begin(),
				pos_end: tok.pos_end(),
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
				pos_begin: name.tok().pos_begin(),
				pos_end: name.tok().pos_end(),
				descr: format!("{}", err),
				inner: InnerErr::Var (err),
			},
			VarErr::NotSet { ref name } => InterpErr {
				pos_begin: name.tok().pos_begin(),
				pos_end: name.tok().pos_end(),
				descr: format!("{}", err),
				inner: InnerErr::Var (err),
			},
			VarErr::UnknownType { ref name } => InterpErr {
				pos_begin: name.tok().pos_begin(),
				pos_end: name.tok().pos_end(),
				descr: format!("{}", err),
				inner: InnerErr::Var (err),
			},
			VarErr::AlreadyExists { ref name } => InterpErr {
				pos_begin: name.tok().pos_begin(),
				pos_end: name.tok().pos_end(),
				descr: format!("{}", err),
				inner: InnerErr::Var (err),
			},
			VarErr::WrongValue { ref var_name, .. } => InterpErr {
				pos_begin: var_name.tok().pos_begin(),
				pos_end: var_name.tok().pos_end(),
				descr: format!("{}", err),
				inner: InnerErr::Var (err),
			},
			VarErr::WrongType { ref var_name, .. } => InterpErr {
				pos_begin: var_name.tok().pos_begin(),
				pos_end: var_name.tok().pos_end(),
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
			StatementErr::Func { err: ferr, ref name } => InterpErr {
				pos_begin: name.tok().pos_begin(),
				pos_end: name.tok().pos_end(),
				descr,
				inner: InnerErr::BuiltinFunc (ferr),
			},
			StatementErr::UnfinishedBody (pos) => InterpErr {
				pos_begin: pos,
				pos_end: pos,
				descr,
				inner: InnerErr::Statement (err),
			},
			StatementErr::IfConditionType { cond_expr_begin, cond_expr_end, } => InterpErr {
				pos_begin: cond_expr_begin,
				pos_end: cond_expr_end,
				descr,
				inner: InnerErr::Statement (err),
			},
		}
	}
}

//------------------------ Tests --------------------

#[cfg(test)]
mod tests {
	use super::*;
	use super::token::{Token, TokenContent};
	use super::string_char::CharPos;
	use super::statement::NameToken;
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
		
		let nt = NameToken::from(Token::new(CharPos::new(), CharPos::new(), TokenContent::Name(String::from("a")))).unwrap();
		
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
}