mod memory;
mod expr;
mod string_char;
mod token;
mod statement;
mod var_data;
mod func_data;

use statement::*;
use memory::*;
use func_data::{FuncsDefList, FuncDef, FuncArg, FuncErr};
use var_data::{VarErr, Value, DataType};
use string_char::CharPos;

//------------------------ Interpreter --------------------

pub struct Interpreter {
	memory: Memory,
	builtin_func_defs: FuncsDefList,
	statements_iter: StatementsIter,
}

impl Interpreter {
	pub fn new() -> Self {
		let mut builtin_func_defs = FuncsDefList::new();
		
		builtin_func_defs.add(FuncDef::new(
			"print".to_string(),
			vec![
				FuncArg::new("value".to_string(), None),
			]
		)).unwrap();
		
		builtin_func_defs.add(FuncDef::new(
			"exit".to_string(),
			Vec::new()
		)).unwrap();
		
		Self {
			memory: Memory::new(),
			builtin_func_defs,
			statements_iter: StatementsIter::new(),
		}
	}
	
	pub fn check_and_run(&mut self, code: &str) -> Result<InterpInnerSignal, InterpErr> {
		self.statements_iter.push_string(code.to_string());
		
		let mut statements = Vec::<Statement>::new();
		let mut types_memory = Memory::new();
	
		while let Some(statement_result) = self.statements_iter.next() {
			let st: Statement = statement_result?;
			st.check_types(&mut types_memory, &self.builtin_func_defs, &self.memory)?;
			statements.push(st);
		}
		
		for st in statements {
			match self.run_statement(st)? {
				InterpInnerSignal::CanContinue => continue,
				InterpInnerSignal::Exit => return Ok(InterpInnerSignal::Exit),
			}
		}
		
		Ok(InterpInnerSignal::CanContinue)
	}
	
	fn run_statement(&mut self, statement: Statement) -> Result<InterpInnerSignal, InterpErr> {
		match statement {
			Statement::WithVariable (st) => match st {
				WithVariable::Declare { var_name, data_type } => 
					self.memory.add_variable(var_name, data_type, None)?,
					
				WithVariable::DeclareSet { var_name, data_type, value_expr } =>
					self.memory.add_variable(
						var_name, 
						data_type,
						Some(value_expr.calc(&self.memory)?))?,
				
				WithVariable::Set { var_name, value_expr } => 
					self.memory.set_variable(&var_name, value_expr.calc(&self.memory)?)?,
			},
			
			Statement::FuncCall { kind, name, arg_exprs } => {				
				match kind {
					FuncKind::Builtin => {						
						let mut arg_vals = Vec::<Value>::with_capacity(arg_exprs.len());
						for expr in arg_exprs {
							arg_vals.push(expr.calc(&self.memory)?);
						}
						
						if let InterpInnerSignal::Exit = self.call_builtin_func(&name, arg_vals)? {
							return Ok( InterpInnerSignal::Exit );
						}
					},
					FuncKind::UserDefined => {
						todo!();
						//self.memory.call_func(name, args)?;
					},
				}
			},
		}
		Ok( InterpInnerSignal::CanContinue )
	}
	
	fn call_builtin_func(&self, name: &str, args_values: Vec<Value>) -> Result<InterpInnerSignal, InterpErr> {
		match name {
			"print" => {
				println!("{}", args_values[0]);
				Ok( InterpInnerSignal::CanContinue )
			},
			"exit" => {
				Ok( InterpInnerSignal::Exit )
			},
			_ => unreachable!(),
		}
	}
}

//------------------------ InterpInnerSignal --------------------

#[derive(Debug, PartialEq, Eq)]
pub enum InterpInnerSignal {
	CanContinue,
	Exit,
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
	Func (FuncErr),
}

impl InterpErr {
	pub fn pos_begin(&self) -> CharPos { self.pos_begin }
	pub fn pos_end(&self) -> CharPos { self.pos_end }
	pub fn descr(&self) -> &str { &self.descr }
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
			VarErr::NotDefined { .. } => InterpErr {
				pos_begin: CharPos::new(),
				pos_end: CharPos::new(),
				descr: format!("{}", err),
				inner: InnerErr::Var (err),
			},
			VarErr::NotSet { .. } => InterpErr {
				pos_begin: CharPos::new(),
				pos_end: CharPos::new(),
				descr: format!("{}", err),
				inner: InnerErr::Var (err),
			},
			VarErr::UnknownType { .. } => InterpErr {
				pos_begin: CharPos::new(),
				pos_end: CharPos::new(),
				descr: format!("{}", err),
				inner: InnerErr::Var (err),
			},
			VarErr::AlreadyExists { .. } => InterpErr {
				pos_begin: CharPos::new(),
				pos_end: CharPos::new(),
				descr: format!("{}", err),
				inner: InnerErr::Var (err),
			},
			VarErr::WrongValue { .. } => InterpErr {
				pos_begin: CharPos::new(),
				pos_end: CharPos::new(),
				descr: format!("{}", err),
				inner: InnerErr::Var (err),
			},
			VarErr::WrongType { .. } => InterpErr {
				pos_begin: CharPos::new(),
				pos_end: CharPos::new(),
				descr: format!("{}", err),
				inner: InnerErr::Var (err),
			},
		}
	}
}

impl From<FuncErr> for InterpErr {
	fn from(err: FuncErr) -> InterpErr {
		match err {			
			FuncErr::ArgsCnt { .. } => InterpErr {
				pos_begin: CharPos::new(),
				pos_end: CharPos::new(),
				descr: format!("{}", err),
				inner: InnerErr::Func (err),
			},
			FuncErr::ArgType { .. } => InterpErr {
				pos_begin: CharPos::new(),
				pos_end: CharPos::new(),
				descr: format!("{}", err),
				inner: InnerErr::Func (err),
			},
			FuncErr::NotDefined { .. } => InterpErr {
				pos_begin: CharPos::new(),
				pos_end: CharPos::new(),
				descr: format!("{}", err),
				inner: InnerErr::Func (err),
			},
			FuncErr::AlreadyDefined { .. } => InterpErr {
				pos_begin: CharPos::new(),
				pos_end: CharPos::new(),
				descr: format!("{}", err),
				inner: InnerErr::Func (err),
			},
		}
	}
}

//------------------------ Tests --------------------

#[cfg(test)]
mod tests {
	use super::*;
	
	#[test]
	fn check_err_if_redeclare_variable() {
		let mut int = Interpreter::new();
		
		match int.check_and_run("var a: f32 = 3;") {
			Ok(InterpInnerSignal::CanContinue) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
		
		match int.check_and_run("var a: f32 = 4;") {
			Err(InterpErr { inner: InnerErr::Var(VarErr::AlreadyExists { name }), .. } ) if name == String::from("a") => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}
	
	#[test]
	fn check_err_if_set_var_to_data_of_wrong_type() {
		let mut int = Interpreter::new();
		
		match int.check_and_run("var a: f32 = \"hello\";") {
			Err(InterpErr { inner: InnerErr::Var(VarErr::WrongType { 
				value_data_type: DataType::String, 
				var_data_type: DataType::Float32,
			}), .. } ) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
		
		match int.check_and_run("var a: str = 4;") {
			Err(InterpErr { inner: InnerErr::Var(VarErr::WrongType { 
				value_data_type: DataType::Float32, 
				var_data_type: DataType::String,
			}), .. } ) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}

	#[test]
	fn builtin_functions() {
		let mut int = Interpreter::new();
		
		match int.check_and_run("@print(\"hello\");") {
			Ok(InterpInnerSignal::CanContinue) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
		
		match int.check_and_run("@print(3);") {
			Ok(InterpInnerSignal::CanContinue) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
		
		match int.check_and_run("@print(False);") {
			Ok(InterpInnerSignal::CanContinue) => {},
			res @ _ => panic!("Wrong result: {:?}", res),
		}
	}
}