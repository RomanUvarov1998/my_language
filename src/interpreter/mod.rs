mod memory;
mod expr;
mod chars_iter;
mod tokens_iter;
mod statements_iter;

#[cfg(test)]
mod tests;

use chars_iter::CharsIter;
use tokens_iter::*;
use statements_iter::*;
use memory::*;

pub struct Interpreter {
	memory: Memory,
}

impl Interpreter {
	pub fn new() -> Self {
		Self { 
			memory: Memory::new(),
		}
	}
	
	pub fn run(&mut self, code: &str) -> Result<(), InterpErr> {
		let chars_iter = CharsIter::new(code);
		let tokens_iter = TokensIter::new(chars_iter);
		let mut statements_iter = StatementsIter::new(tokens_iter);
		loop {
			match statements_iter.next()? {
				Some(st) => match st {
					Statement::WithVariable (st) => match st {
						WithVariable::Declare { var_name, var_type } => 
							self.memory.add_variable(var_name, var_type)?,
						WithVariable::DeclareSet { var_name, var_type, value_expr } => {
							self.memory.add_variable(var_name.clone(), var_type)?;
							self.memory.set_variable(&var_name, value_expr.calc(&self.memory)?)?;
						},
						WithVariable::Set { var_name, value_expr } => 
							self.memory.set_variable(&var_name, value_expr.calc(&self.memory)?)?,
					}
				},
				None => break,
			}
		}	
		
		Ok(())
	}
}

#[derive(Debug)]
pub struct InterpErr { msg: String }

impl InterpErr {
	pub fn new(msg: String) -> Self {
		Self { msg }
	}
}

impl std::fmt::Display for InterpErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "Error occured: {}", &self.msg)
	}
}

use tokens_iter::TokConstructErr;
impl From<TokConstructErr> for InterpErr {
	fn from(err: TokConstructErr) -> InterpErr {
        InterpErr::new(format!("{}", err))
    }
}

use expr::ExprError;
impl From<ExprError> for InterpErr {
	fn from(err: ExprError) -> InterpErr {
        InterpErr::new(format!("{}", err))
    }
}

impl From<StatementErr> for InterpErr {
	fn from(err: StatementErr) -> InterpErr {
        InterpErr::new(format!("{}", err))
    }
}

impl From<VarErr> for InterpErr {
	fn from(err: VarErr) -> InterpErr {
		match err {
			VarErr::NotDefined { name } => 
				InterpErr::new(format!("Variable '{}' is not defined", &name)),
			VarErr::NotSet { name } => 
				InterpErr::new(format!("Variable '{}' is not set", &name)),
			VarErr::UnknownType { name } => 
				InterpErr::new(format!("Unknown type '{}'", &name)),
			VarErr::AlreadyExists { name } => 
				InterpErr::new(format!("Already exists '{}'", &name)),
			VarErr::WrongType { new_value, var_type } =>
				InterpErr::new(format!("Wrong value '{:?}' for type '{:?}'", new_value, var_type)),
		}
    }
}