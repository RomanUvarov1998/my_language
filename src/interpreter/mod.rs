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
						WithVariable::Declare { var_name, data_type } => 
							self.memory.add_variable(var_name, data_type)?,
						WithVariable::DeclareSet { var_name, data_type, value_expr } => {
							self.memory.add_variable(var_name.clone(), data_type)?;
							self.memory.set_variable(&var_name, value_expr.calc(&self.memory)?)?;
						},
						WithVariable::Set { var_name, value_expr } => 
							self.memory.set_variable(&var_name, value_expr.calc(&self.memory)?)?,
					},
					Statement::FuncCall { name, args } => 
							self.memory.call_func(name, args)?,
				},
				None => break,
			}
		}	
		
		Ok(())
	}
}

#[derive(Debug, PartialEq, Eq)]
pub enum InterpErr {
	Token (TokenErr),
	Expr (ExprErr),
	Statement (StatementErr),
	Var (VarErr),
	Func (FuncErr),
}

impl std::fmt::Display for InterpErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {		
		match self {
			InterpErr::Token (err) => write!(f, "{}", err),
			InterpErr::Expr (err) => write!(f, "{}", err),
			InterpErr::Statement (err) => write!(f, "{}", err),
			InterpErr::Var (err) => write!(f, "{}", err),
			InterpErr::Func (err) => write!(f, "{}", err),
		}
	}
}

fn display_error_pos(f: &mut std::fmt::Formatter<'_>, pos_begin: usize, pos_end: usize) -> std::fmt::Result {
	for _ in 0..(pos_begin - 1) {
		write!(f, "_")?;
	}
	for _ in (pos_begin - 1)..pos_end {
		write!(f, "^")?;
	}
	writeln!(f, "")
}

use tokens_iter::TokenErr;
impl From<TokenErr> for InterpErr {
	fn from(err: TokenErr) -> InterpErr {
        InterpErr::Token(err)
    }
}

use expr::ExprErr;
impl From<ExprErr> for InterpErr {
	fn from(err: ExprErr) -> InterpErr {
        InterpErr::Expr(err)
    }
}

impl From<StatementErr> for InterpErr {
	fn from(err: StatementErr) -> InterpErr {
        InterpErr::Statement(err)
    }
}

impl From<VarErr> for InterpErr {
	fn from(err: VarErr) -> InterpErr {
		InterpErr::Var(err)
    }
}

impl From<FuncErr> for InterpErr {
	fn from(err: FuncErr) -> InterpErr {
        InterpErr::Func(err)
    }
}