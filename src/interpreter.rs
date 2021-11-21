use super::chars_iter::CharsIter;
use super::tokens_iter::*;
use super::statements_iter::*;

pub struct Interpreter {}

impl Interpreter {
	pub fn new() -> Self {
		Self { }
	}
	
	pub fn process(&mut self, code: &str) -> Result<(), InterpErr> {
		let chars_iter = CharsIter::new(code);
		let tokens_iter = TokensIter::new(chars_iter);
		let mut statements_iter = StatementsIter::new(tokens_iter);
		
		loop {
			match statements_iter.next()? {
				Some(st) => println!("{:?}", st),
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

use super::tokens_iter::TokConstructErr;
impl From<TokConstructErr> for InterpErr {
	fn from(err: TokConstructErr) -> InterpErr {
        InterpErr::new(format!("{}", err))
    }
}

use super::expr::ExprError;
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