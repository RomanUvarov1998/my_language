use super::parser::Parser;
use super::lexer::Lexer;

pub struct Interpreter<'code> {
	code: &'code str
}

impl<'code> Interpreter<'code> {
	pub fn new(code: &'code str) -> Self {
		Self { code }
	}
	
	pub fn process(&self) -> Result<(), InterpErr> {
		let parser = Parser::new(&self.code)?;
		let lexer = Lexer::new(parser.tokens())?;	
		println!("Result: {}", lexer.calc()?);		
		Ok(())
	}
}

pub struct InterpErr {
	msg: String
}

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

use super::parser::TokConstructErr;
impl From<TokConstructErr> for InterpErr {
	fn from(err: TokConstructErr) -> InterpErr {
        InterpErr::new(format!("{}", err))
    }
}

use super::lexer::LexerErr;
impl From<LexerErr> for InterpErr {
	fn from(err: LexerErr) -> InterpErr {
        InterpErr::new(format!("{}", err))
    }
}