use super::tokens_iter::*;
use super::expr::Expr;
use super::interpreter::InterpErr;

pub struct StatementsIter<'code> {
	tokens_iter: TokensIter<'code>
}

impl<'code> StatementsIter<'code> {
	pub fn new(tokens_iter: TokensIter<'code>) -> Self {
		StatementsIter { tokens_iter }
	}
	
	pub fn next(&mut self) -> Result<Option<Statement>, InterpErr> {
		let first = self.tokens_iter.next()?;
		let st: Statement = match first {
			Some ( token ) => match token {
				Token::Keyword ( Keyword::Var ) => self.parse_varable_declaration()?,				
				_ => return Err( InterpErr::new( format!("Unexpected token {:?}", token) ) ),
			},
			None => return Ok( None ),
		};
		Ok (  Some ( st ) )
	}
	
	fn parse_varable_declaration(&mut self) -> Result<Statement, InterpErr> {	
		let tok_var_name = self.tokens_iter.next()?;
		let var_name: String = match tok_var_name {
			Some( Token::Name ( name ) ) => name,
			_ => return Err( InterpErr::new( format!("Expected variable name but found {:?}", tok_var_name) ) ),
		};
		
		let tok_colon = self.tokens_iter.next()?;
		match tok_colon {
			Some( Token::StatementOp ( StatementOp::Colon ) ) => {},
			_ => return Err( InterpErr::new( format!("Expected ':' but found {:?}", tok_colon) ) ),
		};
		
		let tok_type_name = self.tokens_iter.next()?;
		let type_name: String = match tok_type_name {
			Some( Token::Name ( name ) ) => name,
			_ => return Err( InterpErr::new( format!("Expected type name but found {:?}", tok_type_name) ) ),
		};
		let var_type = VarType::from(&type_name)?;
		
		let tok_assign = self.tokens_iter.next()?;
		match tok_assign {
			Some( Token::AssignOp ) => {},
			_ => return Err( InterpErr::new( format!("Expected '=' but found {:?}", tok_assign) ) ),
		};
		
		let expr = Expr::new(&mut self.tokens_iter)?;
		
		let tok_semicolon = self.tokens_iter.next()?;
		match tok_semicolon {
			Some( Token::StatementOp ( StatementOp::Semicolon ) ) => {},
			_ => return Err( InterpErr::new( format!("Expected ';' but found {:?}", tok_semicolon) ) ),
		};
		
		Ok ( Statement::WithVariable ( WithVariable::DeclareInitialize {
				var_name, 
				var_type, 
				value: VarValue::Float32 ( expr.calc()? ),
			}
		) )
	}
}

#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
	WithVariable (WithVariable),
	VarOp,
	FuncCall,
	QuitCmd,
}

#[derive(Debug, Eq, PartialEq)]
pub enum WithVariable {
	//Declare { name: String, var_type: VarType },
	DeclareInitialize { var_name: String, var_type: VarType, value: VarValue },
	//Set,
}

#[derive(Debug, Eq, PartialEq)]
pub enum VarType {
	Float32,
}
impl VarType {
	fn from(code: &str) -> Result<Self, StatementErr> {
		match code {
			"f32" => Ok( VarType::Float32 ),
			_ => Err( StatementErr::new( format!("Unknown type name {:?}", code) ) ),
		}
	}
}

#[derive(Debug)]
pub enum VarValue {
	Float32 (f32),
}
impl Eq for VarValue {}
impl PartialEq for VarValue {
	fn eq(&self, other: &Self) -> bool {
		match self {
			VarValue::Float32 (f1) => match other {
				VarValue::Float32 (f2) => (f1 - f2).abs() <= std::f32::EPSILON,
			},
		}
	}
}

#[derive(Debug)]
pub struct StatementErr { msg: String, }

impl StatementErr {
	pub fn new(msg: String) -> Self {
		Self { msg }
	}
}

impl std::fmt::Display for StatementErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		writeln!(f, "Statement error: {}", &self.msg)
	}
}