use super::tokens_iter::*;
use super::expr::Expr;
use super::{ InterpErr, memory::VarType };

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
				Token::Name( var_name ) => self.parse_variable_set(var_name)?,
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
		let var_type = VarType::parse(&type_name)?;
		
		let tok_assign = self.tokens_iter.next()?;
		match tok_assign {
			Some( Token::AssignOp ) => {},
			Some( Token::StatementOp ( StatementOp::Semicolon ) ) => return 
				Ok ( Statement::WithVariable ( WithVariable::Declare {
					var_name, 
					var_type, 
				}
			) ),
			_ => return Err( InterpErr::new( format!("Expected '=' but found {:?}", tok_assign) ) ),
		};
		
		let value_expr = Expr::new(&mut self.tokens_iter)?;
		
		let tok_semicolon = self.tokens_iter.next()?;
		match tok_semicolon {
			Some( Token::StatementOp ( StatementOp::Semicolon ) ) => {},
			_ => return Err( InterpErr::new( format!("Expected ';' but found {:?}", tok_semicolon) ) ),
		};
		
		Ok ( Statement::WithVariable ( WithVariable::DeclareSet {
				var_name, 
				var_type, 
				value_expr,
			}
		) )
	}
	
	fn parse_variable_set(&mut self, var_name: String) -> Result<Statement, InterpErr> {		
		let tok_assign = self.tokens_iter.next()?;
		match tok_assign {
			Some( Token::AssignOp ) => {},
			_ => return Err( InterpErr::new( format!("Expected '=' but found {:?}", tok_assign) ) ),
		};
		
		let value_expr = Expr::new(&mut self.tokens_iter)?;
		
		let tok_semicolon = self.tokens_iter.next()?;
		match tok_semicolon {
			Some( Token::StatementOp ( StatementOp::Semicolon ) ) => {},
			_ => return Err( InterpErr::new( format!("Expected ';' but found {:?}", tok_semicolon) ) ),
		};
		
		Ok ( Statement::WithVariable ( WithVariable::Set {
				var_name, 
				value_expr,
			}
		) )
	}
}

#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
	WithVariable (WithVariable),
}

#[derive(Debug, Eq, PartialEq)]
pub enum WithVariable {
	Declare { var_name: String, var_type: VarType },
	DeclareSet { var_name: String, var_type: VarType, value_expr: Expr },
	Set { var_name: String, value_expr: Expr },
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