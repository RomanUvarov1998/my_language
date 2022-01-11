use super::tokens_iter::*;
use super::arithmetic_expr::{ArithmeticExpr, ExprContextKind};
use super::{ InterpErr, memory::DataType };

pub struct StatementsIter<'code> {
	tokens_iter: TokensIter<'code>
}

impl<'code> StatementsIter<'code> {
	pub fn new(tokens_iter: TokensIter<'code>) -> Self {
		StatementsIter { tokens_iter }
	}
	
	fn parse_variable_set_or_func_call(&mut self, name: String) -> Result<Statement, InterpErr> {
		let second = self.tokens_iter.next_or_err()?;
		let statement = match second {
			Token { content: TokenContent::Bracket(Bracket::Left), .. } => 
				self.parse_func_call(name)?,
			Token { content: TokenContent::Operator (Operator::Assign), .. } => 
				self.parse_variable_set(name)?,
			_ => return Err( InterpErr::Token ( TokenErr::ExpectedButFound { 
					expected: vec![
						TokenContent::Bracket(Bracket::Left),
						TokenContent::Operator (Operator::Assign),
					], 
					found: second 
				} ) ),
		};
		Ok(statement)
	}
	
	fn parse_varable_declaration(&mut self) -> Result<Statement, InterpErr> {	
		let var_name = self.tokens_iter.expect_name()?;
		
		self.tokens_iter.expect(TokenContent::StatementOp ( StatementOp::Colon ))?.check()?;
		
		let type_name = self.tokens_iter.expect_name()?;
		let data_type = DataType::parse(&type_name)?;
		
		let tok_assign = self.tokens_iter.next_or_err()?;
		match tok_assign {
			Token { content: TokenContent::Operator (Operator::Assign), .. } => {},
			Token { content: TokenContent::StatementOp ( StatementOp::Semicolon ), .. } => return 
				Ok ( Statement::WithVariable ( WithVariable::Declare {
					var_name, 
					data_type, 
				}
			) ),
			found @ _ => return Err( InterpErr::Token( TokenErr::ExpectedButFound { 
							expected: vec![
								TokenContent::Operator (Operator::Assign),
								TokenContent::StatementOp ( StatementOp::Semicolon ),
							], 
							found
						} ) ),
		};
		
		let value_expr = ArithmeticExpr::new(
			&mut self.tokens_iter, 
			ExprContextKind::ValueToAssign)?;
		
		
		self.tokens_iter.expect(TokenContent::StatementOp ( StatementOp::Semicolon ))?.check()?;
		
		Ok ( Statement::WithVariable ( WithVariable::DeclareSet {
				var_name, 
				data_type, 
				value_expr,
			}
		) )
	}
	
	fn parse_variable_set(&mut self, var_name: String) -> Result<Statement, InterpErr> {		
		let value_expr = ArithmeticExpr::new(
			&mut self.tokens_iter,
			ExprContextKind::ValueToAssign)?;
		
		self.tokens_iter.expect(TokenContent::StatementOp ( StatementOp::Semicolon ))?.check()?;
		
		Ok ( Statement::WithVariable ( WithVariable::Set {
				var_name, 
				value_expr,
			}
		) )
	}

	fn parse_func_call(&mut self, func_name: String) -> Result<Statement, InterpErr> {
		let mut exprs = Vec::<ArithmeticExpr>::new();
		
		loop {
			exprs.push(ArithmeticExpr::new(
				&mut self.tokens_iter,
				ExprContextKind::FunctionArg)?);
			match self.tokens_iter.peek_or_err()? {
				&Token { content: TokenContent::StatementOp ( StatementOp::Comma ), .. } => {
					self.tokens_iter.next_or_err()?;
				},
				&Token { content: TokenContent::Bracket ( Bracket::Right ), .. } => {
					self.tokens_iter.next_or_err()?;
					break;
				},
				_ => {
					let found = self.tokens_iter.next_or_err()?;
					return Err( InterpErr::Token( TokenErr::ExpectedButFound { 
							expected: vec![
								TokenContent::StatementOp(StatementOp::Comma),
								TokenContent::Bracket ( Bracket::Right ),
							], 
							found
						} ) );
				},
			}
		}
		
		self.tokens_iter.expect(TokenContent::StatementOp ( StatementOp::Semicolon ))?.check()?;
		
		Ok ( Statement::FuncCall {
			name: func_name, 
			args: exprs,
		} )
	}
}

impl Iterator for StatementsIter<'_> {
	type Item = Result<Statement, InterpErr>;
	
	fn next(&mut self) -> Option<Self::Item> {
		let first = match self.tokens_iter.next()? {
			Err(err) => match err {
				TokenErr::EndReached { .. } => return None,
				_ => return Some( Err( InterpErr::from(err) ) ),
			},
			Ok(token) => token,
		};
		
		let statement_result = match first {
			Token { content: TokenContent::Keyword ( Keyword::Var ), .. } => 
				self.parse_varable_declaration(),	
			Token { content: TokenContent::Name( name ), .. } => 
				self.parse_variable_set_or_func_call(name),
			found @ _ => 
				Err( InterpErr::Token( TokenErr::ExpectedButFound { 
					expected: vec![
						TokenContent::Keyword ( Keyword::Var ),
						TokenContent::Name( String::from("<name>") ),
					], 
					found
				} ) ),
		};
		
		Some(statement_result)
	}	
}

#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
	WithVariable (WithVariable),
	FuncCall { name: String, args: Vec<ArithmeticExpr> },
}

#[derive(Debug, Eq, PartialEq)]
pub enum WithVariable {
	Declare { var_name: String, data_type: DataType },
	DeclareSet { var_name: String, data_type: DataType, value_expr: ArithmeticExpr },
	Set { var_name: String, value_expr: ArithmeticExpr },
}

#[derive(Debug, PartialEq, Eq)]
pub enum StatementErr {
	#[allow(unused)]
	EndReached,
}

impl std::fmt::Display for StatementErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			StatementErr::EndReached => writeln!(f, "End reached"),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::super::chars_iter::*;
	use super::super::tokens_iter::*;
	use super::*;

	#[test]
	fn can_parse_builtin_funcs_call() {
		let tokens_iter = TokensIter::new(CharsIter::new("print(1.2 + 3.4);"));
		let mut st_iter = StatementsIter::new(tokens_iter);
		
		assert_eq!(
			st_iter.next().unwrap().unwrap(),
			Statement::FuncCall {
				name: String::from("print"), 
				args: vec![
					ArithmeticExpr::new(
						&mut TokensIter::new(CharsIter::new("1.2 + 3.4);")),
						ExprContextKind::FunctionArg).unwrap(),
				]
			},
		);
		
		assert_eq!(
			st_iter.next(),
			None
		);
	}
}