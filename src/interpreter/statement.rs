use super::token::*;
use super::arithmetic_expr::{ArithmeticExpr, ExprContextKind};
use super::{ InterpErr, var_data::DataType };

pub struct StatementsIter<'code> {
	tokens_iter: TokensIter<'code>
}

impl<'code> StatementsIter<'code> {
	pub fn new(tokens_iter: TokensIter<'code>) -> Self {
		StatementsIter { tokens_iter }
	}
	
	fn parse_variable_set_or_func_call(&mut self, name: String, is_builtin: bool) -> Result<Statement, InterpErr> {
		let second = self.tokens_iter.next_or_err()?;
		let statement = match second {
			Token { content: TokenContent::Bracket(Bracket::Left), .. } => 
				self.parse_func_call(name, is_builtin)?,
				
			Token { content: TokenContent::Operator (Operator::Assign), .. } => 
				self.parse_variable_set(name, is_builtin)?,
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
	
	fn parse_variable_set(&mut self, var_name: String, is_builtin: bool) -> Result<Statement, InterpErr> {		
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

	fn parse_func_call(&mut self, func_name: String, is_builtin: bool) -> Result<Statement, InterpErr> {
		let mut arg_exprs = Vec::<ArithmeticExpr>::new();
		
		loop {
			if let TokenContent::Bracket (Bracket::Right) = self.tokens_iter.peek_or_err()?.content() {
				self.tokens_iter.next_or_err()?;
				break;
			}
			
			arg_exprs.push(ArithmeticExpr::new(
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
			kind: if is_builtin { FuncKind::Builtin } else { FuncKind::UserDefined },
			name: func_name, 
			arg_exprs,
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
				self.parse_variable_set_or_func_call(name, false),
			
			Token { content: TokenContent::BuiltinName( name ), .. } =>
				self.parse_variable_set_or_func_call(name, true),
				
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

//------------------- Statement --------------------

#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
	WithVariable (WithVariable),
	FuncCall { 
		kind: FuncKind,
		name: String, 
		arg_exprs: Vec<ArithmeticExpr> 
	},
}

#[derive(Debug, Eq, PartialEq)]
pub enum WithVariable {
	Declare { var_name: String, data_type: DataType },
	DeclareSet { var_name: String, data_type: DataType, value_expr: ArithmeticExpr },
	Set { var_name: String, value_expr: ArithmeticExpr },
}

#[derive(Debug, Eq, PartialEq)]
pub enum FuncKind {
	UserDefined,
	Builtin,
}

//------------------- StatementErr --------------------

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
	use super::super::string_char::*;
	use super::super::token::*;
	use super::*;
	use super::super::var_data::VarValue;
	use super::super::memory::Memory;

	#[test]
	fn can_parse_builtin_funcs_call() {
		let tokens_iter = TokensIter::new(CharsIter::new("@print(1.2 + 3.4);"));
		let mut st_iter = StatementsIter::new(tokens_iter);
		
		match st_iter.next().unwrap().unwrap() {
			Statement::FuncCall {
				kind: FuncKind::Builtin,
				name, 
				arg_exprs
			} => {
				assert_eq!("print", &name);
				
				assert_eq!(arg_exprs.len(), 1);
				
				let mem = Memory::new();
				
				match arg_exprs[0].calc(&mem).unwrap() {
					VarValue::Float32 (val) => assert!((val - (1.2_f32 + 3.4_f32)).abs() <= std::f32::EPSILON * 2.0),
					ans @ _ => panic!("Wrong answer {:?}", ans),
				}
			},
			st @ _ => panic!("Wrong pattern {:?}", st),
		};
		
		assert_eq!(
			st_iter.next(),
			None
		);
	}
}