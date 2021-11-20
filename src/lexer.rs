use super::parser::{ Token, TokensIter };
use super::expr::Expr;
use super::interpreter::InterpErr;

pub struct Lexer {
	expr: Expr,
}

impl Lexer {
	pub fn new(mut tokens_iter: TokensIter) -> Result<Self, InterpErr> {
		let mut expr = Expr::new();
		
		while let Some(token) = tokens_iter.next_token()? {
			expr.add_node(token)?;
		}
		expr.complete()?;
		
		Ok( Self { expr } )
	}
	
	pub fn calc(&self) -> Result<f32, LexerErr> {
		self.expr.calc()
	}
}

impl std::fmt::Display for Lexer {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "[{}]", self.expr)
	}
}

#[derive(Debug)]
pub enum LexerErr {
	UnexpectedToken(Token),
	Empty,
}

impl std::fmt::Display for LexerErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			LexerErr::UnexpectedToken(tok) => write!(f, "Unexpected token {:?}", tok),
			LexerErr::Empty => write!(f, "Expression is empty"),
		}
	}
}