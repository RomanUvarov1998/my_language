use super::parser::Token;
use super::expr::Expr;

pub struct Lexer {
	expr: Expr,
}

impl Lexer {
	pub fn new(tokens: &Vec<Token>) -> Result<Self, LexerErr> {
		let mut expr = Expr::new();
		
		for token in tokens {
			expr.add_node(*token)?;
			println!("{}", expr);
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