use super::parser::Token;
use super::tree::Tree;

pub struct Lexer {
	tree: Tree,
}

impl Lexer {
	pub fn new(tokens: &Vec<Token>) -> Result<Self, LexerErr> {
		let mut tree = Tree::new();
		
		for token in tokens {
			tree.add_node(*token)?;
			println!("{}", tree);
		}
		tree.complete()?;
		
		Ok( Self { tree } )
	}
	
	pub fn calc(&self) -> Result<f32, LexerErr> {
		self.tree.calc()
	}
}

impl std::fmt::Display for Lexer {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "[{}]", self.tree)
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