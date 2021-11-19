use super::parser::{Token, BinOp};
use super::lexer::LexerErr;

#[derive(Debug)]
pub struct Tree {
	main_stack: Vec<Token>,
	temp_stack: Vec<BinOp>,
}

impl Tree {
	pub fn new() -> Self {
		Self {
			main_stack: Vec::<Token>::new(),
			temp_stack: Vec::<BinOp>::new(),
		}
	}
	
	pub fn add_node(&mut self, token: Token) -> Result<(), LexerErr> {	
		use std::cmp::Ordering;		
		match token {
			Token::BinOp (bin_op) => {
				match self.temp_stack.last() { // last_bin_op
					Some(last_bin_op) => {
						match bin_op.cmp(last_bin_op) {
							Ordering::Less | Ordering::Equal => {
								let last_bin_op_token = Token::BinOp ( self.temp_stack.pop().unwrap() );
								self.main_stack.push(last_bin_op_token);
								self.temp_stack.push(bin_op);
							},
							Ordering::Greater => self.temp_stack.push(bin_op),
						};
					},
					None => self.temp_stack.push(bin_op),
				}
			},
			Token::Number (..) => self.main_stack.push(token),
		}
		
		Ok(())
	}
	
	pub fn complete(&mut self) -> Result<(), LexerErr> {
		while let Some(last_bin_op) = self.temp_stack.pop() {
			self.main_stack.push( Token::BinOp ( last_bin_op ) );
		}
		Ok(())
	}		
	
	pub fn calc(&self) -> Result<f32, LexerErr> {
		match self.main_stack.last() {
			Some(..) => {
				let (res, _) = self.calc_op(self.main_stack.len() - 1)?;
				Ok(res)
			},
			None => Err(LexerErr::Empty),
		}
	}
	fn calc_op(&self, tok_ind: usize) -> Result<(f32, usize), LexerErr> {
		match self.main_stack[tok_ind] {
			Token::Number (val) => Ok( ( val, tok_ind ) ),
			Token::BinOp (bin_op) => {
				let (val1, pos) = self.calc_op(tok_ind - 1)?;
				let (val2, pos) = self.calc_op(pos - 1)?;
				let res: f32 = match bin_op {
					BinOp::Plus => val2 + val1,
					BinOp::Minus => val2 - val1,
					BinOp::Mul => val2 * val1,
					BinOp::Div => val2 / val1,
				};
				Ok( ( res, pos ) )
			}
		}
	}
}

impl std::fmt::Display for Tree {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		writeln!(f, "Tree {{")?;
		writeln!(f, "\t main_stack: {:?},", self.main_stack)?;
		writeln!(f, "\t temp_stack: {:?},", self.temp_stack)?;
		writeln!(f, "}}")
	}
}

trait Node: std::fmt::Display {
	fn calc(&self) -> f32;
}