use super::parser::{Token, BinOp, Bracket};
use super::lexer::LexerErr;

#[derive(Debug)]
pub struct Expr {
	node_stack: NodeStack,
	bin_op_stack: BinOpStack,
	opened_brackets_cnt: u32,
}
		
#[derive(Clone, Copy)]
struct RankedBinOp {
	op: BinOp,
	rank: u32,
}
impl RankedBinOp {
	fn new(op: BinOp, opened_brackets_cnt: u32) -> Self {
		const BRACKET_RANK: u32 = 2;
		let mut rank = match op {
			BinOp::Plus | BinOp::Minus => 1,
			BinOp::Mul | BinOp::Div => 2,
		};
		rank += opened_brackets_cnt * BRACKET_RANK;
		Self { op, rank }
	}
}
impl std::fmt::Debug for RankedBinOp {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "(")?;
		
		match self.op {
			BinOp::Plus => write!(f, "Plus")?,
			BinOp::Minus => write!(f, "Minus")?,
			BinOp::Mul => write!(f, "Mul")?,
			BinOp::Div => write!(f, "Div")?,
		}
		
		write!(f, "[{}]", self.rank)?;
		 
		write!(f, ")")
    }
}

#[derive(Clone, Copy)]
enum Node {
	Number (f32),
	BinOp (RankedBinOp), 
}
impl std::fmt::Debug for Node {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Node::Number (val) => write!(f, "({})", val),
			Node::BinOp (op) => write!(f, "{:?}", op),
		}
    }
}

impl Expr {
	pub fn new() -> Self {
		Self {
			node_stack: NodeStack::new(),
			bin_op_stack: BinOpStack::new(),
			opened_brackets_cnt: 0,
		}
	}
	
	pub fn add_node(&mut self, token: Token) -> Result<(), LexerErr> {		
		match token {
			Token::BinOp (bin_op) => {
				let cur_bin_op = RankedBinOp::new(bin_op, self.opened_brackets_cnt);
				
				match self.bin_op_stack.last() {
					Some(last_bin_op) => if cur_bin_op.rank <= last_bin_op.rank {
						self.node_stack.push_all_ge_than_from(&mut self.bin_op_stack, cur_bin_op);
					},
					None => {
						self.bin_op_stack.push(cur_bin_op);
						return Ok(());
					},
				};
				
				 
				self.bin_op_stack.push(cur_bin_op);
			},
			Token::Number (val) => self.node_stack.push_number( val ),
			Token::Bracket (br) => match br {
				Bracket::Left => {
					self.opened_brackets_cnt += 1;
					println!("(, {}", self.opened_brackets_cnt);
				},
				Bracket::Right => {
					if self.opened_brackets_cnt == 0 {
						return Err( LexerErr::UnexpectedToken (token) );
					}
					self.opened_brackets_cnt -= 1;
					println!("), {}", self.opened_brackets_cnt);
				},
			}
		}
		
		Ok(())
	}
	
	pub fn complete(&mut self) -> Result<(), LexerErr> {
		while let Some(last_bin_op) = self.bin_op_stack.pop() {
			self.node_stack.push( Node::BinOp ( last_bin_op ) );
		}
		Ok(())
	}		
	
	pub fn calc(&self) -> Result<f32, LexerErr> {
		match self.node_stack.last() {
			Some(..) => {
				let (res, _) = self.calc_op(self.node_stack.len() - 1)?;
				Ok(res)
			},
			None => Err(LexerErr::Empty),
		}
	}
	fn calc_op(&self, tok_ind: usize) -> Result<(f32, usize), LexerErr> {
		match self.node_stack[tok_ind] {
			Node::Number (val) => Ok( ( val, tok_ind ) ),
			Node::BinOp (bin_op) => {
				let (val1, pos) = self.calc_op(tok_ind - 1)?;
				let (val2, pos) = self.calc_op(pos - 1)?;
				let res: f32 = match bin_op.op {
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

impl std::fmt::Display for Expr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		writeln!(f, "Expr {{")?;
		writeln!(f, "\t node_stack: {:?},", self.node_stack)?;
		writeln!(f, "\t bin_op_stack: {:?},", self.bin_op_stack)?;
		writeln!(f, "\t braces: {},", self.opened_brackets_cnt)?;
		writeln!(f, "}}")
	}
}

use std::ops::{Deref, DerefMut};

#[derive(Debug)]
struct NodeStack {
	inner: Vec<Node>,
}
impl NodeStack {
	fn new() -> Self {
		Self { inner: Vec::new() }
	}
	fn push_number(&mut self, val: f32) {
		self.inner.push( Node::Number (val) );
	}
	fn push_bin_op(&mut self, op: RankedBinOp) {
		self.inner.push( Node::BinOp (op) );
	}
	fn push_all_ge_than_from(&mut self, bin_op_stack: &mut BinOpStack, bin_op: RankedBinOp) {
		loop {
			match bin_op_stack.last() {
				Some(op) => 
					if op.rank >= bin_op.rank {					
						self.push_bin_op( bin_op_stack.pop().unwrap() );
					} else {
						break;
					},
				None => break,
			}
		}
	}
}
impl Deref for NodeStack {
    type Target = Vec<Node>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl DerefMut for NodeStack {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

#[derive(Debug)]
struct BinOpStack {
	inner: Vec<RankedBinOp>,
}
impl BinOpStack {
	fn new() -> Self {
		Self { inner: Vec::new() }
	}
}
impl Deref for BinOpStack {
    type Target = Vec<RankedBinOp>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl DerefMut for BinOpStack {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}
