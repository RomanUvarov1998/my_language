use super::tokens_iter::*;
use super::InterpErr;

#[derive(Debug, Eq, PartialEq)]
pub struct Expr {
	node_stack: NodeStack,
}

impl Expr {
	pub fn new(tokens_iter: &mut TokensIter) -> Result<Self, InterpErr> {
		let mut node_stack = NodeStack::new();
		let mut bin_op_stack = ArithmeticalOpStack::new();
		let mut opened_brackets_cnt = 0_u32;
		
		loop {
			match tokens_iter.peek()? {
				Some(token_ref) => match token_ref {
					Token::StatementOp (..) => break,
					_ => {
						let token = tokens_iter.next().unwrap().unwrap();
						Self::add_token(
								&mut node_stack, 
								&mut bin_op_stack, 
								&mut opened_brackets_cnt, 
								token)?
					},
				},
				None => break,
			}
		}
		
		Self::complete(&mut node_stack, &mut bin_op_stack)?;
		
		Ok( Self { node_stack } )
	}		
	
	pub fn calc(&self, memory: &Memory) -> Result<VarValue, InterpErr> {
		match self.node_stack.last() {
			Some(..) => {
				let (res, _) = self.calc_op(self.node_stack.len() - 1, memory)?;
				Ok(VarValue::Float32(res))
			},
			None => Err(InterpErr::from(ExprError::Empty)),
		}
	}
	
	fn add_token(
		node_stack: &mut NodeStack, 
		bin_op_stack: &mut ArithmeticalOpStack, 
		opened_brackets_cnt: &mut u32, 
		token: Token
	) -> Result<(), ExprError> {		
		match token {
			Token::ArithmeticalOp (bin_op) => {
				let cur_bin_op = RankedArithmeticalOp::new(bin_op, *opened_brackets_cnt);
				
				match bin_op_stack.last() {
					Some(last_bin_op) => if cur_bin_op.rank <= last_bin_op.rank {
						node_stack.push_all_ge_than_from(bin_op_stack, cur_bin_op);
					},
					None => {
						bin_op_stack.push(cur_bin_op);
						return Ok(());
					},
				};
				
				 
				bin_op_stack.push(cur_bin_op);
			},
			Token::Number (val) => node_stack.push_number(val),
			Token::Bracket (br) => match br {
				Bracket::Left => {
					*opened_brackets_cnt += 1;
				},
				Bracket::Right => {
					if *opened_brackets_cnt == 0 {
						return Err( ExprError::UnexpectedToken (token) );
					}
					*opened_brackets_cnt -= 1;
				},
			},
			Token::Name (name) => node_stack.push_var_name(name),
			_ => return Err( ExprError::UnexpectedToken (token) ),
		}
		
		Ok(())
	}
	
	fn complete(
		node_stack: &mut NodeStack, 
		bin_op_stack: &mut ArithmeticalOpStack
	) -> Result<(), ExprError> {
		while let Some(last_bin_op) = bin_op_stack.pop() {
			node_stack.push( Node::ArithmeticalOp ( last_bin_op ) );
		}
		Ok(())
	}
	
	fn calc_op(&self, tok_ind: usize, memory: &Memory) -> Result<(f32, usize), InterpErr> {
		match self.node_stack[tok_ind] {
			Node::Number (val) => Ok( ( val, tok_ind ) ),
			Node::Variable { ref name } => {
				let var_value: &VarValue = memory.get_variable(name)?;
				match var_value {
					VarValue::Float32(value) => Ok( ( *value, tok_ind ) ),
					//_ => Err( InterpErr::from(ExprError::WrongType (format!("{:?}", var_value)) ) ),
				}
			},
			Node::ArithmeticalOp (bin_op) => {
				let (val1, pos) = self.calc_op(tok_ind - 1, memory)?;
				let (val2, pos) = self.calc_op(pos - 1, memory)?;
				let res: f32 = match bin_op.op {
					ArithmeticalOp::Plus => val2 + val1,
					ArithmeticalOp::Minus => val2 - val1,
					ArithmeticalOp::Mul => val2 * val1,
					ArithmeticalOp::Div => val2 / val1,
				};
				Ok( ( res, pos ) )
			},
		}
	}
}

impl std::fmt::Display for Expr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		writeln!(f, "Expr {{")?;
		writeln!(f, "\t node_stack: [")?;
		for token in self.node_stack.iter() {
			writeln!(f, "\t\t{:?}", token)?;
		}
		writeln!(f, "]}}")
	}
}

#[derive(Clone, Copy, Eq, PartialEq)]
struct RankedArithmeticalOp {
	op: ArithmeticalOp,
	rank: u32,
}
impl RankedArithmeticalOp {
	fn new(op: ArithmeticalOp, opened_brackets_cnt: u32) -> Self {
		const BRACKET_RANK: u32 = 2;
		let mut rank = match op {
			ArithmeticalOp::Plus | ArithmeticalOp::Minus => 1,
			ArithmeticalOp::Mul | ArithmeticalOp::Div => 2,
		};
		rank += opened_brackets_cnt * BRACKET_RANK;
		Self { op, rank }
	}
}
impl std::fmt::Debug for RankedArithmeticalOp {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "(")?;
		
		match self.op {
			ArithmeticalOp::Plus => write!(f, "Plus")?,
			ArithmeticalOp::Minus => write!(f, "Minus")?,
			ArithmeticalOp::Mul => write!(f, "Mul")?,
			ArithmeticalOp::Div => write!(f, "Div")?,
		}
		
		write!(f, "[{}]", self.rank)?;
		 
		write!(f, ")")
    }
}

enum Node {
	Number (f32),
	ArithmeticalOp (RankedArithmeticalOp), 
	Variable { name: String },
}
impl Eq for Node {}
impl PartialEq for Node {
	fn eq(&self, other: &Self) -> bool {
		match self {
			Node::Number (f1) => match other {
				Node::Number (f2) => (f1 - f2).abs() <= std::f32::EPSILON,
				_ => false,
			},
			Node::ArithmeticalOp (op1) => match other {
				Node::ArithmeticalOp (op2) => op1 == op2,
				_ => false,
			},
			Node::Variable { ref name } => {
				let name2 = if let Node::Variable { ref name } = other {
					name
				} else {
					return false;
				};
				name == name2
			},
		}
	}
}
impl std::fmt::Debug for Node {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Node::Number (val) => write!(f, "[Value {}]", val),
			Node::ArithmeticalOp (op) => write!(f, "[Op {:?}]", op),
			Node::Variable { ref name } => write!(f, "[Var '{}']", name),
		}
    }
}

use std::ops::{Deref, DerefMut};

#[derive(Debug, Eq, PartialEq)]
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
	fn push_bin_op(&mut self, op: RankedArithmeticalOp) {
		self.inner.push( Node::ArithmeticalOp (op) );
	}
	fn push_var_name(&mut self, name: String) {
		self.inner.push( Node::Variable { name } );
	}
	fn push_all_ge_than_from(&mut self, bin_op_stack: &mut ArithmeticalOpStack, bin_op: RankedArithmeticalOp) {
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
struct ArithmeticalOpStack {
	inner: Vec<RankedArithmeticalOp>,
}
impl ArithmeticalOpStack {
	fn new() -> Self {
		Self { inner: Vec::new() }
	}
}
impl Deref for ArithmeticalOpStack {
    type Target = Vec<RankedArithmeticalOp>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl DerefMut for ArithmeticalOpStack {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

#[derive(Debug)]
pub enum ExprError {
	UnexpectedToken (Token),
	#[allow(unused)]
	WrongType (String),
	Empty,
}

use super::memory::*;

impl std::fmt::Display for ExprError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ExprError::UnexpectedToken (token) => write!(f, "Unexpected token '{:?}'", token),
			ExprError::WrongType (var_value) => write!(f, "Wrong operand type: '{:?}'", var_value),
			ExprError::Empty => write!(f, "No tokens to make an expression"),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::super::statements_iter::*;
	use super::super::tokens_iter::*;
	use super::super::chars_iter::CharsIter;
	use super::*;
	
	#[test]
	pub fn can_make_variable_declare_init_statement() {
		let tokens_iter = TokensIter::new(CharsIter::new("var a: f32 = 0.3 + 0.5;"));	
		let mut statements_iter = StatementsIter::new(tokens_iter);
		
		let st = statements_iter.next().unwrap().unwrap();
		assert_eq!(st, Statement::WithVariable ( 
				WithVariable::DeclareSet {
					var_name: String::from("a"), 
					var_type: VarType::Float32, 
					value_expr: Expr {
						node_stack: NodeStack {
							inner: vec![
								Node::Number (0.3),
								Node::Number (0.5),
								Node::ArithmeticalOp (RankedArithmeticalOp { op: ArithmeticalOp::Plus, rank: 1_u32 }),
							]
						}
					},
				} 
			) 
		);
		
		assert!(statements_iter.next().unwrap().is_none());
	}

	#[test]
	pub fn can_make_variable_set_statement() {
		let tokens_iter = TokensIter::new(CharsIter::new("a = 0.3 + 0.5;"));	
		let mut statements_iter = StatementsIter::new(tokens_iter);
		
		let st = statements_iter.next().unwrap().unwrap();
		assert_eq!(st, Statement::WithVariable ( 
				WithVariable::Set {
					var_name: String::from("a"), 
					value_expr: Expr {
						node_stack: NodeStack {
							inner: vec![
								Node::Number (0.3),
								Node::Number (0.5),
								Node::ArithmeticalOp (RankedArithmeticalOp { op: ArithmeticalOp::Plus, rank: 1_u32 }),
							]
						}
					},
				} 
			) 
		);
		
		assert!(statements_iter.next().unwrap().is_none());
	}

	#[test]
	pub fn can_parse_expression_with_variables() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("a + 2 + b / c"));
		
		let expr = Expr::new(&mut tokens_iter).unwrap();
		assert_eq!(
			expr, 
			Expr {
				node_stack: NodeStack {
					inner: vec![
						Node::Variable { name: String::from("a") },
						Node::Number (2_f32),
						Node::ArithmeticalOp (RankedArithmeticalOp { op: ArithmeticalOp::Plus, rank: 1_u32 }),
						Node::Variable { name: String::from("b") },
						Node::Variable { name: String::from("c") },
						Node::ArithmeticalOp (RankedArithmeticalOp { op: ArithmeticalOp::Div, rank: 2_u32 }),
						Node::ArithmeticalOp (RankedArithmeticalOp { op: ArithmeticalOp::Plus, rank: 1_u32 }),
					]
				}
			},
		);
	}
}