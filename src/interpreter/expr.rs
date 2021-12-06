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
		let mut prev_tok_content = Option::<PrevTokenContentType>::None;
		
		loop {
			let next_token_ref = tokens_iter.peek_or_err()?;
			
			match next_token_ref.content() {
				TokenContent::StatementOp (..) => break,
				TokenContent::Bracket (Bracket::Right) if opened_brackets_cnt == 0 => break,
				_ => {
					let token = tokens_iter.next().unwrap()?;
					
					let cur_tok = PrevTokenContentType::from_token_content(token.content());					
					if let (Some(ref t1), Some(t2)) = (&prev_tok_content, &cur_tok) {
						if t1 == t2 {
							return Err( InterpErr::Expr ( ExprErr::UnexpectedToken (token) ) );
						}
					}
					prev_tok_content = cur_tok;
					
					Self::add_token(
							&mut node_stack, 
							&mut bin_op_stack, 
							&mut opened_brackets_cnt, 
							token)?
				},
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
			None => Err(InterpErr::from(ExprErr::Empty)),
		}
	}
		
	fn add_token(
		node_stack: &mut NodeStack, 
		bin_op_stack: &mut ArithmeticalOpStack, 
		opened_brackets_cnt: &mut u32, 
		token: Token
	) -> Result<(), ExprErr> {	
		match token {
			Token { pos_begin, pos_end, content: TokenContent::ArithmeticalOp (bin_op) } => {
				let cur_bin_op = RankedArithmeticalOp::new(pos_begin, pos_end, bin_op, *opened_brackets_cnt);
				
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
			Token { content: TokenContent::Number (val), .. } => node_stack.push_number(val),
			Token { content: TokenContent::Bracket (br), .. } => match br {
				Bracket::Left => {
					*opened_brackets_cnt += 1;
				},
				Bracket::Right => {
					if *opened_brackets_cnt == 0 {
						return Err( ExprErr::UnexpectedToken (token) );
					}
					*opened_brackets_cnt -= 1;
				},
			},
			Token { content: TokenContent::Name (name), .. } => node_stack.push_var_name(name),
			found @ _ => return Err( ExprErr::UnexpectedToken (found) ),
		}
		
		Ok(())
	}
	
	fn complete(
		node_stack: &mut NodeStack, 
		bin_op_stack: &mut ArithmeticalOpStack
	) -> Result<(), ExprErr> {
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
					//_ => Err( InterpErr::from(ExprErr::WrongType (format!("{:?}", var_value)) ) ),
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

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct RankedArithmeticalOp {
	pos_begin: usize, 
	pos_end: usize,
	op: ArithmeticalOp,
	rank: u32,
}
impl RankedArithmeticalOp {
	fn new(pos_begin: usize, pos_end: usize, op: ArithmeticalOp, opened_brackets_cnt: u32) -> Self {
		const BRACKET_RANK: u32 = 2;
		let mut rank = match op {
			ArithmeticalOp::Plus | ArithmeticalOp::Minus => 1,
			ArithmeticalOp::Mul | ArithmeticalOp::Div => 2,
		};
		rank += opened_brackets_cnt * BRACKET_RANK;
		Self { pos_begin, pos_end, op, rank }
	}
}

#[derive(Debug, PartialEq, Eq)]
enum PrevTokenContentType { 
	Operand,
	Operator,
}

impl PrevTokenContentType {
	fn from_token_content(tc: &TokenContent) -> Option<Self> {
		match tc {
			TokenContent::Number (_) 
				| TokenContent::Name (_) => Some( PrevTokenContentType::Operand ),
			TokenContent::ArithmeticalOp (_) 
				| TokenContent::LogicalOp (_) => Some( PrevTokenContentType::Operator ),
			TokenContent::Bracket (_)
				| TokenContent::AssignOp 
				| TokenContent::StatementOp (_) 
				| TokenContent::Keyword (_) => None,
		}
	}
}

#[derive(Debug)]
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

#[derive(Debug, PartialEq, Eq)]
pub enum ExprErr {
	UnexpectedToken (Token),
	#[allow(unused)]
	WrongType (String),
	Empty,
}

use super::memory::*;

impl std::fmt::Display for ExprErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ExprErr::UnexpectedToken (token) => {
				super::display_error_pos(f, token.pos_begin, token.pos_end)?;
				write!(f, "Unexpected token {}", token)
			},
			ExprErr::WrongType (type_name) => write!(f, "Wrong operand type: '{:?}'", type_name),
			ExprErr::Empty => write!(f, "No tokens to make an expression"),
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
	fn can_make_variable_declare_init_statement() {
		let tokens_iter = TokensIter::new(CharsIter::new("var a: f32 = 0.3 + 0.5;"));	
		let mut statements_iter = StatementsIter::new(tokens_iter);
		
		let st = statements_iter.next().unwrap().unwrap();
		assert_eq!(st, Statement::WithVariable ( 
				WithVariable::DeclareSet {
					var_name: String::from("a"), 
					data_type: DataType::Float32, 
					value_expr: Expr {
						node_stack: NodeStack {
							inner: vec![
								Node::Number (0.3),
								Node::Number (0.5),
								Node::ArithmeticalOp (RankedArithmeticalOp { pos_begin: 17, pos_end: 17, op: ArithmeticalOp::Plus, rank: 1_u32 }),
							]
						}
					},
				} 
			) 
		);
		
		assert!(statements_iter.next().is_none());
	}

	#[test]
	fn can_make_variable_set_statement() {
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
								Node::ArithmeticalOp (RankedArithmeticalOp { pos_begin: 8, pos_end: 8, op: ArithmeticalOp::Plus, rank: 1_u32 }),
							]
						}
					},
				} 
			) 
		);
		
		assert!(statements_iter.next().is_none());
	}

	#[test]
	fn can_parse_expression_with_variables() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("a + 2 + b / c;"));
		
		let expr = Expr::new(&mut tokens_iter).unwrap();
		assert_eq!(
			expr, 
			Expr {
				node_stack: NodeStack {
					inner: vec![
						Node::Variable { name: String::from("a") },
						Node::Number (2_f32),
						Node::ArithmeticalOp (RankedArithmeticalOp { pos_begin: 2, pos_end: 2, op: ArithmeticalOp::Plus, rank: 1_u32 }),
						Node::Variable { name: String::from("b") },
						Node::Variable { name: String::from("c") },
						Node::ArithmeticalOp (RankedArithmeticalOp { pos_begin: 10, pos_end: 10, op: ArithmeticalOp::Div, rank: 2_u32 }),
						Node::ArithmeticalOp (RankedArithmeticalOp { pos_begin: 6, pos_end: 6, op: ArithmeticalOp::Plus, rank: 1_u32 }),
					]
				}
			},
		);
	}
	
	#[test]
	fn can_detect_expression_syntax_error_repeating_number() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("2 4;"));
		
		assert_eq!(
			Expr::new(&mut tokens_iter),
			Err (
				InterpErr::Expr ( ExprErr::UnexpectedToken ( Token {
					pos_begin: 2,
					pos_end: 2,
					content: TokenContent::Number ( 4_f32 ),
				} ) )
			)
		);
	}
	
	#[test]
	fn can_detect_expression_syntax_error_repeating_arithmetical_op() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("2 + +;"));
		
		assert_eq!(
			Expr::new(&mut tokens_iter),
			Err (
				InterpErr::Expr ( ExprErr::UnexpectedToken ( Token {
					pos_begin: 4,
					pos_end: 4,
					content: TokenContent::ArithmeticalOp ( ArithmeticalOp::Plus ),
				} ) )
			)
		);
	}

	#[test]
	fn can_parse_print_call() {
		let tokens_iter = TokensIter::new(CharsIter::new(r"
		var a: f32;
		a = 0.3 + 0.5;
		print(a);
		"));	
		let mut statements_iter = StatementsIter::new(tokens_iter);
		
		println!("----------------------------------");
		let st = statements_iter.next().unwrap().unwrap();
		assert_eq!(st, Statement::WithVariable ( 
				WithVariable::Declare {
					var_name: String::from("a"), 
					data_type: DataType::Float32,
				} 
			) 
		);
		
		println!("----------------------------------");
		let st = statements_iter.next().unwrap().unwrap();
		assert_eq!(st, Statement::WithVariable ( 
				WithVariable::Set {
					var_name: String::from("a"), 
					value_expr: Expr {
						node_stack: NodeStack {
							inner: vec![
								Node::Number (0.3),
								Node::Number (0.5),
								Node::ArithmeticalOp (RankedArithmeticalOp { pos_begin: 25, pos_end: 25, op: ArithmeticalOp::Plus, rank: 1_u32 }),
							]
						}
					},
				} 
			) 
		);
		
		println!("----------------------------------");
		let st = statements_iter.next().unwrap().unwrap();
		assert_eq!(
			st, 
			Statement::FuncCall { 
				name: String::from("print"), 
				args: vec![
					Expr {
						node_stack: NodeStack {
							inner: vec![
								Node::Variable { name: String::from("a") },
							]
						}
					},
				]
			});
		
		println!("----------------------------------");
		assert!(statements_iter.next().is_none());
	}
}