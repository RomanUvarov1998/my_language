use super::tokens_iter::{self, *};
use super::InterpErr;

#[derive(Debug, PartialEq, Eq)]
pub struct ArithmeticExpr {
	root: Node,
}

type TokSym = (Token, Symbol);

impl ArithmeticExpr {	
	pub fn new(tokens_iter: &mut TokensIter) -> Result<Self, InterpErr> {
		let expr_stack = Self::create_stack(tokens_iter)?;
		
		let root = Self::create_tree(expr_stack)?;
		
		Ok( Self { root } )
	}
	
	fn create_stack(tokens_iter: &mut TokensIter) -> Result<Vec<TokSym>, InterpErr> {
		let mut tmp_stack = Vec::<TokSym>::new();
		let mut expr_stack = Vec::<TokSym>::new();
		let mut prev_is_operand = false;
		
		loop {
			let next_token_ref = tokens_iter.peek_or_err()?;
			
			if let TokenContent::StatementOp (..) = next_token_ref.content() {		
				break;
			}
			
			let token = dbg!(tokens_iter.next().unwrap()?);
			
			match token.content() {	
				TokenContent::Number (num) => {
					if prev_is_operand {
						return Err( InterpErr::ArithmExpr( ArithmExprErr::UnexpectedToken (token) ) );
					}					
					
					let sym = Symbol::new_number(*num);
					expr_stack.push( (token, sym) );
					
					prev_is_operand = true;
				},
				TokenContent::Name (name) => {
					if prev_is_operand {
						return Err( InterpErr::ArithmExpr( ArithmExprErr::UnexpectedToken (token) ) );
					}					
					
					let sym = Symbol::new_name(name.clone());
					expr_stack.push((token, sym));
					
					prev_is_operand = true;
				},
				
				TokenContent::Operator (op) => {
					let tok_op: tokens_iter::Operator = *op;
					match tok_op {
						tokens_iter::Operator::Plus | tokens_iter::Operator::Minus => {
							if prev_is_operand {
								Self::add_bin_op(&mut expr_stack, &mut tmp_stack, token, tok_op)?;
							} else {
								tmp_stack.push( (token, Symbol::new_un_pref_op(tok_op)) );
							}
						},
						tokens_iter::Operator::Mul | tokens_iter::Operator::Div => {
								Self::add_bin_op(&mut expr_stack, &mut tmp_stack, token, tok_op)?;
						},
						tokens_iter::Operator::Equals | tokens_iter::Operator::Assign 
							=> return Err( InterpErr::ArithmExpr( ArithmExprErr::UnexpectedToken (token) ) ),
					};
					
					prev_is_operand = false;
				},
				
				TokenContent::Bracket (br) => {
					println!("here");
					let tok_br: tokens_iter::Bracket = *br;
					println!("adding bracket {:?}", tok_br);
					Self::add_bracket(&mut expr_stack, &mut tmp_stack, token, tok_br)?;
					
					prev_is_operand = false;
				},
				
				TokenContent::Keyword (..) => {
					return Err( InterpErr::ArithmExpr( ArithmExprErr::UnexpectedToken (token) ) );
				},
				
				TokenContent::StatementOp (..) => unreachable!(),
			}
				
			println!("tmp_stack: {:?}", tmp_stack);
		}
		
		while let Some(top_tok_sym) = tmp_stack.pop() {
			match top_tok_sym.1 {
				Symbol::Operand (..) => expr_stack.push(top_tok_sym),
				Symbol::LeftBracket => return Err( InterpErr::ArithmExpr( ArithmExprErr::UnexpectedToken (top_tok_sym.0) ) ),
				Symbol::ArithmOperator (..) => expr_stack.push(top_tok_sym),
			}
		}
		
		Ok(expr_stack)
	}
	
	fn add_bin_op(
		expr_stack: &mut Vec<TokSym>, 
		tmp_stack: &mut Vec<TokSym>, 
		tok: Token, op: tokens_iter::Operator
		) -> Result<(), InterpErr> 
	{
		let ar_op = ArithmOperator::new_bin(op);
		
		loop {
			let top_tok_sym: TokSym = 
				match tmp_stack.last() {
					Some(top_ref) => 
						match &top_ref.1 {
							Symbol::Operand (..) => {  
								return Err( InterpErr::ArithmExpr( ArithmExprErr::UnexpectedToken (
									tmp_stack.pop().unwrap().0) ) )
							},
								
							Symbol::LeftBracket => {
								break;
							},
							
							Symbol::ArithmOperator ( top_op_ref ) => 
								match top_op_ref {
									ArithmOperator::Binary (..) => 
										if top_op_ref.rank() >= ar_op.rank() {
											tmp_stack.pop().unwrap()
										} else {
											break;
										},
									ArithmOperator::UnaryPrefix (..) => tmp_stack.pop().unwrap(),
								}
						},
					None => break,
				};
			
			expr_stack.push(top_tok_sym);
		}
			
		tmp_stack.push((tok, Symbol::ArithmOperator (ar_op) ));
		
		Ok(())
	}
	
	fn add_bracket(
		expr_stack: &mut Vec<TokSym>, 
		tmp_stack: &mut Vec<TokSym>, 
		tok: Token, br: tokens_iter::Bracket
	) -> Result<(), InterpErr> 
	{
		match br {
			Bracket::Left => {
				tmp_stack.push( (tok, Symbol::new_left_bracket()) );
				println!("push_left");
			},
			Bracket::Right => {
				println!("get_right");
				'out: loop {
					match tmp_stack.pop() {
						Some( tok_sym ) => match tok_sym.1 {
								Symbol::LeftBracket => break 'out,
								Symbol::Operand (..) | Symbol::ArithmOperator (..) => expr_stack.push(tok_sym),
							},
						None => return Err( InterpErr::ArithmExpr( ArithmExprErr::UnpairedBracket (tok) ) ),
					};
				};
			},
		};
		
		Ok(())
	}

	fn create_tree(expr_stack: Vec<TokSym>) -> Result<Node, InterpErr> {
		let (root, begin_ind) = Self::create_node(&expr_stack, expr_stack.len() - 1)?;
		assert!(begin_ind == 0);
		Ok(root)
	}
	
	fn create_node(expr_stack: &Vec<TokSym>, local_root_ind: usize) -> Result<(Node, usize), InterpErr> {		
		match &expr_stack[local_root_ind].1 {
			Symbol::Operand (operand) => Ok( (Node::Operand(operand.clone()), local_root_ind) ),
			
			Symbol::LeftBracket => unreachable!(),
			
			Symbol::ArithmOperator (op_ref) => {				
				let (node, node_begin_ind): (Node, usize) = match op_ref {
					ArithmOperator::Binary (bin_op_ref) => {
						let (rhs_node, rhs_begin_ind) = Self::create_node(&expr_stack, local_root_ind - 1)?;
						let (lhs_node, lhs_begin_ind) = Self::create_node(&expr_stack, rhs_begin_ind - 1)?;
						
						(Node::BinOp {
							lhs: Box::new(lhs_node),
							operator: *bin_op_ref,
							rhs: Box::new(rhs_node),
						}, lhs_begin_ind)
					},
						
					ArithmOperator::UnaryPrefix (un_pref_op_ref) => {
						let (operand_node, operand_begin_ind) = Self::create_node(&expr_stack, local_root_ind - 1)?;
						
						(Node::UnPrefOp {
							operand: Box::new(operand_node),
							operator: *un_pref_op_ref,
						}, operand_begin_ind)
					}
				};
				
				Ok((node, node_begin_ind))
			},
		}
	}
}

//------------------------------- Symbol ----------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
enum Symbol {
	Operand (Operand),
	LeftBracket,
	ArithmOperator (ArithmOperator),
}

impl Symbol {
	fn new_number(num: f32) -> Self {
		Symbol::Operand( Operand::Number (num) )
	}
	fn new_name(name: String) -> Self {
		Symbol::Operand( Operand::Name (name) )
	}
	fn new_left_bracket() -> Self {
		Symbol::LeftBracket
	}
	fn new_bin_op(op: tokens_iter::Operator) -> Self {
		Symbol::ArithmOperator( ArithmOperator::new_bin(op) )
	}
	fn new_un_pref_op(op: tokens_iter::Operator) -> Self {
		Symbol::ArithmOperator( ArithmOperator::new_un_pref(op) )
	}
}

#[derive(Debug, Clone)]
enum Operand {
	Number (f32),
	Name (String),
}
impl Eq for Operand {}
impl PartialEq for Operand {
	fn eq(&self, other: &Self) -> bool {
		match self {
			Operand::Number (f1) => match other {
				Operand::Number (f2) => (f1 - f2).abs() <= std::f32::EPSILON * 2_f32,
				_ => false,
			},
			Operand::Name (op1) => match other {
				Operand::Name (op2) => op1 == op2,
				_ => false,
			},
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ArithmOperator {
	Binary (BinOp),
	UnaryPrefix (UnPrefOp),
}

impl ArithmOperator {
	fn new_bin(op: tokens_iter::Operator) -> Self {
		let ar_op: BinOp = match op {
			tokens_iter::Operator::Plus => BinOp::Plus,
			tokens_iter::Operator::Minus => BinOp::Minus,
			tokens_iter::Operator::Mul => BinOp::Mul,
			tokens_iter::Operator::Div => BinOp::Div,
			tokens_iter::Operator::Equals | tokens_iter::Operator::Assign => unreachable!(),
		};
		ArithmOperator::Binary (ar_op)
	}
	
	fn new_un_pref(op: tokens_iter::Operator) -> Self {
		let ar_op: UnPrefOp = match op {
			tokens_iter::Operator::Plus => UnPrefOp::Plus,
			tokens_iter::Operator::Minus => UnPrefOp::Minus,
			tokens_iter::Operator::Mul 
				| tokens_iter::Operator::Div 
				| tokens_iter::Operator::Equals 
				| tokens_iter::Operator::Assign 
				=> unreachable!(),
		};
		ArithmOperator::UnaryPrefix (ar_op)
	}
	
	fn rank(&self) -> u32 {
		match self {
			ArithmOperator::Binary (bin_op) => match bin_op {
				BinOp::Plus | BinOp::Minus => 0_u32,
				BinOp::Div | BinOp::Mul => 1_u32,
			},
			ArithmOperator::UnaryPrefix (un_op) => match un_op {
				UnPrefOp::Plus | UnPrefOp::Minus => 2_u32,
			},
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BinOp {
	Plus,
	Minus,
	Div,
	Mul,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UnPrefOp {
	Plus,
	Minus,
}

//------------------------------- Node ----------------------------------

#[derive(Debug, PartialEq, Eq)]
enum Node {
	BinOp {
		lhs: Box<Node>,
		operator: BinOp,
		rhs: Box<Node>,
	},
	UnPrefOp {
		operand: Box<Node>,
		operator: UnPrefOp,
	},
	Operand (Operand),
}

impl Node {		
	fn calc(&self, memory: &super::memory::Memory) -> Result<f32, InterpErr> {
		let res: f32 = match self {
			Node::BinOp { lhs, operator, rhs } => match operator {
				BinOp::Plus => 	lhs.calc(memory)? + rhs.calc(memory)?,
				BinOp::Minus => lhs.calc(memory)? - rhs.calc(memory)?,
				BinOp::Div => 	lhs.calc(memory)? / rhs.calc(memory)?,
				BinOp::Mul => 	lhs.calc(memory)? * rhs.calc(memory)?,
			},
			Node::UnPrefOp { operand, operator } => match operator {
				UnPrefOp::Plus => 	operand.calc(memory)?,
				UnPrefOp::Minus => 	-operand.calc(memory)?,
			},
			Node::Operand (operand) => match operand {
				Operand::Number (val) => *val,
				Operand::Name (name) => {
					use super::memory::VarValue;
					let var_value: &VarValue = memory.get_variable(name)?;
					match var_value {
						VarValue::Float32(value) => *value,
					}
				},
			},
		};
		
		Ok(res)
	}
}

//------------------------------- ArithmExprErr ----------------------------------

#[derive(Debug, PartialEq, Eq)]
pub enum ArithmExprErr {
	UnexpectedToken (Token),
	UnpairedBracket (Token),
	NpOperandForOperator (Token),
	WrongType (String),
	Empty,
}

impl std::fmt::Display for ArithmExprErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ArithmExprErr::UnexpectedToken (token) => {
				super::display_error_pos(f, token.pos_begin, token.pos_end)?;
				write!(f, "Unexpected token {}", token)
			},
			ArithmExprErr::UnpairedBracket (token) => {
				super::display_error_pos(f, token.pos_begin, token.pos_end)?;
				write!(f, "Unpaired bracket {}", token)
			},
			ArithmExprErr::NpOperandForOperator  (token) => {
				super::display_error_pos(f, token.pos_begin, token.pos_end)?;
				write!(f, "No operand for operator")
			},
			ArithmExprErr::WrongType (type_name) => write!(f, "Wrong operand type: '{:?}'", type_name),
			ArithmExprErr::Empty => write!(f, "No tokens to make an expression"),
		}
	}
}

//------------------------------- Tests ----------------------------------

#[cfg(test)]
mod tests {
	use super::super::statements_iter::*;
	use super::super::tokens_iter::*;
	use super::super::chars_iter::CharsIter;
	use super::*;
	
	#[test]
	fn check_stack_creation() {
		test_expr_and_its_stack_eq("3.125;", vec![
			Symbol::Operand (Operand::Number (3.125_f32)),
		]);
		test_expr_and_its_stack_eq("3.125 + 5.4;", vec![
			Symbol::Operand (Operand::Number (3.125_f32)),
			Symbol::Operand (Operand::Number (5.4_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Plus)),
		]);
		test_expr_and_its_stack_eq("3.125 + 5.4 * 2.46;", vec![
			Symbol::Operand (Operand::Number (3.125_f32)),
			Symbol::Operand (Operand::Number (5.4_f32)),
			Symbol::Operand (Operand::Number (2.46_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Mul)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Plus)),
		]);
		test_expr_and_its_stack_eq("3.125 + 0 + 5.4 * 2.46 - 3.14 / 2 * 4.9;", vec![
			Symbol::Operand (Operand::Number (3.125_f32)),
			Symbol::Operand (Operand::Number (0_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Plus)),
			Symbol::Operand (Operand::Number (5.4_f32)),
			Symbol::Operand (Operand::Number (2.46_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Mul)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Plus)),
			Symbol::Operand (Operand::Number (3.14_f32)),
			Symbol::Operand (Operand::Number (2_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Div)),
			Symbol::Operand (Operand::Number (4.9_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Mul)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Minus)),
		]);
		test_expr_and_its_stack_eq("3.125 + -5.4 * 2.46;", vec![
			Symbol::Operand (Operand::Number (3.125_f32)),
			Symbol::Operand (Operand::Number (5.4_f32)),
			Symbol::ArithmOperator (ArithmOperator::UnaryPrefix (UnPrefOp::Minus)),
			Symbol::Operand (Operand::Number (2.46_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Mul)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Plus)),
		]);
		test_expr_and_its_stack_eq("2.34 * ---5.67;", vec![
			Symbol::Operand (Operand::Number (2.34_f32)),
			Symbol::Operand (Operand::Number (5.67_f32)),
			Symbol::ArithmOperator (ArithmOperator::UnaryPrefix (UnPrefOp::Minus)),
			Symbol::ArithmOperator (ArithmOperator::UnaryPrefix (UnPrefOp::Minus)),
			Symbol::ArithmOperator (ArithmOperator::UnaryPrefix (UnPrefOp::Minus)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Mul)),
		]);
		test_expr_and_its_stack_eq("1.23 * (3.45 + 2.34);", vec![
			Symbol::Operand (Operand::Number (1.23_f32)),
			Symbol::Operand (Operand::Number (3.45_f32)),
			Symbol::Operand (Operand::Number (2.34_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Plus)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Mul)),
		]);
		test_expr_and_its_stack_eq("-(8.79 - 2.34 * 5.67 + 4.56) / -3.4;", vec![
			Symbol::Operand (Operand::Number (8.79_f32)),
			Symbol::Operand (Operand::Number (2.34_f32)),
			Symbol::Operand (Operand::Number (5.67_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Mul)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Minus)),
			Symbol::Operand (Operand::Number (4.56_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Plus)),
			Symbol::ArithmOperator (ArithmOperator::UnaryPrefix (UnPrefOp::Minus)),
			Symbol::Operand (Operand::Number (3.4_f32)),
			Symbol::ArithmOperator (ArithmOperator::UnaryPrefix (UnPrefOp::Minus)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Div)),
		]);
	}
	
	fn test_expr_and_its_stack_eq(expr_str: &str, correct_expr_stack: Vec<Symbol>) {
		let mut tokens_iter = TokensIter::new(CharsIter::new(expr_str));	
		let expr_stack: Vec<(Token, Symbol)> = ArithmeticExpr::create_stack(&mut tokens_iter).unwrap();
		
		let syms_expr_stack: Vec<Symbol> = expr_stack.iter().map(|(_tok, sym)| sym.clone()).collect();
		
		if syms_expr_stack == correct_expr_stack { return; }
		
		let mut has_error = false;
		
		let max_len = std::cmp::max(expr_stack.len(), syms_expr_stack.len());
		for i in 0..max_len {
			match syms_expr_stack.get(i) {
				Some(sym) => print!("{:?}", sym),
				None => print!("None"),
			}
			match syms_expr_stack.get(i) == correct_expr_stack.get(i) {
				true => print!(" == "),
			false => {
				print!(" != "); 
				has_error = true; 
			},
			}
			match correct_expr_stack.get(i) {
				Some(sym) => println!("{:?}", sym),
				None => println!("None"),
			}
		}
		
		if has_error {
			panic!("Error ^^^^");
		}
	}
	
	#[test]	
	fn can_parse_single_number() {
		let mut tokens_iter = TokensIter::new(CharsIter::new("3.125;"));	
		let ar_expr: ArithmeticExpr = ArithmeticExpr::new(&mut tokens_iter).unwrap();
		assert_eq!(
			ar_expr,
			ArithmeticExpr {
				root: Node::Operand (Operand::Number (3.125_f32))
			}
		);
	}
}