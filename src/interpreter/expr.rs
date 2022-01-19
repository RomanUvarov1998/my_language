use super::token::{Token, TokenContent, TokensIter, Operator, Bracket, StatementOp};
use super::InterpErr;
use super::memory::Memory;
use super::var_data::{Value, DataType};

type TokSym = (Token, Symbol);

#[derive(Debug, PartialEq, Eq)]
pub struct Expr {
	data_type: DataType,
	expr_stack: Vec<TokSym>,
}

impl Expr {	
	pub fn new(tokens_iter: &mut TokensIter, context_kind: ExprContextKind) -> Result<Self, InterpErr> {
		let context = ExprContext::new(context_kind);
		let expr_stack = Self::create_stack(tokens_iter, context)?;
		
		Ok( Self { data_type: DataType::Float32, expr_stack } )
	}
	
	pub fn get_data_type(&self) -> DataType {
		self.data_type
	}
	
	pub fn calc(&self, memory: &Memory) -> Result<Value, InterpErr> {
		let mut calc_stack = Vec::<Value>::with_capacity(self.expr_stack.len());
		let mut expr = self.expr_stack.clone();
		expr.reverse();
		
		while let Some((tok, sym)) = expr.pop() {
			match sym {
				Symbol::Operand (opnd) => {
					let value: Value = match opnd {
						Operand::Value (val) => val,
						Operand::Name (name) => memory.get_variable(&name)?.clone(),
						Operand::BuiltinName (_name) => todo!(),
					};
					calc_stack.push(value);
				},
				
				Symbol::LeftBracket => unreachable!(),
				
				Symbol::ArithmOperator (op) => {
					let value: Value = match op.apply(&mut calc_stack) {
						Err( err ) => return Err( InterpErr::from(ExprErr::Operator { err, tok }) ),
						Ok(val) => val,
					};
					calc_stack.push(value);
				},
			}
		}
		
		let result: Value = calc_stack.pop().unwrap();
		assert_eq!(calc_stack.pop(), None);
		
		Ok(result)
	}

	fn create_stack(tokens_iter: &mut TokensIter, mut context: ExprContext) -> Result<Vec<TokSym>, InterpErr> {
		let mut tmp_stack = Vec::<TokSym>::new();
		let mut expr_stack = Vec::<TokSym>::new();
		let mut prev_is_operand = false;
		
		loop {
			let next_token_ref = tokens_iter.peek_or_err()?;
			
			if context.check_expr_end(next_token_ref)? {		
				if expr_stack.len() == 0 {
					let found_token = tokens_iter.next_or_err()?;
					return Err( InterpErr::from(ExprErr::ExpectedExprButFound(found_token)) );
				}
				break;
			}
			
			let token = tokens_iter.next().unwrap()?;
			
			match token.content() {	
				TokenContent::Number (num) => {
					if prev_is_operand {
						return Err( InterpErr::Expr( ExprErr::UnexpectedToken (token) ) );
					}					
					
					let sym = Symbol::new_number(*num);
					expr_stack.push( (token, sym) );
					
					prev_is_operand = true;
				},
				TokenContent::Name (name) => {
					if prev_is_operand {
						return Err( InterpErr::Expr( ExprErr::UnexpectedToken (token) ) );
					}					
					
					let sym = Symbol::new_name(name.clone());
					expr_stack.push((token, sym));
					
					prev_is_operand = true;
				},
				TokenContent::BuiltinName (name) => {
					if prev_is_operand {
						return Err( InterpErr::Expr( ExprErr::UnexpectedToken (token) ) );
					}					
					
					let sym = Symbol::new_builtin_name(name.clone());
					expr_stack.push((token, sym));
					
					prev_is_operand = true;
				},
				
				TokenContent::Operator (op) => {
					let tok_op: Operator = *op;
					
					match tok_op {
						Operator::Plus | Operator::Minus => {
							if prev_is_operand {
								Self::add_bin_op(&mut expr_stack, &mut tmp_stack, token, tok_op)?;
							} else {
								tmp_stack.push( (token, Symbol::new_un_pref_op(tok_op)) );
							}
						},
						Operator::Mul 
							| Operator::Div 
							| Operator::Pow 
							=> {
								Self::add_bin_op(&mut expr_stack, &mut tmp_stack, token, tok_op)?;
						},
						Operator::Equals | Operator::Assign 
							=> return Err( InterpErr::Expr( ExprErr::UnexpectedToken (token) ) ),
					};
					
					prev_is_operand = false;
				},
				
				TokenContent::Bracket (br) => {
					let tok_br: Bracket = *br;
					Self::add_bracket(&mut expr_stack, &mut tmp_stack, token, tok_br)?;
					
					prev_is_operand = match tok_br {
						Bracket::Right => true,
						Bracket::Left => false,
					};
				},
				
				TokenContent::Keyword (..) => {
					return Err( InterpErr::Expr( ExprErr::UnexpectedToken (token) ) );
				},
				
				TokenContent::StatementOp (..) => unreachable!(),
			}
		}
		
		while let Some(top_tok_sym) = tmp_stack.pop() {
			match top_tok_sym.1 {
				Symbol::Operand (..) => expr_stack.push(top_tok_sym),
				Symbol::LeftBracket => return Err( InterpErr::Expr( ExprErr::UnexpectedToken (top_tok_sym.0) ) ),
				Symbol::ArithmOperator (..) => expr_stack.push(top_tok_sym),
			}
		}
				
		Ok(expr_stack)
	}
	
	fn add_bin_op(
		expr_stack: &mut Vec<TokSym>, 
		tmp_stack: &mut Vec<TokSym>, 
		tok: Token, op: Operator
		) -> Result<(), InterpErr> 
	{
		let next = ArithmOperator::new_bin(op);
		
		use std::cmp::Ordering;
		
		loop {
			let top_tok_sym: TokSym = 
				match tmp_stack.last() {
					Some(top_ref) => 
						match &top_ref.1 {
							Symbol::Operand (..) => {  
								return Err( InterpErr::Expr( ExprErr::UnexpectedToken (
									tmp_stack.pop().unwrap().0) ) )
							},
								
							Symbol::LeftBracket => break,
							
							Symbol::ArithmOperator ( top_ref ) => {
								match next.rank().cmp(&top_ref.rank()) {
									Ordering::Less => tmp_stack.pop().unwrap(),
									Ordering::Equal => match (top_ref.assot(), next.assot()) {
										(OpAssot::Right, OpAssot::Right) => break,
										_ => tmp_stack.pop().unwrap(),
									},
									Ordering::Greater => break,
								}
							},
						},
						
					None => break,
				};
			
			expr_stack.push(top_tok_sym);
		}
			
		tmp_stack.push((tok, Symbol::ArithmOperator (next) ));
		
		Ok(())
	}
	
	fn add_bracket(
		expr_stack: &mut Vec<TokSym>, 
		tmp_stack: &mut Vec<TokSym>, 
		tok: Token, br: Bracket
	) -> Result<(), InterpErr> 
	{
		match br {
			Bracket::Left => {
				tmp_stack.push( (tok, Symbol::new_left_bracket()) );
			},
			Bracket::Right => {
				'out: loop {
					match tmp_stack.pop() {
						Some( tok_sym ) => match tok_sym.1 {
								Symbol::LeftBracket => break 'out,
								Symbol::Operand (..) | Symbol::ArithmOperator (..) => expr_stack.push(tok_sym),
							},
						None => return Err( InterpErr::Expr( ExprErr::UnpairedBracket (tok) ) ),
					};
				};
			},
		};
		
		Ok(())
	}
}

//------------------------------- ExprContext ----------------------------------

#[derive(Debug, Clone, Copy)]
pub enum ExprContextKind {
	ValueToAssign,
	FunctionArg,
}

pub struct ExprContext {
	kind: ExprContextKind,
	left_brackets_count: u32,
}

impl ExprContext {
	fn new(kind: ExprContextKind)-> Self {
		Self {
			kind,
			left_brackets_count: 0_u32,
		}
	}
	
	fn check_expr_end(&mut self, tok: &Token) -> Result<bool, InterpErr> {
		match self.kind {
			ExprContextKind::ValueToAssign =>
				match tok.content() {
					TokenContent::Number (..) | 
						TokenContent::Operator (..) | 
						TokenContent::Name (..) |
						TokenContent::BuiltinName (..)
						=> Ok(false),
					TokenContent::Bracket (br) 
						=> Ok( self.check_brackets(*br) ),
					TokenContent::StatementOp (st_op) => match st_op {
						StatementOp::Colon | StatementOp::Comma 
							=> Err( InterpErr::Expr (ExprErr::UnexpectedToken (tok.clone()) ) ),
						StatementOp::Semicolon => Ok(true),
					},
					TokenContent::Keyword (..) 
						=> Err( InterpErr::Expr (ExprErr::UnexpectedToken (tok.clone()) ) ),
				},
			ExprContextKind::FunctionArg =>
				match tok.content() {
					TokenContent::Number (..) | 
						TokenContent::Operator (..) | 
						TokenContent::Name (..) |
						TokenContent::BuiltinName (..)
						=> Ok(false),
					TokenContent::Bracket (br) 
						=> Ok( self.check_brackets(*br) ),
					TokenContent::StatementOp (st_op) => match st_op {
						StatementOp::Colon 
							=> Err( InterpErr::Expr (ExprErr::UnexpectedToken (tok.clone()) ) ),
						StatementOp::Semicolon | StatementOp::Comma 
							=> Ok(true),
					},
					TokenContent::Keyword (..) 
						=> Err( InterpErr::Expr (ExprErr::UnexpectedToken (tok.clone()) ) ),
				},
		}
	}
	
	fn check_brackets(&mut self, br: Bracket) -> bool {
		match br {
			Bracket::Left => {
				self.left_brackets_count += 1;
				false
			},
			Bracket::Right => {
				if self.left_brackets_count > 0 {
					self.left_brackets_count -= 1;
					false
				} else {
					true
				}
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
		Symbol::Operand( Operand::Value (Value::Float32(num)) )
	}
	fn new_name(name: String) -> Self {
		Symbol::Operand( Operand::Name (name) )
	}
	fn new_builtin_name(name: String) -> Self {
		Symbol::Operand( Operand::BuiltinName (name) )
	}
	fn new_left_bracket() -> Self {
		Symbol::LeftBracket
	}
	fn new_un_pref_op(op: Operator) -> Self {
		Symbol::ArithmOperator( ArithmOperator::new_un_pref(op) )
	}
}

#[derive(Debug, Clone)]
enum Operand {
	Value (Value),
	Name (String),
	BuiltinName (String),
}
impl Eq for Operand {}
impl PartialEq for Operand {
	fn eq(&self, other: &Self) -> bool {
		match self {
			Operand::Value (v1) => match other {
				Operand::Value (v2) => v1 == v2,
				_ => false,
			},
			Operand::Name (op1) => match other {
				Operand::Name (op2) => op1 == op2,
				_ => false,
			},
			Operand::BuiltinName (op1) => match other {
				Operand::BuiltinName (op2) => op1 == op2,
				_ => false,
			},
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
enum ArithmOperator {
	BinPlus = 0_usize,
	BinMinus,
	Div,
	Mul,
	Pow,
	UnPlus,
	UnMinus,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct OpInfo {
	arity: OpArity,
	rank: u32,
	assot: OpAssot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OpArity {
	Binary,
	Unary,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OpAssot {
	Left,
	Right,
}

static OP_ATTRS: [OpInfo; 7] = [
	OpInfo { arity: OpArity::Binary, rank: 0, assot: OpAssot::Left },		//BinPlus
	OpInfo { arity: OpArity::Binary, rank: 0, assot: OpAssot::Left }, 	//BinMinus
	OpInfo { arity: OpArity::Binary, rank: 1, assot: OpAssot::Left }, 	//Div
	OpInfo { arity: OpArity::Binary, rank: 1, assot: OpAssot::Left }, 	//Mul
	OpInfo { arity: OpArity::Binary, rank: 3, assot: OpAssot::Right }, 	//Pow
	OpInfo { arity: OpArity::Unary, rank: 2, assot: OpAssot::Left },		//UnPlus
	OpInfo { arity: OpArity::Unary, rank: 2, assot: OpAssot::Left },		//UnMinus
];

impl ArithmOperator {
	fn new_bin(op: Operator) -> Self {
		match op {
			Operator::Plus => ArithmOperator::BinPlus,
			Operator::Minus => ArithmOperator::BinMinus,
			Operator::Mul => ArithmOperator::Mul,
			Operator::Div => ArithmOperator::Div,
			Operator::Pow => ArithmOperator::Pow,
			Operator::Equals | Operator::Assign => unreachable!(),
		}
	}
	
	fn new_un_pref(op: Operator) -> Self {
		match op {
			Operator::Plus => ArithmOperator::UnPlus,
			Operator::Minus => ArithmOperator::UnMinus,
			Operator::Mul 
				| Operator::Div 
				| Operator::Equals 
				| Operator::Assign 
				| Operator::Pow 
				=> unreachable!(),
		}
	}
	
	fn rank(&self) -> u32 {
		OP_ATTRS[*self as usize].rank
	}
	
	fn assot(&self) -> OpAssot {
		OP_ATTRS[*self as usize].assot
	}

	#[allow(unreachable_patterns)]
	fn apply(&self, calc_stack: &mut Vec<Value>) -> Result<Value, OperatorErr> {		
		let create_err = |values: &[&Value]| -> OperatorErr {
			let types: Vec<DataType> = values
				.iter()
				.map(|val| val.get_type())
				.collect();
			
			OperatorErr::WrongType { 
				descr: String::from(format!(
					"Operator {:?} cannot be applied to type(-s) {:?}", 
					self, 
					&types[..]))
			}
		};
		
		use ArithmOperator::*;
		match self {
			BinPlus | BinMinus | Div | Mul | Pow => {
				let rhs: Value = calc_stack
					.pop()
					.ok_or(OperatorErr::NotEnoughOperands { 
						provided_cnt: 0,
						required_cnt: 2, 
					} )?;
				let lhs: Value = calc_stack
					.pop()
					.ok_or(OperatorErr::NotEnoughOperands { 
						provided_cnt: 1,
						required_cnt: 2, 
					} )?;
					
				let (val1, val2): (f32, f32) = match (lhs, rhs) { 
					(Value::Float32 (val1), Value::Float32 (val2)) => (val1, val2),
					ops @ _ => return Err( create_err(&[&ops.0, &ops.1]) ),
				};
			
				match self {
					BinPlus => Ok( Value::Float32(val1 + val2) ),
					BinMinus => Ok( Value::Float32(val1 - val2) ),
					Div => Ok( Value::Float32(val1 / val2) ),
					Mul => Ok( Value::Float32(val1 * val2) ),
					Pow => Ok( Value::Float32(val1.powf(val2)) ),
					_ => unreachable!(),
				}
			},
			UnPlus | UnMinus => {
				let op: Value = calc_stack
					.pop()
					.ok_or(OperatorErr::NotEnoughOperands { 
						provided_cnt: 0,
						required_cnt: 1, 
					} )?;

				let val: f32 = match op { 
					Value::Float32 (val) => val,
					_ => return Err( create_err(&[&op]) ),
				};

				match self {
					UnPlus => Ok( Value::Float32(val) ),
					UnMinus => Ok( Value::Float32(-val) ),
					_ => unreachable!(),
				}
			},
		}
	}
}

#[derive(Debug, PartialEq, Eq)]
pub enum OperatorErr {
	WrongType {
		descr: String,
	},
	NotEnoughOperands {
		provided_cnt: u32,
		required_cnt: u32,
	},
}

//------------------------------- ExprErr ----------------------------------

#[derive(Debug, PartialEq, Eq)]
pub enum ExprErr {
	UnexpectedToken (Token),
	UnpairedBracket (Token),
	ExpectedExprButFound (Token),
	Operator { 
		err: OperatorErr, 
		tok: Token 
	},
}

impl std::fmt::Display for ExprErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ExprErr::UnexpectedToken (token) => {
				super::display_error_pos(f, token.pos_begin, token.pos_end)?;
				write!(f, "Unexpected token {}", token)
			},
			ExprErr::UnpairedBracket (token) => {
				super::display_error_pos(f, token.pos_begin, token.pos_end)?;
				write!(f, "Unpaired bracket {}", token)
			},
			ExprErr::ExpectedExprButFound (token) => {
				super::display_error_pos(f, token.pos_begin, token.pos_end)?;
				write!(f, "Expected arithmetical expression, but found {}", token)
			},
			ExprErr::Operator { err, tok } => match err {
				OperatorErr::WrongType { descr } => {
					super::display_error_pos(f, tok.pos_begin, tok.pos_end)?;
					write!(f, "{}", descr)
				},
				OperatorErr::NotEnoughOperands { provided_cnt, required_cnt } => {
					super::display_error_pos(f, tok.pos_begin, tok.pos_end)?;
					write!(f, "Expected {} operand(-s) for operator {}, but found {}", required_cnt, tok, provided_cnt)
				}
			},
		}
	}
}

//------------------------------- Tests ----------------------------------

#[cfg(test)]
mod tests {
	use super::super::token::*;
	use super::*;
	
	#[test]
	fn lookup_array_initialization() {
		assert_eq!(
			OP_ATTRS[ArithmOperator::BinPlus as usize], 
			OpInfo { arity: OpArity::Binary, rank: 0, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ArithmOperator::BinMinus as usize], 
			OpInfo { arity: OpArity::Binary, rank: 0, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ArithmOperator::Div as usize], 
			OpInfo { arity: OpArity::Binary, rank: 1, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ArithmOperator::Mul as usize], 
			OpInfo { arity: OpArity::Binary, rank: 1, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ArithmOperator::Pow as usize], 
			OpInfo { arity: OpArity::Binary, rank: 3, assot: OpAssot::Right });
		assert_eq!(
			OP_ATTRS[ArithmOperator::UnPlus as usize], 
			OpInfo { arity: OpArity::Unary, rank: 2, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ArithmOperator::UnMinus as usize], 
			OpInfo { arity: OpArity::Unary, rank: 2, assot: OpAssot::Left });
	}
	
	#[test]
	fn check_stack_creation_and_calc() {
		test_expr_and_its_stack_eq("3.125;", vec![
			Symbol::Operand (Operand::Value (Value::Float32 (3.125_f32))),
		],
		3.125_f32);
		
		test_expr_and_its_stack_eq("3.125 + 5.4;", vec![
			Symbol::Operand (Operand::Value (Value::Float32 (3.125_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (5.4_f32))),
			Symbol::ArithmOperator (ArithmOperator::BinPlus),
		],
		3.125_f32 + 5.4_f32);
		
		test_expr_and_its_stack_eq("3.125 + 5.4 * 2.46;", vec![
			Symbol::Operand (Operand::Value (Value::Float32 (3.125_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (5.4_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (2.46_f32))),
			Symbol::ArithmOperator (ArithmOperator::Mul),
			Symbol::ArithmOperator (ArithmOperator::BinPlus),
		],
		3.125_f32 + 5.4_f32 * 2.46_f32);
		
		test_expr_and_its_stack_eq("3.125 + 0 + 5.25 * 2.25 - 3.25 / 2 * 4.25;", vec![
			Symbol::Operand (Operand::Value (Value::Float32 (3.125_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (0_f32))),
			Symbol::ArithmOperator (ArithmOperator::BinPlus),
			Symbol::Operand (Operand::Value (Value::Float32 (5.25_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (2.25_f32))),
			Symbol::ArithmOperator (ArithmOperator::Mul),
			Symbol::ArithmOperator (ArithmOperator::BinPlus),
			Symbol::Operand (Operand::Value (Value::Float32 (3.25_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (2_f32))),
			Symbol::ArithmOperator (ArithmOperator::Div),
			Symbol::Operand (Operand::Value (Value::Float32 (4.25_f32))),
			Symbol::ArithmOperator (ArithmOperator::Mul),
			Symbol::ArithmOperator (ArithmOperator::BinMinus),
		],
		3.125_f32 + 0.0_f32 + 5.25_f32 * 2.25_f32 - 3.25_f32 / 2.0_f32 * 4.25_f32);
		
		test_expr_and_its_stack_eq("3.125 + -5.25 * 2.25;", vec![
			Symbol::Operand (Operand::Value (Value::Float32 (3.125_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (5.25_f32))),
			Symbol::ArithmOperator (ArithmOperator::UnMinus),
			Symbol::Operand (Operand::Value (Value::Float32 (2.25_f32))),
			Symbol::ArithmOperator (ArithmOperator::Mul),
			Symbol::ArithmOperator (ArithmOperator::BinPlus),
		],
		3.125_f32 + -5.25_f32 * 2.25_f32);
		
		test_expr_and_its_stack_eq("2.5 * ---5.5;", vec![
			Symbol::Operand (Operand::Value (Value::Float32 (2.5_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (5.5_f32))),
			Symbol::ArithmOperator (ArithmOperator::UnMinus),
			Symbol::ArithmOperator (ArithmOperator::UnMinus),
			Symbol::ArithmOperator (ArithmOperator::UnMinus),
			Symbol::ArithmOperator (ArithmOperator::Mul),
		],
		2.5_f32 * ---5.5_f32);
		
		test_expr_and_its_stack_eq("1.125 * (3.125 + 2.125);", vec![
			Symbol::Operand (Operand::Value (Value::Float32 (1.125_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (3.125_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (2.125_f32))),
			Symbol::ArithmOperator (ArithmOperator::BinPlus),
			Symbol::ArithmOperator (ArithmOperator::Mul),
		],
		1.125_f32 * (3.125_f32 + 2.125_f32));
		
		test_expr_and_its_stack_eq("33 + (1 + 2 * (3 + 4) + 5) / 10 - 30;", vec![
			Symbol::Operand (Operand::Value (Value::Float32 (33_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (1_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (2_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (3_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (4_f32))),
			Symbol::ArithmOperator (ArithmOperator::BinPlus),
			Symbol::ArithmOperator (ArithmOperator::Mul),
			Symbol::ArithmOperator (ArithmOperator::BinPlus),
			Symbol::Operand (Operand::Value (Value::Float32 (5_f32))),
			Symbol::ArithmOperator (ArithmOperator::BinPlus),
			Symbol::Operand (Operand::Value (Value::Float32 (10_f32))),
			Symbol::ArithmOperator (ArithmOperator::Div),
			Symbol::ArithmOperator (ArithmOperator::BinPlus),
			Symbol::Operand (Operand::Value (Value::Float32 (30_f32))),
			Symbol::ArithmOperator (ArithmOperator::BinMinus),
		],
		33_f32 + (1_f32 + 2_f32 * (3_f32 + 4_f32) + 5_f32) / 10_f32 - 30_f32);
		
		test_expr_and_its_stack_eq("-(8 - 2.125 * 5.125 + 4.125) / -3.125;", vec![
			Symbol::Operand (Operand::Value (Value::Float32 (8_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (2.125_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (5.125_f32))),
			Symbol::ArithmOperator (ArithmOperator::Mul),
			Symbol::ArithmOperator (ArithmOperator::BinMinus),
			Symbol::Operand (Operand::Value (Value::Float32 (4.125_f32))),
			Symbol::ArithmOperator (ArithmOperator::BinPlus),
			Symbol::ArithmOperator (ArithmOperator::UnMinus),
			Symbol::Operand (Operand::Value (Value::Float32 (3.125_f32))),
			Symbol::ArithmOperator (ArithmOperator::UnMinus),
			Symbol::ArithmOperator (ArithmOperator::Div),
		],
		-(8_f32 - 2.125_f32 * 5.125_f32 + 4.125_f32) / -3.125_f32);
		
		test_expr_and_its_stack_eq("33 + (1 + 2 * (3 + 4) + 5) / 10 - 30;", vec![
			Symbol::Operand (Operand::Value (Value::Float32 (33_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (1_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (2_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (3_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (4_f32))),
			Symbol::ArithmOperator (ArithmOperator::BinPlus),
			Symbol::ArithmOperator (ArithmOperator::Mul),
			Symbol::ArithmOperator (ArithmOperator::BinPlus),
			Symbol::Operand (Operand::Value (Value::Float32 (5_f32))),
			Symbol::ArithmOperator (ArithmOperator::BinPlus),
			Symbol::Operand (Operand::Value (Value::Float32 (10_f32))),
			Symbol::ArithmOperator (ArithmOperator::Div),
			Symbol::ArithmOperator (ArithmOperator::BinPlus),
			Symbol::Operand (Operand::Value (Value::Float32 (30_f32))),
			Symbol::ArithmOperator (ArithmOperator::BinMinus),
		],
		33_f32 + (1_f32 + 2_f32 * (3_f32 + 4_f32) + 5_f32) / 10_f32 - 30_f32);
		
		test_expr_and_its_stack_eq("2^2;", vec![
			Symbol::Operand (Operand::Value (Value::Float32 (2_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (2_f32))),
			Symbol::ArithmOperator (ArithmOperator::Pow),
		],
		2_f32.powf(2_f32));
		
		test_expr_and_its_stack_eq("-2^2+4;", vec![
			Symbol::Operand (Operand::Value (Value::Float32 (2_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (2_f32))),
			Symbol::ArithmOperator (ArithmOperator::Pow),
			Symbol::ArithmOperator (ArithmOperator::UnMinus),
			Symbol::Operand (Operand::Value (Value::Float32 (4_f32))),
			Symbol::ArithmOperator (ArithmOperator::BinPlus),
		],
		-2_f32.powf(2_f32) + 4_f32);
		
		test_expr_and_its_stack_eq("3^1^2;", vec![
			Symbol::Operand (Operand::Value (Value::Float32 (3_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (1_f32))),
			Symbol::Operand (Operand::Value (Value::Float32 (2_f32))),
			Symbol::ArithmOperator (ArithmOperator::Pow),
			Symbol::ArithmOperator (ArithmOperator::Pow),
		],
		3_f32.powf(1_f32.powf(2_f32)));
	}
	
	fn test_expr_and_its_stack_eq(
		expr_str: &str, 
		correct_expr_stack: Vec<Symbol>,
		result: f32
	) {
		let mut tokens_iter = TokensIter::new();	
		tokens_iter.push_string(expr_str.to_string());
		assert!(tokens_iter.cache_until_semicolon().unwrap());
		
		let expr_stack: Vec<(Token, Symbol)> = Expr::create_stack(
			&mut tokens_iter, 
			ExprContext::new(ExprContextKind::ValueToAssign)).unwrap();
		
		let syms_expr_stack: Vec<Symbol> = expr_stack.iter().map(|(_tok, sym)| sym.clone()).collect();
		
		if syms_expr_stack == correct_expr_stack { 
			let mut tokens_iter = TokensIter::new();	
			tokens_iter.push_string(expr_str.to_string());
			assert!(tokens_iter.cache_until_semicolon().unwrap());
		
			let expr = Expr::new(&mut tokens_iter, ExprContextKind::ValueToAssign).unwrap();
			
			let memory = Memory::new();
			
			match expr.calc(&memory).unwrap() {
				Value::Float32 (res) => if (result - res).abs() > std::f32::EPSILON * 3.0 {
					panic!("Wrong result '{}' != {:?} instead of {}", expr_str, res, result);
				},
				res @ _ => panic!("Wrong result type for code '{}', expected Value::Float32, got {:?}", expr_str, res),
			}
			
			println!("Ok: '{}'", expr_str);
			
			return; 
		}
		
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
				},
			}
			match correct_expr_stack.get(i) {
				Some(sym) => println!("{:?}", sym),
				None => println!("None"),
			}
		}
		
		panic!("Test failed ^^^");
	}
}