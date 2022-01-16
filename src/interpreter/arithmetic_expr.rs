use super::token::{Token, TokenContent, Operator, Bracket, TokensIter, StatementOp};
use super::InterpErr;
use super::memory::Memory;
use super::var_data::{VarValue, DataType};

type TokSym = (Token, Symbol);

#[derive(Debug, PartialEq, Eq)]
pub struct ArithmeticExpr {
	data_type: DataType,
	expr_stack: Vec<TokSym>,
}

impl ArithmeticExpr {	
	pub fn new(tokens_iter: &mut TokensIter, context_kind: ExprContextKind) -> Result<Self, InterpErr> {
		let context = ExprContext::new(context_kind);
		let expr_stack = Self::create_stack(tokens_iter, context)?;
		
		Ok( Self { data_type: DataType::Float32, expr_stack } )
	}
	
	pub fn get_data_type(&self) -> DataType {
		self.data_type
	}
	
	pub fn calc(&self, memory: &Memory) -> Result<VarValue, InterpErr> {
		let mut calc_stack = Vec::<VarValue>::with_capacity(self.expr_stack.len());
		let mut expr = self.expr_stack.clone();
		expr.reverse();
		
		while let Some((tok, sym)) = expr.pop() {
			match sym {
				Symbol::Operand (opnd) => {
					let value: VarValue = match opnd {
						Operand::Number (val) => VarValue::Float32(val),
						Operand::Name (name) => memory.get_variable(&name)?.clone(),
						Operand::BuiltinName (name) => todo!(),
					};
					calc_stack.push(value);
				},
				
				Symbol::LeftBracket => unreachable!(),
				
				Symbol::ArithmOperator (op) => {
					let value: VarValue = match op.apply(&mut calc_stack) {
						Err( err ) => return Err( InterpErr::from(ArithmExprErr::Operator { err, tok }) ),
						Ok(val) => val,
					};
					calc_stack.push(value);
				},
			}
		}
		
		let result: VarValue = calc_stack.pop().unwrap();
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
					let found_token = tokens_iter.next().unwrap()?;
					return Err( InterpErr::from(ArithmExprErr::ExpectedExprButFound(found_token)) );
				}
				break;
			}
			
			let token = tokens_iter.next().unwrap()?;
			
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
				TokenContent::BuiltinName (name) => {
					if prev_is_operand {
						return Err( InterpErr::ArithmExpr( ArithmExprErr::UnexpectedToken (token) ) );
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
							=> return Err( InterpErr::ArithmExpr( ArithmExprErr::UnexpectedToken (token) ) ),
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
					return Err( InterpErr::ArithmExpr( ArithmExprErr::UnexpectedToken (token) ) );
				},
				
				TokenContent::StatementOp (..) => unreachable!(),
			}
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
		tok: Token, op: Operator
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
								
							Symbol::LeftBracket => break,
							
							Symbol::ArithmOperator ( top_op_ref ) => 
								if top_op_ref.rank() >= ar_op.rank() {
									tmp_stack.pop().unwrap()
								} else {
									break;
								},
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
						None => return Err( InterpErr::ArithmExpr( ArithmExprErr::UnpairedBracket (tok) ) ),
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
							=> Err( InterpErr::ArithmExpr (ArithmExprErr::UnexpectedToken (tok.clone()) ) ),
						StatementOp::Semicolon => Ok(true),
					},
					TokenContent::Keyword (..) 
						=> Err( InterpErr::ArithmExpr (ArithmExprErr::UnexpectedToken (tok.clone()) ) ),
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
							=> Err( InterpErr::ArithmExpr (ArithmExprErr::UnexpectedToken (tok.clone()) ) ),
						StatementOp::Semicolon | StatementOp::Comma 
							=> Ok(true),
					},
					TokenContent::Keyword (..) 
						=> Err( InterpErr::ArithmExpr (ArithmExprErr::UnexpectedToken (tok.clone()) ) ),
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

//------------------------------- Symbol''' ----------------------------------
/*
struct Expr {
	syms: Vec<&'static dyn Optr>,	
}

impl Expr {
	fn push<T: Optr + 'static>(&mut self, optr: &'static T) {
		self.syms.push(optr as &'static dyn Optr);
	}
}

trait Optr: std::fmt::Debug {
	fn rank(&self) -> u32;
	fn apply(&self, calc_stack: &mut Vec<VarValue>);
	fn assoc(&self) -> Assoc;
	fn arity(&self) -> Arity;
}

#[derive(Debug)]
struct Plus {
	rank: u32,
	assoc: Assoc,
	arity: Arity,
	tc: Operator,
}

impl Optr for Plus {
	fn rank(&self) -> u32 { self.rank }
	fn apply(&self, calc_stack: &mut Vec<VarValue>) {  }
	fn assoc(&self) -> Assoc { self.assoc }
	fn arity(&self) -> Arity { self.arity }
}

static operators: [Op; 7] = [
	Op { rank: 0, assoc: Assoc::L, arity: Arity::B, tc: Operator::Plus },
	Op { rank: 0, assoc: Assoc::L, arity: Arity::B, tc: Operator::Minus },
	Op { rank: 1, assoc: Assoc::L, arity: Arity::B, tc: Operator::Mul },
	Op { rank: 1, assoc: Assoc::L, arity: Arity::B, tc: Operator::Div },
	Op { rank: 2, assoc: Assoc::L, arity: Arity::U, tc: Operator::Plus },
	Op { rank: 2, assoc: Assoc::L, arity: Arity::U, tc: Operator::Minus },
	Op { rank: 3, assoc: Assoc::L, arity: Arity::B, tc: Operator::Pow },
];

struct Op {
	rank: u32,
	assoc: Assoc,
	arity: Arity,
	tc: Operator,
}

#[derive(Debug, Clone, Copy)]
enum Assoc {
	L,
	R,
}

#[derive(Debug, Clone, Copy)]
enum Arity {
	U,
	B,
}
*/

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
	Number (f32),
	Name (String),
	BuiltinName (String),
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
			Operand::BuiltinName (op1) => match other {
				Operand::BuiltinName (op2) => op1 == op2,
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
	fn new_bin(op: Operator) -> Self {
		let ar_op: BinOp = match op {
			Operator::Plus => BinOp::Plus,
			Operator::Minus => BinOp::Minus,
			Operator::Mul => BinOp::Mul,
			Operator::Div => BinOp::Div,
			Operator::Pow => BinOp::Pow,
			Operator::Equals | Operator::Assign => unreachable!(),
		};
		ArithmOperator::Binary (ar_op)
	}
	
	fn new_un_pref(op: Operator) -> Self {
		let ar_op: UnPrefOp = match op {
			Operator::Plus => UnPrefOp::Plus,
			Operator::Minus => UnPrefOp::Minus,
			Operator::Mul 
				| Operator::Div 
				| Operator::Equals 
				| Operator::Assign 
				| Operator::Pow 
				=> unreachable!(),
		};
		ArithmOperator::UnaryPrefix (ar_op)
	}
	
	fn rank(&self) -> u32 {
		match self {
			ArithmOperator::Binary (bin_op) => match bin_op {
				BinOp::Plus | BinOp::Minus => 0_u32,
				BinOp::Div | BinOp::Mul => 1_u32,
				BinOp::Pow => 3_u32,
			},
			ArithmOperator::UnaryPrefix (un_op) => match un_op {
				UnPrefOp::Plus | UnPrefOp::Minus => 2_u32,
			},
		}
	}

	#[allow(unreachable_patterns)]
	fn apply(&self, calc_stack: &mut Vec<VarValue>) -> Result<VarValue, OperatorErr> {		
		let create_err = |values: &[&VarValue]| -> OperatorErr {
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
		
		match self {
			ArithmOperator::Binary (bin_op) => {
				let rhs: VarValue = calc_stack
					.pop()
					.ok_or(OperatorErr::NotEnoughOperands { 
						provided_cnt: 0,
						required_cnt: 2, 
					} )?;
				let lhs: VarValue = calc_stack
					.pop()
					.ok_or(OperatorErr::NotEnoughOperands { 
						provided_cnt: 1,
						required_cnt: 2, 
					} )?;
					
				let (val1, val2): (f32, f32) = match (lhs, rhs) { 
					(VarValue::Float32 (val1), VarValue::Float32 (val2)) => (val1, val2),
					ops @ _ => return Err( create_err(&[&ops.0, &ops.1]) ),
				};
			
				match bin_op {
					BinOp::Plus => Ok( VarValue::Float32(val1 + val2) ),
					BinOp::Minus => Ok( VarValue::Float32(val1 - val2) ),
					BinOp::Div => Ok( VarValue::Float32(val1 / val2) ),
					BinOp::Mul => Ok( VarValue::Float32(val1 * val2) ),
					BinOp::Pow => Ok( VarValue::Float32(val1.powf(val2)) ),
				}
			},
			ArithmOperator::UnaryPrefix (un_pref_op) => {
				let op: VarValue = calc_stack
					.pop()
					.ok_or(OperatorErr::NotEnoughOperands { 
						provided_cnt: 0,
						required_cnt: 1, 
					} )?;

				let val: f32 = match op { 
					VarValue::Float32 (val) => val,
					_ => return Err( create_err(&[&op]) ),
				};

				match un_pref_op {
					UnPrefOp::Plus => Ok( VarValue::Float32(val) ),
					UnPrefOp::Minus => Ok( VarValue::Float32(-val) ),
				}
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
	Pow,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UnPrefOp {
	Plus,
	Minus,
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

//------------------------------- ArithmExprErr ----------------------------------

#[derive(Debug, PartialEq, Eq)]
pub enum ArithmExprErr {
	UnexpectedToken (Token),
	UnpairedBracket (Token),
	ExpectedExprButFound (Token),
	Operator { 
		err: OperatorErr, 
		tok: Token 
	},
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
			ArithmExprErr::ExpectedExprButFound (token) => {
				super::display_error_pos(f, token.pos_begin, token.pos_end)?;
				write!(f, "Expected arithmetical expression, but found {}", token)
			},
			ArithmExprErr::Operator { err, tok } => match err {
				OperatorErr::WrongType { descr } => {
					super::display_error_pos(f, tok.pos_begin, tok.pos_end)?;
					write!(f, "{}", descr)
				},
				OperatorErr::NotEnoughOperands { provided_cnt, required_cnt } => {
					super::display_error_pos(f, tok.pos_begin, tok.pos_end)?;
					write!(f, "Expected {} operand(-s) for operator for operator {:?}, but found {}", provided_cnt, tok, required_cnt)
				}
			},
		}
	}
}

//------------------------------- Tests ----------------------------------

#[cfg(test)]
mod tests {
	use super::super::statement::*;
	use super::super::token::*;
	use super::super::string_char::CharsIter;
	use super::*;
	
	#[test]
	fn check_stack_creation_and_calc() {
		test_expr_and_its_stack_eq("3.125;", vec![
			Symbol::Operand (Operand::Number (3.125_f32)),
		],
		3.125_f32);
		
		test_expr_and_its_stack_eq("3.125 + 5.4;", vec![
			Symbol::Operand (Operand::Number (3.125_f32)),
			Symbol::Operand (Operand::Number (5.4_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Plus)),
		],
		3.125_f32 + 5.4_f32);
		
		test_expr_and_its_stack_eq("3.125 + 5.4 * 2.46;", vec![
			Symbol::Operand (Operand::Number (3.125_f32)),
			Symbol::Operand (Operand::Number (5.4_f32)),
			Symbol::Operand (Operand::Number (2.46_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Mul)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Plus)),
		],
		3.125_f32 + 5.4_f32 * 2.46_f32);
		
		test_expr_and_its_stack_eq("3.125 + 0 + 5.25 * 2.25 - 3.25 / 2 * 4.25;", vec![
			Symbol::Operand (Operand::Number (3.125_f32)),
			Symbol::Operand (Operand::Number (0_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Plus)),
			Symbol::Operand (Operand::Number (5.25_f32)),
			Symbol::Operand (Operand::Number (2.25_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Mul)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Plus)),
			Symbol::Operand (Operand::Number (3.25_f32)),
			Symbol::Operand (Operand::Number (2_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Div)),
			Symbol::Operand (Operand::Number (4.25_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Mul)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Minus)),
		],
		3.125_f32 + 0.0_f32 + 5.25_f32 * 2.25_f32 - 3.25_f32 / 2.0_f32 * 4.25_f32);
		
		test_expr_and_its_stack_eq("3.125 + -5.25 * 2.25;", vec![
			Symbol::Operand (Operand::Number (3.125_f32)),
			Symbol::Operand (Operand::Number (5.25_f32)),
			Symbol::ArithmOperator (ArithmOperator::UnaryPrefix (UnPrefOp::Minus)),
			Symbol::Operand (Operand::Number (2.25_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Mul)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Plus)),
		],
		3.125_f32 + -5.25_f32 * 2.25_f32);
		
		test_expr_and_its_stack_eq("2.5 * ---5.5;", vec![
			Symbol::Operand (Operand::Number (2.5_f32)),
			Symbol::Operand (Operand::Number (5.5_f32)),
			Symbol::ArithmOperator (ArithmOperator::UnaryPrefix (UnPrefOp::Minus)),
			Symbol::ArithmOperator (ArithmOperator::UnaryPrefix (UnPrefOp::Minus)),
			Symbol::ArithmOperator (ArithmOperator::UnaryPrefix (UnPrefOp::Minus)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Mul)),
		],
		2.5_f32 * ---5.5_f32);
		
		test_expr_and_its_stack_eq("1.125 * (3.125 + 2.125);", vec![
			Symbol::Operand (Operand::Number (1.125_f32)),
			Symbol::Operand (Operand::Number (3.125_f32)),
			Symbol::Operand (Operand::Number (2.125_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Plus)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Mul)),
		],
		1.125_f32 * (3.125_f32 + 2.125_f32));
		
		test_expr_and_its_stack_eq("-(8 - 2.125 * 5.125 + 4.125) / -3.125;", vec![
			Symbol::Operand (Operand::Number (8_f32)),
			Symbol::Operand (Operand::Number (2.125_f32)),
			Symbol::Operand (Operand::Number (5.125_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Mul)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Minus)),
			Symbol::Operand (Operand::Number (4.125_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Plus)),
			Symbol::ArithmOperator (ArithmOperator::UnaryPrefix (UnPrefOp::Minus)),
			Symbol::Operand (Operand::Number (3.125_f32)),
			Symbol::ArithmOperator (ArithmOperator::UnaryPrefix (UnPrefOp::Minus)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Div)),
		],
		-(8_f32 - 2.125_f32 * 5.125_f32 + 4.125_f32) / -3.125_f32);
		
		test_expr_and_its_stack_eq("33 + (1 + 2 * (3 + 4) + 5) / 10 - 30;", vec![
			Symbol::Operand (Operand::Number (33_f32)),
			Symbol::Operand (Operand::Number (1_f32)),
			Symbol::Operand (Operand::Number (2_f32)),
			Symbol::Operand (Operand::Number (3_f32)),
			Symbol::Operand (Operand::Number (4_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Plus)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Mul)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Plus)),
			Symbol::Operand (Operand::Number (5_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Plus)),
			Symbol::Operand (Operand::Number (10_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Div)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Plus)),
			Symbol::Operand (Operand::Number (30_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Minus)),
		],
		33_f32 + (1_f32 + 2_f32 * (3_f32 + 4_f32) + 5_f32) / 10_f32 - 30_f32);
		
		test_expr_and_its_stack_eq("2^2;", vec![
			Symbol::Operand (Operand::Number (2_f32)),
			Symbol::Operand (Operand::Number (2_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Pow)),
		],
		2_f32.powf(2_f32));
		
		test_expr_and_its_stack_eq("-2^2+4;", vec![
			Symbol::Operand (Operand::Number (2_f32)),
			Symbol::Operand (Operand::Number (2_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Pow)),
			Symbol::ArithmOperator (ArithmOperator::UnaryPrefix (UnPrefOp::Minus)),
			Symbol::Operand (Operand::Number (4_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Plus)),
		],
		-2_f32.powf(2_f32) + 4_f32);
		
		test_expr_and_its_stack_eq("3^1^2;", vec![
			Symbol::Operand (Operand::Number (3_f32)),
			Symbol::Operand (Operand::Number (1_f32)),
			Symbol::Operand (Operand::Number (2_f32)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Pow)),
			Symbol::ArithmOperator (ArithmOperator::Binary (BinOp::Pow)),
		],
		3_f32.powf(1_f32.powf(2_f32)));
	}
	
	fn test_expr_and_its_stack_eq(
		expr_str: &str, 
		correct_expr_stack: Vec<Symbol>,
		result: f32
	) {
		let mut tokens_iter = TokensIter::new(CharsIter::new(expr_str));	
		let expr_stack: Vec<(Token, Symbol)> = ArithmeticExpr::create_stack(
			&mut tokens_iter, 
			ExprContext::new(ExprContextKind::ValueToAssign)).unwrap();
		
		let syms_expr_stack: Vec<Symbol> = expr_stack.iter().map(|(_tok, sym)| sym.clone()).collect();
		
		if syms_expr_stack == correct_expr_stack { 
			let mut tokens_iter = TokensIter::new(CharsIter::new(expr_str));	
			let expr = ArithmeticExpr::new(&mut tokens_iter, ExprContextKind::ValueToAssign).unwrap();
			
			let memory = Memory::new();
			
			match expr.calc(&memory).unwrap() {
				VarValue::Float32 (res) => if (result - res).abs() > std::f32::EPSILON * 3.0 {
					panic!("Wrong result '{}' != {:?} instead of {}", expr_str, res, result);
				},
				res @ _ => panic!("Wrong result type for code '{}', expected VarValue::Float32, got {:?}", expr_str, res),
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