use super::token::{Token, TokenContent, TokensIter, Operator, Bracket, StatementOp, Keyword, TokenErr};
use super::InterpErr;
use super::memory::Memory;
use super::variable::{Value, DataType};
use super::function::{BuiltinFuncsDefList, UserFuncArg, UserFuncDef};
use super::utils::{CharPos, CodePos, NameToken};
use super::statement::FuncKind;

// TODO: make Expr not clonable
#[derive(Debug, Clone)]
pub struct Expr {
	expr_stack: Vec<Symbol>,
	pos: CodePos,
}

impl Expr {	
	pub fn new(tokens_iter: &mut TokensIter, context_kind: ExprContextKind) -> Result<Self, InterpErr> {
		let context = ExprContext::new(context_kind);
		let expr_stack = Self::create_stack(tokens_iter, context)?;
		
		let mut pos_begin: CharPos = expr_stack[0].pos().begin();
		let mut pos_end: CharPos = expr_stack[0].pos().end();
		
		for sym_ref in &expr_stack {
			pos_begin = std::cmp::min(sym_ref.pos().begin(), pos_begin);
			pos_end = std::cmp::max(sym_ref.pos().end(), pos_end);
		}
		
		Ok( Self { 
			expr_stack,
			pos: CodePos::new(pos_begin, pos_end),
		} )
	}
	
	pub fn calc(&self, memory: &mut Memory, builtin_func_defs: &BuiltinFuncsDefList) -> Value {
		let mut calc_stack = Vec::<Value>::with_capacity(self.expr_stack.len()); // TODO: try to keep symbols instead of values to be able to call member functions
		
		for Symbol { kind, pos } in self.expr_stack.iter() {
			let pos = *pos;
			match kind {
				SymbolKind::Operand (opnd) => {
					let value: Value = match opnd {
						Operand::Value (val) => val.clone(), // TODO: try do it without cloning values
						Operand::Variable (name) => memory.get_variable_value(&NameToken::new_with_pos(&name, pos)).unwrap().clone(),
						Operand::FuncCall { kind, func_name, arg_exprs } => {
							match kind {
								FuncKind::Builtin => {
									let f = builtin_func_defs.find(&func_name).unwrap();
							
									let mut args_values = Vec::<Value>::with_capacity(arg_exprs.len());
									
									for expr in arg_exprs {
										let value: Value = expr.calc(memory, builtin_func_defs);
										args_values.push(value);
									}
									
									f.call(args_values).unwrap()
								},
								FuncKind::UserDefined => {
									let f: UserFuncDef = (*memory.find_func_def(&func_name).unwrap()).clone(); // TODO: Do not clone UserFuncDef
							
									let mut args_values = Vec::<Value>::with_capacity(arg_exprs.len());
									
									for expr in arg_exprs {
										let value: Value = expr.calc(memory, builtin_func_defs);
										args_values.push(value);
									}
									
									memory.push_frame();
									
									let func_args: &Vec<UserFuncArg> = f.args();
									for i in 0..args_values.len() {
										memory.add_variable(
											func_args[i].name().clone(),
											func_args[i].data_type(),
											Some(args_values[i].clone())).unwrap();
									}
									
									let value: Value = f.call(memory, builtin_func_defs).unwrap();
									
									memory.pop_frame();
									
									value
								},
							}
						},
					};
					calc_stack.push(value);
				},
				
				SymbolKind::LeftBracket => unreachable!(),
				
				SymbolKind::ExprOperator (op) => {
					let value: Value = op.apply(&mut calc_stack).unwrap();
					calc_stack.push(value);
				},
			}
		}
		
		let result: Value = calc_stack.pop().unwrap();
		assert_eq!(calc_stack.pop(), None);
		
		result
	}
	
	pub fn check_and_calc_data_type(&self, check_memory: &Memory, builtin_func_defs: &BuiltinFuncsDefList) -> Result<DataType, InterpErr> {		
		assert!(self.expr_stack.len() > 0);
		let mut type_calc_stack = Vec::<DataType>::with_capacity(self.expr_stack.len());
		
		for Symbol { kind, pos } in self.expr_stack.iter() {
			let pos = *pos;
			match kind {
				SymbolKind::Operand (ref opnd) => {
					let opnd_dt: DataType = match opnd {
						Operand::Value (val) => val.get_type(),
						Operand::Variable (name) =>
							check_memory.get_variable_value(&NameToken::new_with_pos(&name, pos))?.get_type(),
						Operand::FuncCall { kind, func_name, arg_exprs } => {
							match kind {
								FuncKind::Builtin => {
									let f = builtin_func_defs.find(&func_name).unwrap();
							
									f.check_args(&func_name, arg_exprs, check_memory, builtin_func_defs)?;
									f.return_type()
								},
								FuncKind::UserDefined => {
									let f = check_memory.find_func_def(&func_name).unwrap();
							
									f.check_args(arg_exprs, check_memory, builtin_func_defs)?;
									f.return_type()
								},
							}
						},
					};
					type_calc_stack.push(opnd_dt);
				},
				
				SymbolKind::LeftBracket => unreachable!(),
				
				SymbolKind::ExprOperator (op) => {
					match op.get_result_data_type(&mut type_calc_stack) {
						Err(err) => return Err( InterpErr::from(ExprErr::Operator { err, pos }) ),
						Ok(dt) => type_calc_stack.push(dt),
					}
				}, 
			}
		}
		
		let result: DataType = type_calc_stack.pop().unwrap();
		assert_eq!(type_calc_stack.pop(), None);
		
		Ok(result)
	}

	pub fn pos(&self) -> CodePos {
		self.pos
	}
	
	fn create_stack(tokens_iter: &mut TokensIter, mut context: ExprContext) -> Result<Vec<Symbol>, InterpErr> {
		let mut tmp_stack = Vec::<Symbol>::new();
		let mut expr_stack = Vec::<Symbol>::new();
		let mut prev_is_operand = false;
		
		loop {
			let next_token_ref = tokens_iter.peek_or_end_reached_err()?;
			
			if context.check_expr_end(next_token_ref)? {		
				if expr_stack.len() == 0 {
					let found_token = tokens_iter.next_or_end_reached_err()?;
					return Err( InterpErr::from(ExprErr::ExpectedExprButFound(found_token.pos())) );
				}
				break;
			}
			
			let token = tokens_iter.next().unwrap()?;
			
			let Token { pos, content } = token;
			
			match content {	
				TokenContent::Number (num) => {
					if prev_is_operand {
						return Err(unexpected(pos));
					}
					
					let sym = Symbol::new_number(num, pos);
					expr_stack.push(sym);
					
					prev_is_operand = true;
				},
				
				TokenContent::BuiltinName (name) => {
					if prev_is_operand {
						return Err(unexpected(pos));
					}
					
					let sym = Self::parse_func_call_or_name(FuncKind::Builtin, &mut prev_is_operand, tokens_iter, name, pos)?;
					expr_stack.push(sym);
				},
				
				TokenContent::Name (name) => {
					if prev_is_operand {
						return Err(unexpected(pos));
					}
					
					let sym = Self::parse_func_call_or_name(FuncKind::UserDefined, &mut prev_is_operand, tokens_iter, name, pos)?;
					expr_stack.push(sym);
				},
				
				TokenContent::StringLiteral (s) => {
					if prev_is_operand {
						return Err(unexpected(pos));
					}
					
					let sym = Symbol::new_string_literal(s, pos);
					expr_stack.push(sym);
					
					prev_is_operand = true;					
				},
				
				TokenContent::Operator (ref tok_op) => {
					match tok_op {
						Operator::Plus | Operator::Minus => {
							if prev_is_operand {
								Self::add_bin_op(&mut expr_stack, &mut tmp_stack, pos, content)?;
							} else {
								tmp_stack.push(Symbol::new_un_pref_op(content, pos));
							}
						},
						Operator::Mul 
							| Operator::Div 
							| Operator::Pow 
							| Operator::Greater 
							| Operator::GreaterEqual
							| Operator::Less
							| Operator::LessEqual
							| Operator::Not
							| Operator::Equal
							| Operator::NotEqual
							| Operator::LogicalAnd
							| Operator::LogicalOr
							| Operator::LogicalXor
							=> {
								Self::add_bin_op(&mut expr_stack, &mut tmp_stack, pos, content)?;
						},
						Operator::Assign => return Err(unexpected(pos)),
					};
					
					prev_is_operand = false;
				},
				
				TokenContent::Bracket (tok_br) => {					
					prev_is_operand = match tok_br {
						Bracket::Right => true,
						Bracket::Left => false,
						Bracket::LeftCurly | Bracket::RightCurly => return Err(unexpected(pos)),
					};
					
					Self::add_bracket(&mut expr_stack, &mut tmp_stack, pos, tok_br)?;
				},
				
				TokenContent::Keyword (kw) => match kw {
					Keyword::Var | 
						Keyword::If | 
						Keyword::Else | 
						Keyword::While | 
						Keyword::F | 
						Keyword::Return => return Err(unexpected(pos)),
					Keyword::True | Keyword::False => {
						if prev_is_operand {
							return Err(unexpected(pos));
						}
						
						let sym = Symbol::new_bool_literal(kw, pos);
						expr_stack.push(sym);
						prev_is_operand = true;
					},
				},
				
				TokenContent::StatementOp (ref st_op) => match st_op {
					StatementOp::Dot => {
						Self::add_bin_op(&mut expr_stack, &mut tmp_stack, pos, content)?;
						prev_is_operand = false;
					},
					_ => return Err(unexpected(pos)),
				},
			}
		}
		
		while let Some(top_sym) = tmp_stack.pop() {
			match top_sym.kind {
				SymbolKind::Operand (..) => expr_stack.push(top_sym),
				SymbolKind::LeftBracket => return Err( unpaired_bracket(top_sym.pos) ),
				SymbolKind::ExprOperator (..) => expr_stack.push(top_sym),
			}
		}
				
		Ok(expr_stack)
	}
	
	fn add_bin_op(
		expr_stack: &mut Vec<Symbol>, 
		tmp_stack: &mut Vec<Symbol>, 
		pos: CodePos, tc: TokenContent
		) -> Result<(), InterpErr> 
	{
		let next_kind = ExprOperator::new_bin(tc);
		
		use std::cmp::Ordering;
		
		loop {
			let top_tok_sym: Symbol = 
				match tmp_stack.last() {
					Some(top_ref) => 
						match &top_ref.kind {
							SymbolKind::Operand (..) => return Err(unexpected(tmp_stack.pop().unwrap().pos())),
								
							SymbolKind::LeftBracket => break,
							
							SymbolKind::ExprOperator ( top_ref ) => {
								match next_kind.rank().cmp(&top_ref.rank()) {
									Ordering::Less => tmp_stack.pop().unwrap(),
									Ordering::Equal => match (top_ref.assot(), next_kind.assot()) {
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
			
		tmp_stack.push(Symbol { kind: SymbolKind::ExprOperator (next_kind), pos });
		
		Ok(())
	}
	
	fn add_bracket(
		expr_stack: &mut Vec<Symbol>, 
		tmp_stack: &mut Vec<Symbol>, 
		pos: CodePos, br: Bracket
	) -> Result<(), InterpErr> 
	{
		match br {
			Bracket::Left => {
				tmp_stack.push(Symbol::new_left_bracket(pos));
			},
			Bracket::Right => {
				'out: loop {
					match tmp_stack.pop() {
						Some( sym ) => match sym.kind {
								SymbolKind::LeftBracket => break 'out,
								SymbolKind::Operand (..) | SymbolKind::ExprOperator (..) => expr_stack.push(sym),
							},
						None => return Err( InterpErr::from( ExprErr::UnpairedBracket (pos) ) ),
					};
				};
			},
			Bracket::LeftCurly => return Err(unexpected(pos)),
			Bracket::RightCurly => return Err(unexpected(pos)),
		};
		
		Ok(())
	}

	fn parse_func_call_or_name(kind: FuncKind, prev_is_operand: &mut bool, tokens_iter: &mut TokensIter, name: String, pos: CodePos) -> Result<Symbol, InterpErr> {
		if *prev_is_operand {
			return Err(unexpected(pos));
		}
		*prev_is_operand = true;
		
		match tokens_iter.peek_or_end_reached_err()?.content() {
			TokenContent::Bracket (Bracket::Left) => {
				tokens_iter.next_or_end_reached_err().unwrap();
				
				let mut arg_exprs = Vec::<Expr>::new();
				
				if let TokenContent::Bracket (Bracket::Right) = tokens_iter.peek_or_end_reached_err()?.content() {
					tokens_iter.next_or_end_reached_err().unwrap();
				} else {
					loop {			
						arg_exprs.push(Expr::new(
							tokens_iter,
							ExprContextKind::FunctionArg)?);
							
						match tokens_iter.next_or_end_reached_err()? {
							Token { content: TokenContent::StatementOp ( StatementOp::Comma ), .. } => {},
							
							Token { content: TokenContent::Bracket ( Bracket::Right ), .. } => break,
							
							found @ _ => 
								return Err( InterpErr::from( TokenErr::ExpectedButFound { 
									expected: vec![
										TokenContent::StatementOp(StatementOp::Comma),
										TokenContent::Bracket ( Bracket::Right ),
									], 
									found
								} ) ),
						}
					}
				}
				
				let func_name = NameToken::new_with_pos(&name, pos);
				
				let sym = Symbol::new_func_call(kind, func_name, arg_exprs);
				Ok(sym)
			},
			_ => {
				let sym = Symbol::new_name(name, pos);
				Ok(sym)
			},
		}
	}
}

impl Eq for Expr {}
impl PartialEq for Expr {
	fn eq(&self, other: &Self) -> bool {
		self.expr_stack == other.expr_stack
	}
}

//------------------------------- ExprContext ----------------------------------

#[derive(Debug, Clone, Copy)]
pub enum ExprContextKind {
	ValueToAssign,
	ToReturn,
	FunctionArg,
	IfCondition,
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
		match tok.content() {
			TokenContent::Number (..) | 
				TokenContent::Operator (..) | 
				TokenContent::Name (..) |
				TokenContent::BuiltinName (..) |
				TokenContent::StringLiteral (..)
				=> Ok(false),
			TokenContent::Bracket (_) 
				=> self.check_brackets(tok),
			TokenContent::StatementOp (st_op) => match st_op {
				StatementOp::Dot => Ok(false),
				StatementOp::Colon | StatementOp::Comment (_) | StatementOp::ThinArrow => Err(unexpected(tok.pos())),
				StatementOp::Comma => match self.kind {
					ExprContextKind::ValueToAssign | ExprContextKind::IfCondition | ExprContextKind::ToReturn => Err(unexpected(tok.pos())),
					ExprContextKind::FunctionArg => Ok(true),
				},
				StatementOp::Semicolon => Ok(true),
			},
			TokenContent::Keyword (kw) => match kw {
				Keyword::Var | 
					Keyword::If | 
					Keyword::Else | 
					Keyword::While | 
					Keyword::F | 
					Keyword::Return => Err(unexpected(tok.pos())),
				Keyword::True | Keyword::False => Ok(false),
			},
		}
	}
	
	fn check_brackets(&mut self, br_tok: &Token) -> Result<bool, InterpErr> {
		match br_tok.content() {
			TokenContent::Bracket(br) => match br {
				Bracket::Left => {
					self.left_brackets_count += 1;
					Ok(false)
				},
				Bracket::Right  => {
					if self.left_brackets_count > 0 {
						self.left_brackets_count -= 1;
						Ok(false)
					} else {
						match self.kind {
							ExprContextKind::ValueToAssign | ExprContextKind::IfCondition | ExprContextKind::ToReturn => Err( unpaired_bracket(br_tok.pos()) ),
							ExprContextKind::FunctionArg => Ok(true),
						}
					}
				},
				Bracket::LeftCurly => match self.kind {
					ExprContextKind::IfCondition => Ok(true),
					ExprContextKind::ValueToAssign | ExprContextKind::FunctionArg | ExprContextKind::ToReturn => Err( unpaired_bracket(br_tok.pos()) ),
				},
				_ => Err( unexpected(br_tok.pos()) ),
			},
			_ => unreachable!(),
		}
	}
}

//------------------------------- Symbol ----------------------------------

#[derive(Debug, Clone)]
struct Symbol {
	kind: SymbolKind,
	pos: CodePos,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum SymbolKind {
	Operand (Operand),
	LeftBracket,
	ExprOperator (ExprOperator),
}

impl Symbol {
	fn new_number(num: f32, pos: CodePos) -> Self {
		Self {
			kind: SymbolKind::Operand( Operand::Value (Value::from(num)) ),
			pos,
		}
	}
	fn new_name(name: String, pos: CodePos) -> Self {
		Self {
			kind: SymbolKind::Operand( Operand::Variable (name) ),
			pos,
		}
	}
	fn new_func_call(kind: FuncKind, func_name: NameToken, arg_exprs: Vec<Expr>) -> Self {
		let pos = func_name.pos();
		Self {
			kind: SymbolKind::Operand( Operand::FuncCall {
				kind,
				func_name, 
				arg_exprs,
			} ),
			pos,
		}
	}
	fn new_string_literal(content: String, pos: CodePos) -> Self {
		Self {
			kind: SymbolKind::Operand( Operand::Value (Value::from(content)) ),
			pos,
		}
	}
	fn new_bool_literal(kw: Keyword, pos: CodePos) -> Self {
		let kind: SymbolKind = match kw {
			Keyword::True => SymbolKind::Operand (Operand::Value (Value::from(true))),
			Keyword::False => SymbolKind::Operand (Operand::Value (Value::from(false))),
			_ => panic!("Unexpected input: {:?}", kw),
		};
		Self { kind, pos }
	}
	fn new_left_bracket(pos: CodePos) -> Self {
		Self { 
			kind: SymbolKind::LeftBracket, 
			pos,
		}
	}
	fn new_un_pref_op(tc: TokenContent, pos: CodePos) -> Self {
		Self { 
			kind: SymbolKind::ExprOperator( ExprOperator::new_un_pref(tc) ), 
			pos,
		}
	}

	fn pos(&self) -> CodePos {
		self.pos
	}
}

impl Eq for Symbol {}
impl PartialEq for Symbol {
	fn eq(&self, other: &Self) -> bool {
		self.kind == other.kind
	}
}

#[derive(Debug, Clone)]
enum Operand {
	Value (Value),
	Variable (String),
	FuncCall {
		kind: FuncKind,
		func_name: NameToken, 
		arg_exprs: Vec<Expr>,
	},
}
impl Eq for Operand {}
impl PartialEq for Operand {
	fn eq(&self, other: &Self) -> bool {
		match self {
			Operand::Value (v1) => match other {
				Operand::Value (v2) => v1 == v2,
				_ => false,
			},
			Operand::Variable (op1) => match other {
				Operand::Variable (op2) => op1 == op2,
				_ => false,
			},
			Operand::FuncCall { kind: kind1, func_name: fn1, arg_exprs: ae1 } => match other {
				Operand::FuncCall { kind: kind2, func_name: fn2, arg_exprs: ae2 } => fn1 == fn2 && ae1 == ae2 && kind1 == kind2,
				_ => false,
			},
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
enum ExprOperator {
	LogicalOr = 0_usize,
	LogicalAnd,
	LogicalXor,
	
	Equal,
	NotEqual,
	
	Less,
	LessEqual,
	Greater,
	GreaterEqual,
	
	BinPlus,
	BinMinus,
	
	Div,
	Mul,
	
	UnPlus,
	UnMinus,
	Not,
	
	Pow,
	
	DotMemberAccess,
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

static OP_ATTRS: [OpInfo; 18] = [
	OpInfo { arity: OpArity::Binary, rank: 0, assot: OpAssot::Left },		//LogicalOr
	OpInfo { arity: OpArity::Binary, rank: 1, assot: OpAssot::Left },		//LogicalAnd
	OpInfo { arity: OpArity::Binary, rank: 2, assot: OpAssot::Left },		//LogicalXor
	
	OpInfo { arity: OpArity::Binary, rank: 3, assot: OpAssot::Left },		//Equal
	OpInfo { arity: OpArity::Binary, rank: 3, assot: OpAssot::Left },		//NotEqual
	
	OpInfo { arity: OpArity::Binary, rank: 4, assot: OpAssot::Left },		//Less
	OpInfo { arity: OpArity::Binary, rank: 4, assot: OpAssot::Left },		//LessEqual
	OpInfo { arity: OpArity::Binary, rank: 4, assot: OpAssot::Left },		//Greater
	OpInfo { arity: OpArity::Binary, rank: 4, assot: OpAssot::Left },		//GreaterEqual
	
	OpInfo { arity: OpArity::Binary, rank: 5, assot: OpAssot::Left },		//BinPlus
	OpInfo { arity: OpArity::Binary, rank: 5, assot: OpAssot::Left }, 	//BinMinus
	
	OpInfo { arity: OpArity::Binary, rank: 6, assot: OpAssot::Left }, 	//Div
	OpInfo { arity: OpArity::Binary, rank: 6, assot: OpAssot::Left }, 	//Mul
	
	OpInfo { arity: OpArity::Unary, rank: 7, assot: OpAssot::Left },		//UnPlus
	OpInfo { arity: OpArity::Unary, rank: 7, assot: OpAssot::Left },		//UnMinus
	OpInfo { arity: OpArity::Unary, rank: 7, assot: OpAssot::Left },		//Not
	
	OpInfo { arity: OpArity::Binary, rank: 8, assot: OpAssot::Right }, 	//Pow
	
	OpInfo { arity: OpArity::Binary, rank: 9, assot: OpAssot::Left }, 	//Dot
];

impl ExprOperator {
	fn new_bin(tc: TokenContent) -> Self {
		match tc {
			TokenContent::Operator (ref tok_op) => match tok_op {
				Operator::Plus => ExprOperator::BinPlus,
				Operator::Minus => ExprOperator::BinMinus,
				Operator::Mul => ExprOperator::Mul,
				Operator::Div => ExprOperator::Div,
				Operator::Pow => ExprOperator::Pow,
				Operator::Equal => ExprOperator::Equal,
				Operator::Greater => ExprOperator::Greater,
				Operator::GreaterEqual => ExprOperator::GreaterEqual,
				Operator::Less => ExprOperator::Less,
				Operator::LessEqual => ExprOperator::LessEqual,
				Operator::NotEqual => ExprOperator::NotEqual,
				Operator::LogicalAnd => ExprOperator::LogicalAnd,
				Operator::LogicalOr => ExprOperator::LogicalOr,
				Operator::LogicalXor => ExprOperator::LogicalXor,
				Operator::Not => ExprOperator::Not,
				Operator::Assign => panic!("Wrong input: {:?}", tc),
			},
			TokenContent::StatementOp (ref st_op) => match st_op {
				StatementOp::Dot => ExprOperator::DotMemberAccess,
				_ => panic!("Wrong input: {:?}", tc),
			},
			_ => panic!("Wrong input: {:?}", tc),
		}
	}
	
	fn new_un_pref(tc: TokenContent) -> Self {
		match tc {
			TokenContent::Operator (tok_op) => match tok_op {
				Operator::Plus => ExprOperator::UnPlus,
				Operator::Minus => ExprOperator::UnMinus,
				Operator::Mul 
					| Operator::Div 
					| Operator::Equal 
					| Operator::Assign 
					| Operator::Pow 
					| Operator::Greater 
					| Operator::GreaterEqual
					| Operator::Less 
					| Operator::LessEqual 
					| Operator::NotEqual
					| Operator::Not
					| Operator::LogicalAnd
					| Operator::LogicalOr
					| Operator::LogicalXor
				=> panic!("Wrong input: {:?}", tc),
			},
			_ => panic!("Wrong input: {:?}", tc),
		}
	}
	
	fn rank(&self) -> u32 {
		OP_ATTRS[*self as usize].rank
	}
	
	fn assot(&self) -> OpAssot {
		OP_ATTRS[*self as usize].assot
	}

	fn apply(&self, calc_stack: &mut Vec<Value>) -> Result<Value, OperatorErr> {	
		use ExprOperator::*;
		match self {
			BinPlus => self.apply_bin_plus(calc_stack),
			BinMinus => self.apply_bin_minus(calc_stack),
			Div => self.apply_bin_div(calc_stack),
			Mul => self.apply_bin_mul(calc_stack),
			Pow => self.apply_bin_pow(calc_stack),
			UnPlus => self.apply_unary_plus(calc_stack),
			UnMinus => self.apply_unary_minus(calc_stack),
			Equal => self.apply_equal(calc_stack),
			NotEqual => self.apply_not_equal(calc_stack),
			Not => self.apply_not(calc_stack),
			Greater => self.apply_greater(calc_stack),
			GreaterEqual => self.apply_greater_equal(calc_stack),
			Less => self.apply_less(calc_stack),
			LessEqual => self.apply_less_equal(calc_stack),
			LogicalAnd => self.apply_logical_and(calc_stack),
			LogicalOr => self.apply_logical_or(calc_stack),
			LogicalXor => self.apply_logical_xor(calc_stack),
			DotMemberAccess => todo!(),
		}
	}
	
	fn get_result_data_type(self, calc_stack: &mut Vec<DataType>) -> Result<DataType, OperatorErr> {
		use ExprOperator::*;
		match self {
			BinPlus => self.get_bin_plus_result_type(calc_stack),
			BinMinus => self.get_bin_minus_result_type(calc_stack),
			Div => self.get_bin_div_result_type(calc_stack),
			Mul => self.get_bin_mul_result_type(calc_stack),
			Pow => self.get_bin_pow_result_type(calc_stack),
			UnPlus => self.get_unary_plus_result_type(calc_stack),
			UnMinus => self.get_unary_minus_result_type(calc_stack),
			Equal => self.get_equal_result_type(calc_stack),
			NotEqual => self.get_not_equal_result_type(calc_stack),
			Not => self.get_not_result_type(calc_stack),
			Greater => self.get_greater_result_type(calc_stack),
			GreaterEqual => self.get_greater_equal_result_type(calc_stack),
			Less => self.get_less_result_type(calc_stack),
			LessEqual => self.get_less_equal_result_type(calc_stack),
			LogicalAnd => self.get_logical_and_result_type(calc_stack),
			LogicalOr => self.get_logical_or_result_type(calc_stack),
			LogicalXor => self.get_logical_xor_result_type(calc_stack),
			DotMemberAccess => todo!(),
		}
	}
	
	fn apply_bin_plus(&self, calc_stack: &mut Vec<Value>) -> Result<Value, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Ok(Value::Float32(val1 + val2)),
			(Value::String (val1), Value::String (val2)) => {
				let mut res: String = val1.clone();
				res.push_str(&val2);
				Ok(Value::String(res))
			},
			ops @ _ => return Err( self.create_value_type_err(&[&ops.0, &ops.1]) ),
		}
	}
	
	fn apply_bin_minus(&self, calc_stack: &mut Vec<Value>) -> Result<Value, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Ok(Value::Float32(val1 - val2)),
			ops @ _ => return Err( self.create_value_type_err(&[&ops.0, &ops.1]) ),
		}
	}
	
	fn apply_bin_mul(&self, calc_stack: &mut Vec<Value>) -> Result<Value, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Ok(Value::Float32(val1 * val2)),
			ops @ _ => return Err( self.create_value_type_err(&[&ops.0, &ops.1]) ),
		}
	}
	
	fn apply_bin_div(&self, calc_stack: &mut Vec<Value>) -> Result<Value, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Ok(Value::Float32(val1 / val2)),
			ops @ _ => return Err( self.create_value_type_err(&[&ops.0, &ops.1]) ),
		}
	}
	
	fn apply_bin_pow(&self, calc_stack: &mut Vec<Value>) -> Result<Value, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Ok(Value::Float32(val1.powf(val2))),
			ops @ _ => return Err( self.create_value_type_err(&[&ops.0, &ops.1]) ),
		}
	}
	
	fn apply_equal(&self, calc_stack: &mut Vec<Value>) -> Result<Value, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Ok(Value::Bool(val1 == val2)),
			(Value::String (val1), Value::String (val2)) => Ok(Value::Bool(val1 == val2)),
			(Value::Bool (val1), Value::Bool (val2)) => Ok(Value::Bool(val1 == val2)),
			ops @ _ => return Err( self.create_value_type_err(&[&ops.0, &ops.1]) ),
		}
	}

	fn apply_not_equal(&self, calc_stack: &mut Vec<Value>) -> Result<Value, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Ok(Value::Bool(val1 != val2)),
			(Value::String (val1), Value::String (val2)) => Ok(Value::Bool(val1 != val2)),
			(Value::Bool (val1), Value::Bool (val2)) => Ok(Value::Bool(val1 != val2)),
			ops @ _ => return Err( self.create_value_type_err(&[&ops.0, &ops.1]) ),
		}
	}

	fn apply_greater(&self, calc_stack: &mut Vec<Value>) -> Result<Value, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Ok(Value::Bool(val1 > val2)),
			(Value::String (val1), Value::String (val2)) => Ok(Value::Bool(val1 > val2)),
			ops @ _ => return Err( self.create_value_type_err(&[&ops.0, &ops.1]) ),
		}
	}

	fn apply_greater_equal(&self, calc_stack: &mut Vec<Value>) -> Result<Value, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Ok(Value::Bool(val1 >= val2)),
			(Value::String (val1), Value::String (val2)) => Ok(Value::Bool(val1 >= val2)),
			ops @ _ => return Err( self.create_value_type_err(&[&ops.0, &ops.1]) ),
		}
	}

	fn apply_less(&self, calc_stack: &mut Vec<Value>) -> Result<Value, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Ok(Value::Bool(val1 < val2)),
			(Value::String (val1), Value::String (val2)) => Ok(Value::Bool(val1 < val2)),
			ops @ _ => return Err( self.create_value_type_err(&[&ops.0, &ops.1]) ),
		}
	}

	fn apply_less_equal(&self, calc_stack: &mut Vec<Value>) -> Result<Value, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Ok(Value::Bool(val1 <= val2)),
			(Value::String (val1), Value::String (val2)) => Ok(Value::Bool(val1 <= val2)),
			ops @ _ => return Err( self.create_value_type_err(&[&ops.0, &ops.1]) ),
		}
	}

	fn apply_logical_and(&self, calc_stack: &mut Vec<Value>) -> Result<Value, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(Value::Bool (val1), Value::Bool (val2)) => Ok(Value::Bool(val1 && val2)),
			ops @ _ => return Err( self.create_value_type_err(&[&ops.0, &ops.1]) ),
		}
	}

	fn apply_logical_or(&self, calc_stack: &mut Vec<Value>) -> Result<Value, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(Value::Bool (val1), Value::Bool (val2)) => Ok(Value::Bool(val1 || val2)),
			ops @ _ => return Err( self.create_value_type_err(&[&ops.0, &ops.1]) ),
		}
	}

	fn apply_logical_xor(&self, calc_stack: &mut Vec<Value>) -> Result<Value, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(Value::Bool (val1), Value::Bool (val2)) => Ok(Value::Bool(val1 ^ val2)),
			ops @ _ => return Err( self.create_value_type_err(&[&ops.0, &ops.1]) ),
		}
	}
	
	fn take_2_operands<T>(calc_stack: &mut Vec<T>) -> Result<(T, T), OperatorErr> {
		let rhs: T = calc_stack
			.pop()
			.ok_or(OperatorErr::NotEnoughOperands { 
				provided_cnt: 0,
				required_cnt: 2, 
			} )?;
		let lhs: T = calc_stack
			.pop()
			.ok_or(OperatorErr::NotEnoughOperands { 
				provided_cnt: 1,
				required_cnt: 2, 
			} )?;
		Ok((lhs, rhs))
	}
	
	
	fn apply_unary_plus(&self, calc_stack: &mut Vec<Value>) -> Result<Value, OperatorErr> {
		let op = Self::take_1_operand(calc_stack)?;
		match op {
			Value::Float32 (val1) => Ok(Value::Float32(val1)),
			_ => return Err( self.create_value_type_err(&[&op]) ),
		}
	}
	
	fn apply_unary_minus(&self, calc_stack: &mut Vec<Value>) -> Result<Value, OperatorErr> {
		let op = Self::take_1_operand(calc_stack)?;
		match op {
			Value::Float32 (val1) => Ok(Value::Float32(-val1)),
			_ => return Err( self.create_value_type_err(&[&op]) ),
		}
	}
	
	fn apply_not(&self, calc_stack: &mut Vec<Value>) -> Result<Value, OperatorErr> {
		let op = Self::take_1_operand(calc_stack)?;
		match op {
			Value::Bool (val1) => Ok(Value::Bool(!val1)),
			_ => return Err( self.create_value_type_err(&[&op]) ),
		}
	}
	
	fn take_1_operand<T>(calc_stack: &mut Vec<T>) -> Result<T, OperatorErr> {
		let op: T = calc_stack
			.pop()
			.ok_or(OperatorErr::NotEnoughOperands { 
				provided_cnt: 0,
				required_cnt: 1, 
			} )?;
		Ok(op)
	}
	
	fn create_value_type_err(&self, values: &[&Value]) -> OperatorErr {
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
	}
	
	//---------------- Result data type -----------------------
	
	fn get_bin_plus_result_type(&self, calc_stack: &mut Vec<DataType>) -> Result<DataType, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(DataType::Float32, DataType::Float32) => Ok(DataType::Float32),
			(DataType::String, DataType::String) => Ok(DataType::String),
			ops @ _ => return Err( self.create_type_err(&[&ops.0, &ops.1]) ),
		}
	}
	
	fn get_bin_minus_result_type(&self, calc_stack: &mut Vec<DataType>) -> Result<DataType, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(DataType::Float32, DataType::Float32) => Ok(DataType::Float32),
			ops @ _ => return Err( self.create_type_err(&[&ops.0, &ops.1]) ),
		}
	}
	
	fn get_bin_mul_result_type(&self, calc_stack: &mut Vec<DataType>) -> Result<DataType, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(DataType::Float32, DataType::Float32) => Ok(DataType::Float32),
			ops @ _ => return Err( self.create_type_err(&[&ops.0, &ops.1]) ),
		}
	}
	
	fn get_bin_div_result_type(&self, calc_stack: &mut Vec<DataType>) -> Result<DataType, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(DataType::Float32, DataType::Float32) => Ok(DataType::Float32),
			ops @ _ => return Err( self.create_type_err(&[&ops.0, &ops.1]) ),
		}
	}
	
	fn get_bin_pow_result_type(&self, calc_stack: &mut Vec<DataType>) -> Result<DataType, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(DataType::Float32, DataType::Float32) => Ok(DataType::Float32),
			ops @ _ => return Err( self.create_type_err(&[&ops.0, &ops.1]) ),
		}
	}
	
	fn get_equal_result_type(&self, calc_stack: &mut Vec<DataType>) -> Result<DataType, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(DataType::Float32, DataType::Float32) => Ok(DataType::Bool),
			(DataType::String, DataType::String) => Ok(DataType::Bool),
			(DataType::Bool, DataType::Bool) => Ok(DataType::Bool),
			ops @ _ => return Err( self.create_type_err(&[&ops.0, &ops.1]) ),
		}
	}

	fn get_not_equal_result_type(&self, calc_stack: &mut Vec<DataType>) -> Result<DataType, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(DataType::Float32, DataType::Float32) => Ok(DataType::Bool),
			(DataType::String, DataType::String) => Ok(DataType::Bool),
			(DataType::Bool, DataType::Bool) => Ok(DataType::Bool),
			ops @ _ => return Err( self.create_type_err(&[&ops.0, &ops.1]) ),
		}
	}

	fn get_greater_result_type(&self, calc_stack: &mut Vec<DataType>) -> Result<DataType, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(DataType::Float32, DataType::Float32) => Ok(DataType::Bool),
			(DataType::String, DataType::String) => Ok(DataType::Bool),
			ops @ _ => return Err( self.create_type_err(&[&ops.0, &ops.1]) ),
		}
	}

	fn get_greater_equal_result_type(&self, calc_stack: &mut Vec<DataType>) -> Result<DataType, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(DataType::Float32, DataType::Float32) => Ok(DataType::Bool),
			(DataType::String, DataType::String) => Ok(DataType::Bool),
			ops @ _ => return Err( self.create_type_err(&[&ops.0, &ops.1]) ),
		}
	}

	fn get_less_result_type(&self, calc_stack: &mut Vec<DataType>) -> Result<DataType, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(DataType::Float32, DataType::Float32) => Ok(DataType::Bool),
			(DataType::String, DataType::String) => Ok(DataType::Bool),
			ops @ _ => return Err( self.create_type_err(&[&ops.0, &ops.1]) ),
		}
	}

	fn get_less_equal_result_type(&self, calc_stack: &mut Vec<DataType>) -> Result<DataType, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(DataType::Float32, DataType::Float32) => Ok(DataType::Bool),
			(DataType::String, DataType::String) => Ok(DataType::Bool),
			ops @ _ => return Err( self.create_type_err(&[&ops.0, &ops.1]) ),
		}
	}

	fn get_logical_and_result_type(&self, calc_stack: &mut Vec<DataType>) -> Result<DataType, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(DataType::Bool, DataType::Bool) => Ok(DataType::Bool),
			ops @ _ => return Err( self.create_type_err(&[&ops.0, &ops.1]) ),
		}
	}

	fn get_logical_or_result_type(&self, calc_stack: &mut Vec<DataType>) -> Result<DataType, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(DataType::Bool, DataType::Bool) => Ok(DataType::Bool),
			ops @ _ => return Err( self.create_type_err(&[&ops.0, &ops.1]) ),
		}
	}

	fn get_logical_xor_result_type(&self, calc_stack: &mut Vec<DataType>) -> Result<DataType, OperatorErr> {
		let (lhs, rhs) = Self::take_2_operands(calc_stack)?;
		match (lhs, rhs) {
			(DataType::Bool, DataType::Bool) => Ok(DataType::Bool),
			ops @ _ => return Err( self.create_type_err(&[&ops.0, &ops.1]) ),
		}
	}
	
	fn get_unary_plus_result_type(&self, calc_stack: &mut Vec<DataType>) -> Result<DataType, OperatorErr> {
		let op = Self::take_1_operand(calc_stack)?;
		match op {
			DataType::Float32 => Ok(DataType::Float32),
			_ => return Err( self.create_type_err(&[&op]) ),
		}
	}
	
	fn get_unary_minus_result_type(&self, calc_stack: &mut Vec<DataType>) -> Result<DataType, OperatorErr> {
		let op = Self::take_1_operand(calc_stack)?;
		match op {
			DataType::Float32 => Ok(DataType::Float32),
			_ => return Err( self.create_type_err(&[&op]) ),
		}
	}
	
	fn get_not_result_type(&self, calc_stack: &mut Vec<DataType>) -> Result<DataType, OperatorErr> {
		let op = Self::take_1_operand(calc_stack)?;
		match op {
			DataType::Bool => Ok(DataType::Bool),
			_ => return Err( self.create_type_err(&[&op]) ),
		}
	}
	
	fn create_type_err(&self, values: &[&DataType]) -> OperatorErr {		
		OperatorErr::WrongType { 
			descr: String::from(format!(
				"Operator {:?} cannot be applied to type(-s) {:?}", 
				self, 
				values))
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
	UnexpectedToken (CodePos),
	UnpairedBracket (CodePos),
	ExpectedExprButFound (CodePos),
	Operator { 
		err: OperatorErr, 
		pos: CodePos, 
	},
}

impl std::fmt::Display for ExprErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ExprErr::UnexpectedToken (_) => write!(f, "Unexpected token"),
			ExprErr::UnpairedBracket (_) => write!(f, "Unpaired bracket"),
			ExprErr::ExpectedExprButFound (_) => write!(f, "Expected arithmetical expression, but found"),
			ExprErr::Operator { err, .. } => match err {
				OperatorErr::WrongType { descr } => write!(f, "{}", descr),
				OperatorErr::NotEnoughOperands { provided_cnt, required_cnt } => 
					write!(f, "Expected {} operand(-s) for operator, but found {}", required_cnt, provided_cnt),
			},
		}
	}
}

fn unexpected(pos: CodePos) -> InterpErr {
	InterpErr::from (ExprErr::UnexpectedToken (pos))
}

fn unpaired_bracket(pos: CodePos) -> InterpErr {
	InterpErr::from (ExprErr::UnpairedBracket (pos))
}

//------------------------------- Tests ----------------------------------

#[cfg(test)]
mod tests {
	use super::super::token::*;
	use super::*;
	
	#[test]
	fn lookup_array_initialization() {
		assert_eq!(
			OP_ATTRS[ExprOperator::LogicalOr as usize], 
			OpInfo { arity: OpArity::Binary, rank: 0, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ExprOperator::LogicalAnd as usize], 
			OpInfo { arity: OpArity::Binary, rank: 1, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ExprOperator::LogicalXor as usize], 
			OpInfo { arity: OpArity::Binary, rank: 2, assot: OpAssot::Left });
			
		assert_eq!(
			OP_ATTRS[ExprOperator::Equal as usize], 
			OpInfo { arity: OpArity::Binary, rank: 3, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ExprOperator::NotEqual as usize], 
			OpInfo { arity: OpArity::Binary, rank: 3, assot: OpAssot::Left });
			
		assert_eq!(
			OP_ATTRS[ExprOperator::Less as usize], 
			OpInfo { arity: OpArity::Binary, rank: 4, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ExprOperator::LessEqual as usize], 
			OpInfo { arity: OpArity::Binary, rank: 4, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ExprOperator::Greater as usize], 
			OpInfo { arity: OpArity::Binary, rank: 4, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ExprOperator::GreaterEqual as usize], 
			OpInfo { arity: OpArity::Binary, rank: 4, assot: OpAssot::Left });
			
		assert_eq!(
			OP_ATTRS[ExprOperator::BinPlus as usize], 
			OpInfo { arity: OpArity::Binary, rank: 5, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ExprOperator::BinMinus as usize], 
			OpInfo { arity: OpArity::Binary, rank: 5, assot: OpAssot::Left });
			
		assert_eq!(
			OP_ATTRS[ExprOperator::Div as usize], 
			OpInfo { arity: OpArity::Binary, rank: 6, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ExprOperator::Mul as usize], 
			OpInfo { arity: OpArity::Binary, rank: 6, assot: OpAssot::Left });
			
		assert_eq!(
			OP_ATTRS[ExprOperator::UnPlus as usize], 
			OpInfo { arity: OpArity::Unary, rank: 7, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ExprOperator::UnMinus as usize], 
			OpInfo { arity: OpArity::Unary, rank: 7, assot: OpAssot::Left });
		assert_eq!(
			OP_ATTRS[ExprOperator::Not as usize], 
			OpInfo { arity: OpArity::Unary, rank: 7, assot: OpAssot::Left });
			
		assert_eq!(
			OP_ATTRS[ExprOperator::Pow as usize], 
			OpInfo { arity: OpArity::Binary, rank: 8, assot: OpAssot::Right });
			
		assert_eq!(
			OP_ATTRS[ExprOperator::DotMemberAccess as usize], 
			OpInfo { arity: OpArity::Binary, rank: 9, assot: OpAssot::Left });
	}
	
	#[test]
	fn check_stack_creation_and_arithmetic_calc() {
		test_expr_and_its_stack_eq_and_value("3.125;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (3.125_f32))),
		],
		Value::Float32(3.125_f32));
		
		test_expr_and_its_stack_eq_and_value("3.125 + 5.4;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (3.125_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (5.4_f32))),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
		],
		Value::Float32(3.125_f32 + 5.4_f32));
		
		test_expr_and_its_stack_eq_and_value("3.125 + 5.4 * 2.46;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (3.125_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (5.4_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2.46_f32))),
			SymbolKind::ExprOperator (ExprOperator::Mul),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
		],
		Value::Float32(3.125_f32 + 5.4_f32 * 2.46_f32));
		
		test_expr_and_its_stack_eq_and_value("3.125 + 0 + 5.25 * 2.25 - 3.25 / 2 * 4.25;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (3.125_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (0_f32))),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (5.25_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2.25_f32))),
			SymbolKind::ExprOperator (ExprOperator::Mul),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (3.25_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::ExprOperator (ExprOperator::Div),
			SymbolKind::Operand (Operand::Value (Value::Float32 (4.25_f32))),
			SymbolKind::ExprOperator (ExprOperator::Mul),
			SymbolKind::ExprOperator (ExprOperator::BinMinus),
		],
		Value::Float32(3.125_f32 + 0.0_f32 + 5.25_f32 * 2.25_f32 - 3.25_f32 / 2.0_f32 * 4.25_f32));
		
		test_expr_and_its_stack_eq_and_value("3.125 + -5.25 * 2.25;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (3.125_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (5.25_f32))),
			SymbolKind::ExprOperator (ExprOperator::UnMinus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2.25_f32))),
			SymbolKind::ExprOperator (ExprOperator::Mul),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
		],
		Value::Float32(3.125_f32 + -5.25_f32 * 2.25_f32));
		
		test_expr_and_its_stack_eq_and_value("2.5 * ---5.5;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (2.5_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (5.5_f32))),
			SymbolKind::ExprOperator (ExprOperator::UnMinus),
			SymbolKind::ExprOperator (ExprOperator::UnMinus),
			SymbolKind::ExprOperator (ExprOperator::UnMinus),
			SymbolKind::ExprOperator (ExprOperator::Mul),
		],
		Value::Float32(2.5_f32 * ---5.5_f32));
		
		test_expr_and_its_stack_eq_and_value("1.125 * (3.125 + 2.125);", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (1.125_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (3.125_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2.125_f32))),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::ExprOperator (ExprOperator::Mul),
		],
		Value::Float32(1.125_f32 * (3.125_f32 + 2.125_f32)));
		
		test_expr_and_its_stack_eq_and_value("33 + (1 + 2 * (3 + 4) + 5) / 10 - 30;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (33_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (1_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (3_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (4_f32))),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::ExprOperator (ExprOperator::Mul),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (5_f32))),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (10_f32))),
			SymbolKind::ExprOperator (ExprOperator::Div),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (30_f32))),
			SymbolKind::ExprOperator (ExprOperator::BinMinus),
		],
		Value::Float32(33_f32 + (1_f32 + 2_f32 * (3_f32 + 4_f32) + 5_f32) / 10_f32 - 30_f32));
		
		test_expr_and_its_stack_eq_and_value("-(8 - 2.125 * 5.125 + 4.125) / -3.125;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (8_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2.125_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (5.125_f32))),
			SymbolKind::ExprOperator (ExprOperator::Mul),
			SymbolKind::ExprOperator (ExprOperator::BinMinus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (4.125_f32))),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::ExprOperator (ExprOperator::UnMinus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (3.125_f32))),
			SymbolKind::ExprOperator (ExprOperator::UnMinus),
			SymbolKind::ExprOperator (ExprOperator::Div),
		],
		Value::Float32(-(8_f32 - 2.125_f32 * 5.125_f32 + 4.125_f32) / -3.125_f32));
		
		test_expr_and_its_stack_eq_and_value("33 + (1 + 2 * (3 + 4) + 5) / 10 - 30;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (33_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (1_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (3_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (4_f32))),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::ExprOperator (ExprOperator::Mul),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (5_f32))),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (10_f32))),
			SymbolKind::ExprOperator (ExprOperator::Div),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (30_f32))),
			SymbolKind::ExprOperator (ExprOperator::BinMinus),
		],
		Value::Float32(33_f32 + (1_f32 + 2_f32 * (3_f32 + 4_f32) + 5_f32) / 10_f32 - 30_f32));
		
		test_expr_and_its_stack_eq_and_value("2^2;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::ExprOperator (ExprOperator::Pow),
		],
		Value::Float32(2_f32.powf(2_f32)));
		
		test_expr_and_its_stack_eq_and_value("-2^2+4;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::ExprOperator (ExprOperator::Pow),
			SymbolKind::ExprOperator (ExprOperator::UnMinus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (4_f32))),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
		],
		Value::Float32(-2_f32.powf(2_f32) + 4_f32));
		
		test_expr_and_its_stack_eq_and_value("3^1^2;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (3_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (1_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::ExprOperator (ExprOperator::Pow),
			SymbolKind::ExprOperator (ExprOperator::Pow),
		],
		Value::Float32(3_f32.powf(1_f32.powf(2_f32))));
		
		let zero_pos = CodePos::from(CharPos::new());
		
		test_expr_and_its_stack_eq("a + add(2, 4) + @add(4, 9);", vec![
			SymbolKind::Operand (Operand::Variable (String::from ("a"))),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("add"),
				arg_exprs: vec![
					// arg 1
					Expr { 
						pos: zero_pos,
						expr_stack: vec![
							Symbol {
								kind: SymbolKind::Operand (Operand::Value (Value::from(2_f32))),
								pos: zero_pos,
							},
						],
					},
					
					// arg 2
					Expr {
						pos: zero_pos,
						expr_stack: vec![
							Symbol {
								kind: SymbolKind::Operand (Operand::Value (Value::from(4_f32))),
								pos: zero_pos,
							},
						],
					},
				],
			}),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::Builtin,
				func_name: new_name_token("add"),
				arg_exprs: vec![
					// arg 1
					Expr { 
						pos: zero_pos,
						expr_stack: vec![
							Symbol {
								kind: SymbolKind::Operand (Operand::Value (Value::from(4_f32))),
								pos: zero_pos,
							},
						],
					},
					
					// arg 2
					Expr {
						pos: zero_pos,
						expr_stack: vec![
							Symbol {
								kind: SymbolKind::Operand (Operand::Value (Value::from(9_f32))),
								pos: zero_pos,
							},
						],
					},
				],
			}),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
		]);
		
		test_expr_and_its_stack_eq("a.foo1() + b.foo2();", vec![
			SymbolKind::Operand (Operand::Variable (String::from ("a"))),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("foo1"),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
			SymbolKind::Operand (Operand::Variable (String::from ("b"))),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("foo2"),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
		]);
		
		test_expr_and_its_stack_eq("2 * a.foo1(c.foo3() - 3) ^ b.foo2() / d.foo4();", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::Operand (Operand::Variable (String::from ("a"))),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("foo1"),
				arg_exprs: vec![
					// arg 1
					Expr {
						pos: zero_pos,
						expr_stack: vec![						
							Symbol { kind: SymbolKind::Operand (Operand::Variable (String::from ("c"))), pos: zero_pos, },
							Symbol { kind: SymbolKind::Operand (Operand::FuncCall {
								kind: FuncKind::UserDefined,
								func_name: new_name_token("foo3"),
								arg_exprs: Vec::new(),
							}), pos: zero_pos, },
							Symbol { kind: SymbolKind::ExprOperator (ExprOperator::DotMemberAccess), pos: zero_pos, },
							Symbol { kind: SymbolKind::Operand (Operand::Value (Value::Float32 (3_f32))), pos: zero_pos, },
							Symbol { kind: SymbolKind::ExprOperator (ExprOperator::BinMinus), pos: zero_pos, },
						],
					},
				],
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
			
			SymbolKind::Operand (Operand::Variable (String::from ("b"))),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("foo2"),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),

			SymbolKind::ExprOperator (ExprOperator::Pow),
			
			SymbolKind::ExprOperator (ExprOperator::Mul),
			
			SymbolKind::Operand (Operand::Variable (String::from ("d"))),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("foo4"),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
			
			SymbolKind::ExprOperator (ExprOperator::Div),
		]);
	}
	
	fn new_name_token(name: &str) -> NameToken {
		NameToken::new_with_pos(name, CodePos::from(CharPos::new()))
	}

	#[test]
	fn check_stack_creation_and_bool_calc() {
		test_expr_and_its_stack_eq_and_value("False;", vec![
			SymbolKind::Operand (Operand::Value (Value::Bool (false))),
		],
		Value::Bool(false));
		
		test_expr_and_its_stack_eq_and_value("True;", vec![
			SymbolKind::Operand (Operand::Value (Value::Bool (true))),
		],
		Value::Bool(true));
		
		test_expr_and_its_stack_eq_and_value("!True;", vec![
			SymbolKind::Operand (Operand::Value (Value::Bool (true))),
			SymbolKind::ExprOperator (ExprOperator::Not),
		],
		Value::Bool(false));
		
		test_expr_and_its_stack_eq_and_value("!False;", vec![
			SymbolKind::Operand (Operand::Value (Value::Bool (false))),
			SymbolKind::ExprOperator (ExprOperator::Not),
		],
		Value::Bool(true));
		
		test_expr_and_its_stack_eq_and_value("True == True;", vec![
			SymbolKind::Operand (Operand::Value (Value::Bool (true))),
			SymbolKind::Operand (Operand::Value (Value::Bool (true))),
			SymbolKind::ExprOperator (ExprOperator::Equal),
		],
		Value::Bool(true));
		
		test_expr_and_its_stack_eq_and_value("False == False;", vec![
			SymbolKind::Operand (Operand::Value (Value::Bool (false))),
			SymbolKind::Operand (Operand::Value (Value::Bool (false))),
			SymbolKind::ExprOperator (ExprOperator::Equal),
		],
		Value::Bool(true));
		
		test_expr_and_its_stack_eq_and_value("True != False;", vec![
			SymbolKind::Operand (Operand::Value (Value::Bool (true))),
			SymbolKind::Operand (Operand::Value (Value::Bool (false))),
			SymbolKind::ExprOperator (ExprOperator::NotEqual),
		],
		Value::Bool(true));
		
		test_expr_and_its_stack_eq_and_value("True land False;", vec![
			SymbolKind::Operand (Operand::Value (Value::Bool (true))),
			SymbolKind::Operand (Operand::Value (Value::Bool (false))),
			SymbolKind::ExprOperator (ExprOperator::LogicalAnd),
		],
		Value::Bool(false));
		
		test_expr_and_its_stack_eq_and_value("True lor False;", vec![
			SymbolKind::Operand (Operand::Value (Value::Bool (true))),
			SymbolKind::Operand (Operand::Value (Value::Bool (false))),
			SymbolKind::ExprOperator (ExprOperator::LogicalOr),
		],
		Value::Bool(true));
		
		test_expr_and_its_stack_eq_and_value("True lxor False;", vec![
			SymbolKind::Operand (Operand::Value (Value::Bool (true))),
			SymbolKind::Operand (Operand::Value (Value::Bool (false))),
			SymbolKind::ExprOperator (ExprOperator::LogicalXor),
		],
		Value::Bool(true));
		
		test_expr_and_its_stack_eq_and_value("2 > 3;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (3_f32))),
			SymbolKind::ExprOperator (ExprOperator::Greater),
		],
		Value::Bool(false));
		
		test_expr_and_its_stack_eq_and_value("!(2 > 3);", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (3_f32))),
			SymbolKind::ExprOperator (ExprOperator::Greater),
			SymbolKind::ExprOperator (ExprOperator::Not),
		],
		Value::Bool(true));
		
		test_expr_and_its_stack_eq_and_value("2 >= 3;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (3_f32))),
			SymbolKind::ExprOperator (ExprOperator::GreaterEqual),
		],
		Value::Bool(false));
		
		test_expr_and_its_stack_eq_and_value("2 < 3;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (3_f32))),
			SymbolKind::ExprOperator (ExprOperator::Less),
		],
		Value::Bool(true));
		
		test_expr_and_its_stack_eq_and_value("2 <= 3;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (3_f32))),
			SymbolKind::ExprOperator (ExprOperator::LessEqual),
		],
		Value::Bool(true));
		
		test_expr_and_its_stack_eq_and_value("1 + 1 ^ 10 <= 27 / 9;", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (1_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (1_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (10_f32))),
			SymbolKind::ExprOperator (ExprOperator::Pow),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
			SymbolKind::Operand (Operand::Value (Value::Float32 (27_f32))),
			SymbolKind::Operand (Operand::Value (Value::Float32 (9_f32))),
			SymbolKind::ExprOperator (ExprOperator::Div),
			SymbolKind::ExprOperator (ExprOperator::LessEqual),
		],
		Value::Bool(true));
	}
	
	fn test_expr_and_its_stack_eq_and_value(
		expr_str: &str, 
		correct_expr_stack: Vec<SymbolKind>,
		result: Value
	) {
		let mut tokens_iter = TokensIter::new();	
		tokens_iter.push_string(expr_str.to_string());
		
		let expr_stack: Vec<Symbol> = Expr::create_stack(
			&mut tokens_iter, 
			ExprContext::new(ExprContextKind::ValueToAssign)).unwrap();
		
		let syms_expr_stack: Vec<SymbolKind> = expr_stack.iter().map(|Symbol { kind, .. }| kind.clone()).collect();
		
		if syms_expr_stack == correct_expr_stack { 
			let mut tokens_iter = TokensIter::new();	
			tokens_iter.push_string(expr_str.to_string());
		
			let expr = Expr::new(&mut tokens_iter, ExprContextKind::ValueToAssign).unwrap();
			
			let mut memory = Memory::new();
			let builtin_funcs_def_list = BuiltinFuncsDefList::new();
			
			let ans: Value = expr.calc(&mut memory, &builtin_funcs_def_list);
			
			if ans != result {
				panic!("Wrong result for code '{}': {:?} != {:?}", expr_str, ans, result);
			}
			
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
	
	
	fn test_expr_and_its_stack_eq(
		expr_str: &str, 
		correct_expr_stack: Vec<SymbolKind>,
	) {
		let mut tokens_iter = TokensIter::new();	
		tokens_iter.push_string(expr_str.to_string());
		
		let expr_stack: Vec<Symbol> = Expr::create_stack(
			&mut tokens_iter, 
			ExprContext::new(ExprContextKind::ValueToAssign)).unwrap();
		
		let syms_expr_stack: Vec<SymbolKind> = expr_stack.iter().map(|Symbol { kind, .. }| kind.clone()).collect();
		
		if syms_expr_stack == correct_expr_stack {		
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