use super::token::{Token, TokenContent, TokensIter, Operator, Bracket, StatementOp, Keyword, TokenErr};
use super::InterpErr;
use super::value::Value;
use super::data_type::{DataType, Primitive};
use super::builtin_func::BuiltinFuncDef;
use super::user_func::{UserFuncArg, UserFuncDef};
use super::utils::{CharPos, CodePos, NameToken};
use super::statement::FuncKind;
use super::context::Context;
use std::rc::Rc;

#[derive(Debug)]
pub struct Expr {
	expr_stack: Rc<Vec<Symbol>>,
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
			expr_stack: Rc::new(expr_stack),
			pos: CodePos::new(pos_begin, pos_end),
		} )
	}
	
	pub fn calc(&self, context: &Context) -> Value {
		let mut calc_stack = Vec::<Symbol>::with_capacity(self.expr_stack.len());
		
		for sym in self.expr_stack.iter() {
			match &sym.kind {
				SymbolKind::Operand (_) => calc_stack.push(sym.clone()), // TODO: avoid cloning symbols
				
				SymbolKind::LeftBracket => unreachable!(),
				
				SymbolKind::ExprOperator (op) => {
					let value: Value = op.apply(&mut calc_stack, context);
					calc_stack.push(Symbol { 
						pos: sym.pos, 
						kind: SymbolKind::Operand (Operand::Value(value)),
					}); // TODO: somehow place DataType here, not a massive symbol
				},
			}
		}
		
		let result: Value = calc_stack.pop()
			.unwrap()
			.unwrap_operand()
			.calc_in_place(context);
		assert_eq!(calc_stack.pop(), None);
		
		result
	}
	
	pub fn check_and_calc_data_type(&self, check_context: &Context) -> Result<DataType, InterpErr> {		
		assert!(self.expr_stack.len() > 0);
		
		let mut type_calc_stack = Vec::<Symbol>::with_capacity(self.expr_stack.len());
		
		for sym in self.expr_stack.iter() {
			match &sym.kind {
				SymbolKind::Operand (_) => type_calc_stack.push(sym.clone()), // TODO: avoid cloning symbols
				
				SymbolKind::LeftBracket => unreachable!(),
				
				SymbolKind::ExprOperator (op) => {
					let dt: DataType = op.get_result_data_type(&mut type_calc_stack, check_context, sym.pos)?;
					type_calc_stack.push(Symbol { 
						pos: sym.pos, 
						kind: SymbolKind::Operand (Operand::Value(dt.default_value())),
					}); // TODO: somehow place DataType here, not a massive symbol
				}, 
			}
		}
		
		let result: DataType = type_calc_stack.pop()
			.unwrap()
			.unwrap_operand()
			.check_and_calc_data_type_in_place(check_context)
			.unwrap();
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
						Keyword::Struct | 
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
				
				let func_name = NameToken::new_with_pos(name, pos);
				
				let sym = Symbol::new_func_call(kind, func_name, arg_exprs);
				Ok(sym)
			},
			_ => {
				let name = NameToken::new_with_pos(name, pos);
				let sym = Symbol::new_name(name);
				Ok(sym)
			},
		}
	}
}

impl Eq for Expr {}

impl PartialEq for Expr {
	fn eq(&self, other: &Self) -> bool {
		Vec::<Symbol>::eq(&*self.expr_stack, &*other.expr_stack)
	}
}

impl Clone for Expr {
	fn clone(&self) -> Self {
		Self {
			pos: self.pos,
			expr_stack: Rc::clone(&self.expr_stack),
		}
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
					Keyword::Struct | 
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
	fn new_name(name_tok: NameToken) -> Self {
		Self {
			pos: name_tok.pos(),
			kind: SymbolKind::Operand( Operand::Variable (name_tok) ),
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

	fn unwrap_operand(self) -> Operand {
		match self.kind {
			SymbolKind::Operand (op) => op,
			_ => panic!("Wrong input: {:?}", &self),
		}
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
	Variable (NameToken),
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

//------------------------------- Operand ----------------------------------

impl Operand {
	fn check_and_calc_data_type_in_place(&self, check_context: &Context) -> Result<DataType, InterpErr> {
		let dt: DataType = match self {
			Operand::Value (val) => val.get_type().clone(),
			Operand::Variable (name) =>
				check_context.get_variable_value(&name)?.get_type().clone(),
			Operand::FuncCall { kind, func_name, arg_exprs } => {
				match kind {
					FuncKind::Builtin => {
						let f: &BuiltinFuncDef = check_context.find_builtin_func_def(&func_name).unwrap();
				
						f.check_args(&func_name, arg_exprs, check_context)?;
						f.return_type().clone()
					},
					FuncKind::UserDefined => {
						let f: &UserFuncDef = check_context.find_func_def(&func_name).unwrap();
				
						f.check_args(arg_exprs, check_context)?;
						f.return_type().clone()
					},
				}
			},
		};
		Ok(dt)
	}
	
	fn calc_in_place(&self, context: &Context) -> Value {
		match self {
			Operand::Value (val) => val.clone(), // TODO: try do it without cloning values
			Operand::Variable (name) => context.get_variable_value(&name).unwrap().clone(),
			Operand::FuncCall { kind, func_name, arg_exprs } => {
				match kind {
					FuncKind::Builtin => {
						let f: &BuiltinFuncDef = context.find_builtin_func_def(&func_name).unwrap();
				
						let mut args_values = Vec::<Value>::with_capacity(arg_exprs.len());
						
						for expr in arg_exprs {
							let value: Value = expr.calc(context);
							args_values.push(value);
						}
						
						f.call(args_values).unwrap()
					},
					FuncKind::UserDefined => {
						let f: &UserFuncDef = context.find_func_def(&func_name).unwrap();
				
						let mut args_values = Vec::<Value>::with_capacity(arg_exprs.len());
						
						for expr in arg_exprs {
							let value: Value = expr.calc(context);
							args_values.push(value);
						}
						
						let mut next_context = context.new_stack_frame_context();
						
						let func_args: &Vec<UserFuncArg> = f.args();
						for i in 0..args_values.len() {
							next_context.add_variable(
								func_args[i].name().clone(),
								func_args[i].data_type().clone(),
								Some(args_values[i].clone())).unwrap();
						}
						
						f.call(&mut next_context).unwrap()
					},
				}
			},
		}
	}
}

//------------------------------- ExprOperator ----------------------------------

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

	fn apply(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {	
		use ExprOperator::*;
		
		if let DotMemberAccess = self {
			let rhs: Symbol = calc_stack
				.pop()
				.unwrap();
				
			let lhs: Symbol = calc_stack
				.pop()
				.unwrap();
			
			match (lhs.kind, rhs.kind) {
				(SymbolKind::Operand (Operand::Value (ref value)), SymbolKind::Operand (Operand::FuncCall {
					ref kind,
					ref func_name,
					ref arg_exprs,
				})) => {
					match kind {
						FuncKind::Builtin => {
							let func_def: &BuiltinFuncDef = value.get_type()
								.find_member_builtin_func(&func_name, context).unwrap();
							
							let mut args_values = Vec::<Value>::with_capacity(arg_exprs.len() + 1);
							args_values.push(value.clone());
							for expr in arg_exprs {
								let value: Value = expr.calc(context);
								args_values.push(value);
							}
							
							return func_def.call(args_values).unwrap();
						},
						
						FuncKind::UserDefined => todo!(),
					}
				},
				(SymbolKind::Operand (Operand::Variable (ref var_name)), SymbolKind::Operand (Operand::FuncCall {
					ref kind,
					ref func_name,
					ref arg_exprs,
				})) => {
					let value: &Value = context.get_variable_value(var_name).unwrap();
					match kind {
						FuncKind::Builtin => {
							let func_def: &BuiltinFuncDef = value.get_type()
								.find_member_builtin_func(&func_name, context).unwrap();
							
							let mut args_values = Vec::<Value>::with_capacity(arg_exprs.len() + 1);
							args_values.push(value.clone());
							for expr in arg_exprs {
								let value: Value = expr.calc(context);
								args_values.push(value);
							}
							
							return func_def.call(args_values).unwrap();
						},
						
						FuncKind::UserDefined => todo!(),
					}
				},
				_ => todo!(),
			}
		} else {
			match self {
				BinPlus => self.apply_bin_plus(calc_stack, context),
				BinMinus => self.apply_bin_minus(calc_stack, context),
				Div => self.apply_bin_div(calc_stack, context),
				Mul => self.apply_bin_mul(calc_stack, context),
				Pow => self.apply_bin_pow(calc_stack, context),
				UnPlus => self.apply_unary_plus(calc_stack, context),
				UnMinus => self.apply_unary_minus(calc_stack, context),
				Equal => self.apply_equal(calc_stack, context),
				NotEqual => self.apply_not_equal(calc_stack, context),
				Not => self.apply_not(calc_stack, context),
				Greater => self.apply_greater(calc_stack, context),
				GreaterEqual => self.apply_greater_equal(calc_stack, context),
				Less => self.apply_less(calc_stack, context),
				LessEqual => self.apply_less_equal(calc_stack, context),
				LogicalAnd => self.apply_logical_and(calc_stack, context),
				LogicalOr => self.apply_logical_or(calc_stack, context),
				LogicalXor => self.apply_logical_xor(calc_stack, context),
				DotMemberAccess => todo!(),
			}
		}
	}
	
	fn get_result_data_type(self, calc_stack: &mut Vec<Symbol>, check_context: &Context, operator_pos: CodePos) -> Result<DataType, InterpErr> {
		use ExprOperator::*;
		
		if let DotMemberAccess = self {
			let rhs: Symbol = calc_stack.pop()
				.ok_or(InterpErr::from(ExprErr::not_enough_operands_for_operator(operator_pos, 0, 2)) )?;
				
			let lhs: Symbol = calc_stack.pop()
				.ok_or(InterpErr::from(ExprErr::not_enough_operands_for_operator(operator_pos, 1, 2)) )?;
				
			match (lhs.kind, rhs.kind) {
				(SymbolKind::Operand (Operand::Value (ref value)), SymbolKind::Operand (Operand::FuncCall {
					ref kind,
					ref func_name,
					ref arg_exprs,
				})) => {
					match kind {
						FuncKind::Builtin => {
							let func_def: &BuiltinFuncDef = value.get_type()
								.find_member_builtin_func(&func_name, check_context)?;
							
							func_def.check_args_as_member_function(func_name, arg_exprs, value, lhs.pos, check_context)?;
							
							return Ok(func_def.return_type().clone());
						},
						
						FuncKind::UserDefined => todo!(),
					}
				},
				(SymbolKind::Operand (Operand::Variable (ref var_name)), SymbolKind::Operand (Operand::FuncCall {
					ref kind,
					ref func_name,
					ref arg_exprs,
				})) => {
					let value: &Value = check_context.get_variable_value(&var_name)?;
					match kind {
						FuncKind::Builtin => {
							let func_def: &BuiltinFuncDef = value.get_type()
								.find_member_builtin_func(&func_name, check_context)?;
							
							func_def.check_args_as_member_function(func_name, arg_exprs, value, lhs.pos, check_context)?;
							
							return Ok(func_def.return_type().clone());
						},
						
						FuncKind::UserDefined => todo!(),
					}
				},
				_ => todo!(),
			}
		} else {
			match OP_ATTRS[self as usize].arity {
				OpArity::Binary => {
					let rhs: DataType = calc_stack.pop()
						.ok_or(InterpErr::from(ExprErr::not_enough_operands_for_operator(operator_pos, 0, 2)) )?
						.unwrap_operand()
						.check_and_calc_data_type_in_place(check_context)?;
						
					let lhs: DataType = calc_stack.pop()
						.ok_or(InterpErr::from(ExprErr::not_enough_operands_for_operator(operator_pos, 1, 2)) )?
						.unwrap_operand()
						.check_and_calc_data_type_in_place(check_context)?;
					
					let result = match self {
						BinPlus => self.get_bin_plus_result_type(&rhs, &lhs),
						BinMinus => self.get_bin_minus_result_type(&rhs, &lhs),
						Div => self.get_bin_div_result_type(&rhs, &lhs),
						Mul => self.get_bin_mul_result_type(&rhs, &lhs),
						Pow => self.get_bin_pow_result_type(&rhs, &lhs),
						Equal => self.get_equal_result_type(&rhs, &lhs),
						NotEqual => self.get_not_equal_result_type(&rhs, &lhs),
						Greater => self.get_greater_result_type(&rhs, &lhs),
						GreaterEqual => self.get_greater_equal_result_type(&rhs, &lhs),
						Less => self.get_less_result_type(&rhs, &lhs),
						LessEqual => self.get_less_equal_result_type(&rhs, &lhs),
						LogicalAnd => self.get_logical_and_result_type(&rhs, &lhs),
						LogicalOr => self.get_logical_or_result_type(&rhs, &lhs),
						LogicalXor => self.get_logical_xor_result_type(&rhs, &lhs),
						_ => unreachable!(),
					};
					
					match result {
						Ok(dt) => Ok(dt),
						Err(()) => Err( InterpErr::from(ExprErr::wrong_operands_type_for_operator(
							self, operator_pos, &[&lhs, &rhs])) ),
					}
				},
				OpArity::Unary => {
					let op: DataType = calc_stack
						.pop()
						.ok_or(InterpErr::from(ExprErr::not_enough_operands_for_operator(operator_pos, 0, 1)) )?
						.unwrap_operand()
						.check_and_calc_data_type_in_place(check_context)?;
					
					let result = match self {
						UnPlus => self.get_unary_plus_result_type(&op),
						UnMinus => self.get_unary_minus_result_type(&op),
						Not => self.get_not_result_type(&op),
						_ => unreachable!(),
					};
					
					match result {
						Ok(dt) => Ok(dt),
						Err(()) => Err( InterpErr::from(ExprErr::wrong_operands_type_for_operator(
							self, operator_pos, &[&op])) ),
					}
				},
			}
		}
	}
	
	
	fn apply_bin_plus(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Float32(val1 + val2),
			(Value::String (val1), Value::String (val2)) => {
				let mut res: String = val1.clone();
				res.push_str(&val2);
				Value::String(res)
			},
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}
	
	fn apply_bin_minus(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Float32(val1 - val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}
	
	fn apply_bin_mul(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Float32(val1 * val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}
	
	fn apply_bin_div(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Float32(val1 / val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}
	
	fn apply_bin_pow(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Float32(val1.powf(val2)),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}
	
	fn apply_equal(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Bool(val1 == val2),
			(Value::String (val1), Value::String (val2)) => Value::Bool(val1 == val2),
			(Value::Bool (val1), Value::Bool (val2)) => Value::Bool(val1 == val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}

	fn apply_not_equal(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Bool(val1 != val2),
			(Value::String (val1), Value::String (val2)) => Value::Bool(val1 != val2),
			(Value::Bool (val1), Value::Bool (val2)) => Value::Bool(val1 != val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}

	fn apply_greater(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Bool(val1 > val2),
			(Value::String (val1), Value::String (val2)) => Value::Bool(val1 > val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}

	fn apply_greater_equal(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Bool(val1 >= val2),
			(Value::String (val1), Value::String (val2)) => Value::Bool(val1 >= val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}

	fn apply_less(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Bool(val1 < val2),
			(Value::String (val1), Value::String (val2)) => Value::Bool(val1 < val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}

	fn apply_less_equal(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Float32 (val1), Value::Float32 (val2)) => Value::Bool(val1 <= val2),
			(Value::String (val1), Value::String (val2)) => Value::Bool(val1 <= val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}

	fn apply_logical_and(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Bool (val1), Value::Bool (val2)) => Value::Bool(val1 && val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}

	fn apply_logical_or(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Bool (val1), Value::Bool (val2)) => Value::Bool(val1 || val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}

	fn apply_logical_xor(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let (lhs, rhs): (Value, Value) = Self::take_2_values(calc_stack, context);
		match (lhs, rhs) {
			(Value::Bool (val1), Value::Bool (val2)) => Value::Bool(val1 ^ val2),
			ops @ _ => panic!("Wrong types {:?} for operand {:?}", ops, self),
		}
	}
	
	fn take_2_values(calc_stack: &mut Vec<Symbol>, context: &Context) -> (Value, Value) {
		let rhs: Value = calc_stack
			.pop()
			.unwrap()
			.unwrap_operand()
			.calc_in_place(context);
			
		let lhs: Value = calc_stack
			.pop()
			.unwrap()
			.unwrap_operand()
			.calc_in_place(context);
			
		(lhs, rhs)
	}
	
	
	fn apply_unary_plus(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let op: Value = Self::take_1_value(calc_stack, context);
		match op {
			Value::Float32 (val1) => Value::Float32(val1),
			_ => panic!("Wrong type {:?} for operand {:?}", op, self),
		}
	}
	
	fn apply_unary_minus(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let op: Value = Self::take_1_value(calc_stack, context);
		match op {
			Value::Float32 (val1) => Value::Float32(-val1),
			_ => panic!("Wrong type {:?} for operand {:?}", op, self),
		}
	}
	
	fn apply_not(&self, calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let op: Value = Self::take_1_value(calc_stack, context);
		match op {
			Value::Bool (val1) => Value::Bool(!val1),
			_ => panic!("Wrong type {:?} for operand {:?}", op, self),
		}
	}
	
	fn take_1_value(calc_stack: &mut Vec<Symbol>, context: &Context) -> Value {
		let op: Value = calc_stack
			.pop()
			.unwrap()
			.unwrap_operand()
			.calc_in_place(context);
		
		op
	}
	
	//---------------- Result data type -----------------------
	
	fn get_bin_plus_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => 
				Ok(DataType::Primitive (Primitive::Float32)),
			(DataType::Primitive (Primitive::String), DataType::Primitive (Primitive::String)) => 
				Ok(DataType::Primitive (Primitive::String)),
			_ => return Err(()),
		}
	}
	
	fn get_bin_minus_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => Ok(DataType::Primitive (Primitive::Float32)),
			_ => return Err(()),
		}
	}
	
	fn get_bin_mul_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => Ok(DataType::Primitive (Primitive::Float32)),
			_ => return Err(()),
		}
	}
	
	fn get_bin_div_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => Ok(DataType::Primitive (Primitive::Float32)),
			_ => return Err(()),
		}
	}
	
	fn get_bin_pow_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => Ok(DataType::Primitive (Primitive::Float32)),
			_ => return Err(()),
		}
	}
	
	fn get_equal_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => Ok(DataType::Primitive (Primitive::Bool)),
			(DataType::Primitive (Primitive::String), DataType::Primitive (Primitive::String)) => Ok(DataType::Primitive (Primitive::Bool)),
			(DataType::Primitive (Primitive::Bool), DataType::Primitive (Primitive::Bool)) => Ok(DataType::Primitive (Primitive::Bool)),
			_ => return Err(()),
		}
	}

	fn get_not_equal_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => Ok(DataType::Primitive (Primitive::Bool)),
			(DataType::Primitive (Primitive::String), DataType::Primitive (Primitive::String)) => Ok(DataType::Primitive (Primitive::Bool)),
			(DataType::Primitive (Primitive::Bool), DataType::Primitive (Primitive::Bool)) => Ok(DataType::Primitive (Primitive::Bool)),
			_ => return Err(()),
		}
	}

	fn get_greater_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => Ok(DataType::Primitive (Primitive::Bool)),
			(DataType::Primitive (Primitive::String), DataType::Primitive (Primitive::String)) => Ok(DataType::Primitive (Primitive::Bool)),
			_ => return Err(()),
		}
	}

	fn get_greater_equal_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => Ok(DataType::Primitive (Primitive::Bool)),
			(DataType::Primitive (Primitive::String), DataType::Primitive (Primitive::String)) => Ok(DataType::Primitive (Primitive::Bool)),
			_ => return Err(()),
		}
	}

	fn get_less_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => Ok(DataType::Primitive (Primitive::Bool)),
			(DataType::Primitive (Primitive::String), DataType::Primitive (Primitive::String)) => Ok(DataType::Primitive (Primitive::Bool)),
			_ => return Err(()),
		}
	}

	fn get_less_equal_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Float32), DataType::Primitive (Primitive::Float32)) => Ok(DataType::Primitive (Primitive::Bool)),
			(DataType::Primitive (Primitive::String), DataType::Primitive (Primitive::String)) => Ok(DataType::Primitive (Primitive::Bool)),
			_ => return Err(()),
		}
	}

	fn get_logical_and_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Bool), DataType::Primitive (Primitive::Bool)) => Ok(DataType::Primitive (Primitive::Bool)),
			_ => return Err(()),
		}
	}

	fn get_logical_or_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Bool), DataType::Primitive (Primitive::Bool)) => Ok(DataType::Primitive (Primitive::Bool)),
			_ => return Err(()),
		}
	}

	fn get_logical_xor_result_type(&self, lhs: &DataType, rhs: &DataType) -> Result<DataType, ()> {
		match (lhs, rhs) {
			(DataType::Primitive (Primitive::Bool), DataType::Primitive (Primitive::Bool)) => Ok(DataType::Primitive (Primitive::Bool)),
			_ => return Err(()),
		}
	}

	
	fn get_unary_plus_result_type(&self, operand: &DataType) -> Result<DataType, ()> {
		match operand {
			DataType::Primitive (Primitive::Float32) => Ok(DataType::Primitive (Primitive::Float32)),
			_ => return Err(()),
		}
	}
	
	fn get_unary_minus_result_type(&self, operand: &DataType) -> Result<DataType, ()> {
		match operand {
			DataType::Primitive (Primitive::Float32) => Ok(DataType::Primitive (Primitive::Float32)),
			_ => return Err(()),
		}
	}
	
	fn get_not_result_type(&self, operand: &DataType) -> Result<DataType, ()> {
		match operand {
			DataType::Primitive (Primitive::Bool) => Ok(DataType::Primitive (Primitive::Bool)),
			_ => return Err(()),
		}
	}
}

//------------------------------- ExprErr ----------------------------------

#[derive(Debug, PartialEq, Eq)]
pub enum ExprErr {
	UnexpectedToken (CodePos),
	UnpairedBracket (CodePos),
	ExpectedExprButFound (CodePos),
	WrongOperandsTypeForOperator { 
		operator_pos: CodePos,
		descr: String,
	},
	NotEnoughOperandsForOperator { 
		operator_pos: CodePos,
		descr: String,
	},
}

impl ExprErr {
	fn not_enough_operands_for_operator(operator_pos: CodePos, provided_cnt: usize, required_cnt: usize) -> Self {
		ExprErr::NotEnoughOperandsForOperator { 
			operator_pos,
			descr: format!("Expected {} operand(-s) for operator, but found {}", required_cnt, provided_cnt),
		}
	}
	fn wrong_operands_type_for_operator(op: ExprOperator, operator_pos: CodePos, operands_types: &[&DataType]) -> Self {
		ExprErr::WrongOperandsTypeForOperator { 
			operator_pos,
			descr: format!("Operator '{:?}' can't be applied to operands with type(-s) {:?}", op, operands_types),
		}
	}
}

impl std::fmt::Display for ExprErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			ExprErr::UnexpectedToken (_) => write!(f, "Unexpected token"),
			ExprErr::UnpairedBracket (_) => write!(f, "Unpaired bracket"),
			ExprErr::ExpectedExprButFound (_) => write!(f, "Expected arithmetical expression, but found"),
			ExprErr::WrongOperandsTypeForOperator { ref descr, .. } => write!(f, "{}", descr),
			ExprErr::NotEnoughOperandsForOperator { ref descr, .. } => write!(f, "{}", descr),
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
	use super::super::primitive_type_member_funcs_list::PrimitiveTypeMemberFuncsList;
	
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
			SymbolKind::Operand (Operand::Variable (new_name_token("a"))),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("add"),
				arg_exprs: vec![
					// arg 1
					Expr { 
						pos: zero_pos,
						expr_stack: Rc::new(vec![
							Symbol {
								kind: SymbolKind::Operand (Operand::Value (Value::from(2_f32))),
								pos: zero_pos,
							},
						]),
					},
					
					// arg 2
					Expr {
						pos: zero_pos,
						expr_stack: Rc::new(vec![
							Symbol {
								kind: SymbolKind::Operand (Operand::Value (Value::from(4_f32))),
								pos: zero_pos,
							},
						]),
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
						expr_stack: Rc::new(vec![
							Symbol {
								kind: SymbolKind::Operand (Operand::Value (Value::from(4_f32))),
								pos: zero_pos,
							},
						]),
					},
					
					// arg 2
					Expr {
						pos: zero_pos,
						expr_stack: Rc::new(vec![
							Symbol {
								kind: SymbolKind::Operand (Operand::Value (Value::from(9_f32))),
								pos: zero_pos,
							},
						]),
					},
				],
			}),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
		]);
		
		test_expr_and_its_stack_eq("a.foo1() + b.foo2();", vec![
			SymbolKind::Operand (Operand::Variable (new_name_token("a"))),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("foo1"),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
			SymbolKind::Operand (Operand::Variable (new_name_token("b"))),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("foo2"),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
		]);
		
		test_expr_and_its_stack_eq("foo1().foo3() + b.foo2();", vec![
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("foo1"),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("foo3"),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
			SymbolKind::Operand (Operand::Variable (new_name_token("b"))),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("foo2"),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
			SymbolKind::ExprOperator (ExprOperator::BinPlus),
		]);
		
		test_expr_and_its_stack_eq("2 * a.foo1(c.foo3().foo5() - 3) ^ b.foo2() / d.foo4();", vec![
			SymbolKind::Operand (Operand::Value (Value::Float32 (2_f32))),
			SymbolKind::Operand (Operand::Variable (new_name_token("a"))),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("foo1"),
				arg_exprs: vec![
					// arg 1
					Expr {
						pos: zero_pos,
						expr_stack: Rc::new(vec![
							Symbol { kind: SymbolKind::Operand (Operand::Variable (new_name_token("c"))), pos: zero_pos, },
							Symbol { kind: SymbolKind::Operand (Operand::FuncCall {
								kind: FuncKind::UserDefined,
								func_name: new_name_token("foo3"),
								arg_exprs: Vec::new(),
							}), pos: zero_pos, },
							Symbol { kind: SymbolKind::ExprOperator (ExprOperator::DotMemberAccess), pos: zero_pos, },
							Symbol { kind: SymbolKind::Operand (Operand::FuncCall {
								kind: FuncKind::UserDefined,
								func_name: new_name_token("foo5"),
								arg_exprs: Vec::new(),
							}), pos: zero_pos, },
							Symbol { kind: SymbolKind::ExprOperator (ExprOperator::DotMemberAccess), pos: zero_pos, },
							Symbol { kind: SymbolKind::Operand (Operand::Value (Value::Float32 (3_f32))), pos: zero_pos, },
							Symbol { kind: SymbolKind::ExprOperator (ExprOperator::BinMinus), pos: zero_pos, },
						]),
					},
				],
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),
			
			SymbolKind::Operand (Operand::Variable (new_name_token("b"))),
			SymbolKind::Operand (Operand::FuncCall {
				kind: FuncKind::UserDefined,
				func_name: new_name_token("foo2"),
				arg_exprs: Vec::new(),
			}),
			SymbolKind::ExprOperator (ExprOperator::DotMemberAccess),

			SymbolKind::ExprOperator (ExprOperator::Pow),
			
			SymbolKind::ExprOperator (ExprOperator::Mul),
			
			SymbolKind::Operand (Operand::Variable (new_name_token("d"))),
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
		NameToken::new_with_pos(name.to_string(), CodePos::from(CharPos::new()))
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
			
			let builtin_func_defs = Vec::<BuiltinFuncDef>::new();
			let primitive_type_member_funcs_list = PrimitiveTypeMemberFuncsList::new();
			let context = Context::new(
				&builtin_func_defs,
				&primitive_type_member_funcs_list);
			
			let ans: Value = expr.calc(&context);
			
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