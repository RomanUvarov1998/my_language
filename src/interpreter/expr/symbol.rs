use super::super::token::{TokenContent, Keyword};
use super::super::InterpErr;
use super::super::value::Value;
use super::super::data_type::DataType;
use super::super::builtin_func::BuiltinFuncDef;
use super::super::user_func::{UserFuncArg, UserFuncDef};
use super::super::utils::{CodePos, NameToken};
use super::super::statement::FuncKind;
use super::super::context::Context;
use super::Expr;
use super::expr_operator::ExprOperator;

//------------------------------- Symbol ----------------------------------

#[derive(Debug, Clone)]
pub struct Symbol {
	pub kind: SymbolKind,
	pub pos: CodePos,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolKind {
	Operand (Operand),
	LeftBracket,
	ExprOperator (ExprOperator),
}

impl Symbol {
	pub fn new_number(num: f32, pos: CodePos) -> Self {
		Self {
			kind: SymbolKind::Operand( Operand::Value (Value::from(num)) ),
			pos,
		}
	}
	pub fn new_name(name_tok: NameToken) -> Self {
		Self {
			pos: name_tok.pos(),
			kind: SymbolKind::Operand( Operand::Variable (name_tok) ),
		}
	}
	pub fn new_func_call(kind: FuncKind, func_name: NameToken, arg_exprs: Vec<Expr>) -> Self {
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
	pub fn new_string_literal(content: String, pos: CodePos) -> Self {
		Self {
			kind: SymbolKind::Operand( Operand::Value (Value::from(content)) ),
			pos,
		}
	}
	pub fn new_bool_literal(kw: Keyword, pos: CodePos) -> Self {
		let kind: SymbolKind = match kw {
			Keyword::True => SymbolKind::Operand (Operand::Value (Value::from(true))),
			Keyword::False => SymbolKind::Operand (Operand::Value (Value::from(false))),
			_ => panic!("Unexpected input: {:?}", kw),
		};
		Self { kind, pos }
	}
	pub fn new_left_bracket(pos: CodePos) -> Self {
		Self { 
			kind: SymbolKind::LeftBracket, 
			pos,
		}
	}
	pub fn new_un_pref_op(tc: TokenContent, pos: CodePos) -> Self {
		Self { 
			kind: SymbolKind::ExprOperator( ExprOperator::new_un_pref(tc) ), 
			pos,
		}
	}

	pub fn kind(&self) -> &SymbolKind {
		&self.kind
	}
	pub fn pos(&self) -> CodePos {
		self.pos
	}

	pub fn unwrap_operand(self) -> Operand {
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
pub enum Operand {
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
	pub fn check_and_calc_data_type_in_place(&self, check_context: &Context) -> Result<DataType, InterpErr> {
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
	
	pub fn calc_in_place(&self, context: &Context) -> Value {
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
