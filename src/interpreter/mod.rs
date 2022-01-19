mod memory;
mod arithmetic_expr;
mod string_char;
mod token;
mod statement;
mod var_data;
mod func_data;

use statement::*;
use memory::*;
use func_data::{FuncsDefList, FuncDef, FuncArg, FuncErr};
use var_data::{VarErr, VarValue, DataType};

pub struct Interpreter {
	memory: Memory,
	builtin_func_defs: FuncsDefList,
	statements_iter: StatementsIter,
}

impl Interpreter {
	pub fn new() -> Self {
		let mut builtin_func_defs = FuncsDefList::new();
		
		builtin_func_defs.add(FuncDef::new(
			"print".to_string(),
			vec![
				FuncArg::new("value".to_string(), DataType::Float32),
			]
		)).unwrap();
		
		builtin_func_defs.add(FuncDef::new(
			"exit".to_string(),
			Vec::new()
		)).unwrap();
		
		Self {
			memory: Memory::new(),
			builtin_func_defs,
			statements_iter: StatementsIter::new(),
		}
	}
	
	pub fn run(&mut self, code: &str) -> Result<InterpInnerSignal, InterpErr> {		
		self.statements_iter.push_string(code.to_string());
	
		while let Some(statement_result) = self.statements_iter.next() {
			match statement_result? {
				Statement::WithVariable (st) => match st {
					WithVariable::Declare { var_name, data_type } => 
						self.memory.add_variable(var_name, data_type)?,
					WithVariable::DeclareSet { var_name, data_type, value_expr } => {
						self.memory.add_variable(var_name.clone(), data_type)?;
						self.memory.set_variable(&var_name, value_expr.calc(&self.memory)?)?;
					},
					WithVariable::Set { var_name, value_expr } => 
						self.memory.set_variable(&var_name, value_expr.calc(&self.memory)?)?,
				},
				Statement::FuncCall { kind, name, arg_exprs } => {
					let types: Vec<DataType> = arg_exprs
						.iter()
						.map(|expr| expr.get_data_type())
						.collect();
					
					match kind {
						FuncKind::Builtin => {
							let func_def: &FuncDef = self.builtin_func_defs.try_find(&name)?;
							func_def.check_args(types)?;
							
							let mut arg_vals = Vec::<VarValue>::with_capacity(arg_exprs.len());
							for expr in arg_exprs {
								arg_vals.push(expr.calc(&self.memory)?);
							}
							
							if let InterpInnerSignal::Exit = self.call_builtin_func(&name, arg_vals)? {
								return Ok( InterpInnerSignal::Exit );
							}
						},
						FuncKind::UserDefined => {
							todo!();
							//self.memory.call_func(name, args)?;
						},
					}
				},
			}
		}	
		
		Ok( InterpInnerSignal::CanContinue )
	}
	
	fn call_builtin_func(&self, name: &str, args_values: Vec<VarValue>) -> Result<InterpInnerSignal, InterpErr> {
		match name {
			"print" => {
				println!("{:?}", args_values[0]);
				Ok( InterpInnerSignal::CanContinue )
			},
			"exit" => {
				Ok( InterpInnerSignal::Exit )
			},
			_ => unreachable!(),
		}
	}
}

#[derive(Debug, PartialEq, Eq)]
pub enum InterpInnerSignal {
	CanContinue,
	Exit,
}

#[derive(Debug, PartialEq, Eq)]
pub enum InterpErr {
	Token (TokenErr),
	ArithmExpr (ArithmExprErr),
	Statement (StatementErr),
	Var (VarErr),
	Func (FuncErr),
}

impl std::fmt::Display for InterpErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {		
		match self {
			InterpErr::Token (err) => write!(f, "{}", err),
			InterpErr::ArithmExpr (err) => write!(f, "{}", err),
			InterpErr::Statement (err) => write!(f, "{}", err),
			InterpErr::Var (err) => write!(f, "{}", err),
			InterpErr::Func (err) => write!(f, "{}", err),
		}
	}
}

fn display_error_pos(f: &mut std::fmt::Formatter<'_>, pos_begin: usize, pos_end: usize) -> std::fmt::Result {
	for _ in 0..pos_begin {
		write!(f, "_")?;
	}
	for _ in pos_begin..=pos_end {
		write!(f, "^")?;
	}
	writeln!(f, "")
}

use token::TokenErr;
impl From<TokenErr> for InterpErr {
	fn from(err: TokenErr) -> InterpErr {
        InterpErr::Token(err)
    }
}
impl From<&TokenErr> for InterpErr {
	fn from(err: &TokenErr) -> InterpErr {
        InterpErr::Token(TokenErr::clone(err))
    }
}

use arithmetic_expr::ArithmExprErr;
impl From<ArithmExprErr> for InterpErr {
	fn from(err: ArithmExprErr) -> InterpErr {
        InterpErr::ArithmExpr(err)
	}
}

impl From<StatementErr> for InterpErr {
	fn from(err: StatementErr) -> InterpErr {
        InterpErr::Statement(err)
    }
}

impl From<VarErr> for InterpErr {
	fn from(err: VarErr) -> InterpErr {
		InterpErr::Var(err)
    }
}

impl From<FuncErr> for InterpErr {
	fn from(err: FuncErr) -> InterpErr {
        InterpErr::Func(err)
    }
}