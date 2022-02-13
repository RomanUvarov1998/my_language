use super::utils::{NameToken, CodePos, CharPos};
use super::statement::{Statement, StatementKind, ReturningBody, ParsedFuncArgDef, ConditionalBody, UnconditionalBody};
use std::collections::HashMap;
use super::struct_def::{StructDef, StructFieldDef};
use super::context::Context;
use super::data_type::{DataType, BuiltinType};
use super::user_func::{UserFuncDef, UserFuncArg};
use super::expr::{Expr, Symbol, SymbolKind, Operand, StructLiteralField};
use super::utils::HashMapInsertPanic;

//------------------------- DataTypeTemplate -----------------------

#[derive(Debug)]
pub struct DataTypeTemplate {
	name: &'static str,
	fields: HashMap<String, StructFieldDefTemplate>,
	user_funcs: HashMap<String, UserFuncDefTemplate>,
}

impl DataTypeTemplate {
	pub fn new(name: &'static str) -> Self {
		Self {
			name,
			fields: HashMap::new(),
			user_funcs: HashMap::new(),
		}
	}
	
	pub fn name(&self) -> &'static str {
		self.name
	}
	
	pub fn add_field_template(&mut self, field_template: StructFieldDefTemplate) {
		self.fields.insert_assert_not_replace(
			field_template.name.value().to_string(),
			field_template);
	}
	
	pub fn add_user_func_template(&mut self, user_func_template: UserFuncDefTemplate) {
		self.user_funcs.insert_assert_not_replace(
			user_func_template.name.value().to_string(),
			user_func_template);
	}
	
	pub fn generate_type_def(&self, type_params: &HashMap<String, NameToken>, context: &Context) -> StructDef {
		let mut type_name: String = self.name.to_string();
		
		for (key, value) in type_params.iter() {
			type_name.push_str(&format!("__{}_{}", key, value.value()));
		}
		
		let name_nt = NameToken::new_with_pos(
			type_name,
			CodePos::from(CharPos::new()),
			false);
			
		let mut fields = Vec::<StructFieldDef>::new();
		for (f_name, f) in self.fields.iter() {
			let resolved_field_type_name = resolve_type_name(
				type_params, 
				&f.data_type_name,
				context);
			let dt: DataType = context.find_type_by_name(&resolved_field_type_name).unwrap();
			let name_nt = NameToken::new_with_pos(
				f_name.to_string(),
				CodePos::from(CharPos::new()),
				false);
			fields.push(StructFieldDef::new(name_nt, dt));
		}
			
		StructDef::new(name_nt, fields).unwrap()
	}
	
	pub fn generate_user_member_funcs(&self, generated_type_def: &mut StructDef, mut type_params: HashMap<String, NameToken>, context: &Context) {
		type_params.insert_assert_not_replace(
			"Self".to_string(), 
			NameToken::new_with_pos(
				generated_type_def.inner().name().value().to_string(),
				CodePos::from(CharPos::new()),
				false));
		
		for func_template in self.user_funcs.values() {
			let fd: UserFuncDef = func_template.generate(&type_params, context);
			generated_type_def.inner_mut().add_user_func_def(fd);
		}
	}
}

//------------------------- TemplateTypeParam -----------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TemplateTypeParam {
	name: &'static str,
	data_type_name: NameToken,
}

//------------------------- StructFieldDefTemplate -----------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructFieldDefTemplate {
	name: NameToken,
	data_type_name: NameToken,
}

impl StructFieldDefTemplate {
	pub fn new(name: NameToken, data_type_name: NameToken) -> Self {
		Self {
			name,
			data_type_name,
		}
	}
}

//------------------------- UserFuncDefTemplate -----------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct UserFuncDefTemplate {
	name: NameToken,
	args: Vec<ParsedFuncArgDef>,
	return_type_name: Option<NameToken>,
	body: Vec<Statement>,
}

impl UserFuncDefTemplate {
	pub fn new(name: NameToken, args: Vec<ParsedFuncArgDef>, return_type_name: Option<NameToken>, body: Vec<Statement>) -> Self {
		Self {
			name,
			args,
			return_type_name,
			body,
		}
	}
	
	pub fn generate(&self, type_params: &HashMap<String, NameToken>, context: &Context) -> UserFuncDef {
		let return_type: DataType = match self.return_type_name {
			Some(ref tn) => {
				let resolved_return_type_name: NameToken = resolve_type_name(type_params, tn, context);
				context.find_type_by_name(&resolved_return_type_name).unwrap()
			},
			None => DataType::Builtin (BuiltinType::None),
		};
		
		let args: Vec<UserFuncArg> = resolve_user_member_function_args(type_params, &self.args, context);
		
		let mut next_check_context = context.new_stack_frame_context();
		
		for arg_ref in args.iter() {
			next_check_context.add_variable(
				arg_ref.name().clone(), 
				arg_ref.data_type().clone(),
				arg_ref.data_type().default_value()).unwrap();
		}
		
		let func_body = ReturningBody::new(resolve_statements(&self.body, type_params, &mut next_check_context));
		
		let fd = UserFuncDef::new(
			self.name.clone(),
			args,
			return_type.clone(),
			func_body);
		
		fd.body().check(&return_type, &mut next_check_context).unwrap();
		
		fd
	}
}

//------------------------- Resolving funcs -------------------

fn resolve_statements(template_statements: &Vec<Statement>, type_params: &HashMap<String, NameToken>, context: &mut Context) -> Vec<Statement> {
	let mut statements = Vec::<Statement>::new();
	
	for st in template_statements.iter() {
		let st_kind: StatementKind = match st.kind() {
			StatementKind::VariableDeclareSet { var_name, data_type_name, value_expr } => {
				StatementKind::VariableDeclareSet {
					var_name: var_name.clone(), 
					data_type_name: resolve_type_name(type_params, data_type_name, &context), 
					value_expr: value_expr.clone() 
				}
			},
			
			StatementKind::UserStructDeclare { new_type_name, .. } => {
				panic!(" Err( StructDefErr::StructDefNotInRootContext {}", new_type_name);
			},
			
			StatementKind::UserDefinedFuncDeclare { name, args, return_type_name, body } => {
				// let (resolved_return_type_name, return_type): (Option<NameToken>, DataType) = match return_type_name {
					// Some(tn) => {
						// let resolved_tn: NameToken = resolve_type_name(type_params, tn, &context);
						// let return_type: DataType = context.find_type_by_name(&resolved_tn).unwrap();
						// (Some(resolved_tn), return_type)
					// },
					// None => (None, DataType::Builtin (BuiltinType::None)),
				// };
				
				// let rb: ReturningBody = resolve_returning_body(type_params, body.statements(), &return_type, &context);
				
				// StatementKind::UserDefinedFuncDeclare { 
					// name: name.clone(), // Use the same name
					// args: resolve_function_parsed_args(type_params, args, &context), 
					// return_type_name: resolved_return_type_name, 
					// body: rb,
				// }
				todo!();
			},
			
			
			StatementKind::VariableSet { left_expr, right_expr } => 
				StatementKind::VariableSet {
					left_expr: resolve_expr(type_params, left_expr, &context),
					right_expr: resolve_expr(type_params, right_expr, &context),
				},
			StatementKind::FuncCall { left_expr } => 
				StatementKind::FuncCall {
					left_expr: resolve_expr(type_params, left_expr, &context),
				},
			
			
			StatementKind::UserDefinedFuncReturn { return_expr } => match return_expr {
				Some(expr) => StatementKind::UserDefinedFuncReturn {
					return_expr: Some(resolve_expr(type_params, expr, &context)),
				},
				None => StatementKind::UserDefinedFuncReturn {
					return_expr: None,
				},
			},
				
			StatementKind::BranchingIfElse { if_bodies, else_body } => {
				let mut resolved_if_bodies = Vec::<ConditionalBody>::new();					
				for b in if_bodies {
					let rb: ConditionalBody = resolve_conditional_body(type_params, b, &context);
					resolved_if_bodies.push(rb);
				}
				
				let resolved_else_body: UnconditionalBody = resolve_unconditional_body(type_params, else_body, &context);
				
				StatementKind::BranchingIfElse {
					if_bodies: resolved_if_bodies,
					else_body: resolved_else_body,
				}
			},
			
			StatementKind::BranchingWhile { body } => {
				StatementKind::BranchingWhile {
					body: resolve_conditional_body(type_params, body, &context),
				}
			},
		};
		
		let statement = Statement::new(st.pos(), st_kind);
		
		statement.check(context).unwrap();
		
		statements.push(statement);
	}
	
	statements
}

fn resolve_function_parsed_args(type_params: &HashMap<String, NameToken>, template_args: &Vec<ParsedFuncArgDef>, context: &Context) -> Vec<ParsedFuncArgDef> {
	let mut args = Vec::<ParsedFuncArgDef>::new();
	
	for temp_arg in template_args.iter() {
		let arg_type_name: NameToken = resolve_type_name(type_params, &temp_arg.data_type_name(), context);
		let arg = ParsedFuncArgDef::new(temp_arg.name().clone(), arg_type_name);
		args.push(arg);
	}
	
	args
}

fn resolve_user_member_function_args(type_params: &HashMap<String, NameToken>, template_args: &Vec<ParsedFuncArgDef>, context: &Context) -> Vec<UserFuncArg> {
	let mut args = Vec::<UserFuncArg>::new();
	
	for temp_arg in template_args.iter() {
		let arg_type_name: NameToken = resolve_type_name(type_params, temp_arg.data_type_name(), context);
		let arg_data_type: DataType = context.find_type_by_name(&arg_type_name).unwrap();
		let arg = UserFuncArg::new(temp_arg.name().clone(), arg_data_type);
		args.push(arg);
	}
	
	args
}

fn resolve_expr(type_params: &HashMap<String, NameToken>, expr: &Expr, context: &Context) -> Expr {
	let mut resolved_expr_stack = Vec::<Symbol>::new();
	
	for sym in expr.expr_stack() {
		let resolved_sym: Symbol = match sym.kind() {
			SymbolKind::Operand (opnd) => match opnd {
				Operand::Constant (_) => sym.clone(),
				Operand::Variable (_) => sym.clone(),
				Operand::FuncCall { func_name, arg_exprs } => {
					let mut resolved_arg_exprs = Vec::<Expr>::new();
					for arg_expr in arg_exprs {
						let re: Expr = resolve_expr(type_params, arg_expr, context);
						resolved_arg_exprs.push(re);
					}
					Symbol {
						pos: sym.pos(),
						kind: SymbolKind::Operand (Operand::FuncCall {
							func_name: func_name.clone(), 
							arg_exprs: resolved_arg_exprs,
						}),
					}
				},
				Operand::StructLiteral { data_type_name, fields } => {
					let mut resolved_fields = Vec::<StructLiteralField>::new();
					for f in fields {
						let rf = StructLiteralField::new(
							f.field_name().clone(),
							resolve_expr(type_params, f.value_expr(), context));
						resolved_fields.push(rf);
					}
					
					Symbol {
						pos: sym.pos(),
						kind: SymbolKind::Operand (Operand::StructLiteral {
							data_type_name: resolve_type_name(type_params, data_type_name, context),
							fields: resolved_fields,
						}),
					}
				},
				Operand::ArrayLiteral { elements_exprs } => {
					let mut resolved_elements_exprs = Vec::<Expr>::new();
					for expr in elements_exprs {
						let re: Expr = resolve_expr(type_params, expr, context);
						resolved_elements_exprs.push(re);
					}
					Symbol {
						pos: sym.pos(),
						kind: SymbolKind::Operand (Operand::ArrayLiteral {
							elements_exprs: resolved_elements_exprs,
						}),
					}
				},
				Operand::ValueRef (_) => unreachable!(),
				Operand::StringCharRefByInd { .. } => unreachable!(),
				Operand::ArrayElementRefByInd { .. } => unreachable!(),
				Operand::IndexExpr (expr) => Symbol {
					pos: sym.pos(),
					kind: SymbolKind::Operand (Operand::IndexExpr (
						resolve_expr(type_params, expr, context),
					)),
				},
			},
			SymbolKind::LeftRoundBracket => unreachable!(),
			SymbolKind::RightRoundBracket => unreachable!(),
			SymbolKind::ExprOperator (_) => sym.clone(),
		};
		resolved_expr_stack.push(resolved_sym);
	}
	
	Expr::new_from_stack(resolved_expr_stack)
}

fn resolve_conditional_body(type_params: &HashMap<String, NameToken>, cb: &ConditionalBody, context: &Context) -> ConditionalBody {
	let rce: Expr = resolve_expr(type_params, cb.condition_expr(), context);
	let mut next_context = context.new_stack_frame_context();
	let rsts: Vec<Statement> = resolve_statements(cb.statements(), type_params, &mut next_context);
	ConditionalBody::new(rce, rsts)
}	

fn resolve_unconditional_body(type_params: &HashMap<String, NameToken>, ucb: &UnconditionalBody, context: &Context) -> UnconditionalBody {
	let mut next_context = context.new_stack_frame_context();
	let rsts: Vec<Statement> = resolve_statements(ucb.statements(), type_params, &mut next_context);
	UnconditionalBody::new(rsts)
}

fn resolve_type_name(type_params: &HashMap<String, NameToken>, type_name: &NameToken, context: &Context) -> NameToken {
	if let Ok(_) = context.find_type_by_name(type_name) {
		type_name.clone()
	} else if let Some(resolved_type_name) = type_params.get(type_name.value()) {			
		resolved_type_name.clone()
	} else {
		panic!("TemplateErr::NotSatisfiedTypeParam '{}'", type_name);
	}
}


//------------------------- TemplateErr -----------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TemplateErr {
	NotSatisfiedTypeParam { name: NameToken },
}

impl std::fmt::Display for TemplateErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			TemplateErr::NotSatisfiedTypeParam { ref name } => 
				write!(f, "Template type parameter '{}' is not satisfied", name.value()),
		}
	}
}
