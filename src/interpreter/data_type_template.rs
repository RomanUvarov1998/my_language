use super::utils::{NameToken, CodePos, CharPos};
use super::statement::{Statement, StatementKind, ReturningBody, ParsedFuncArgDef, ParsedStructFieldDef, ConditionalBody, UnconditionalBody};
use std::collections::HashMap;
use super::struct_def::{StructDef, StructFieldDef, StructDefErr};
use super::context::Context;
use super::data_type::{DataType, BuiltinType};
use super::user_func::{UserFuncDef, UserFuncArg};
use super::InterpErr;
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
	
	pub fn generate_type_def(&self, type_params: &HashMap<String, String>, context: &Context) -> StructDef {
		let name_nt = NameToken::new_with_pos(
			self.name.to_string(),
			CodePos::from(CharPos::new()),
			true);
			
		let mut fields = Vec::<StructFieldDef>::new();
		for (f_name, f) in self.fields.iter() {
			let dt: DataType = context.find_type_by_name(&f.data_type_name).unwrap();
			let name_nt = NameToken::new_with_pos(
				f_name.to_string(),
				CodePos::from(CharPos::new()),
				true);
			fields.push(StructFieldDef::new(name_nt, dt));
		}
			
		let mut sd = StructDef::new(name_nt, fields).unwrap();
		
		for (f_name, func) in self.user_funcs.iter() {
			let fd: UserFuncDef = func.generate(type_params, context);
			sd.inner_mut().add_user_func_def(fd);
		}
		
		sd
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

//------------------------- UserFuncDefTemplate -----------------------

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct UserFuncDefTemplate {
	name: NameToken,
	args: Vec<ParsedFuncArgDef>,
	return_type_name: NameToken,
	body: Vec<Statement>,
}

impl UserFuncDefTemplate {
	fn generate(&self, type_params: &HashMap<String, String>, context: &Context) -> UserFuncDef {
		let mut args: Vec<UserFuncArg> = Self::resolve_user_member_function_args(type_params, &self.args, context);
		
		let return_type: DataType = context.find_type_by_name(&self.return_type_name).unwrap();
		
		let func_body: ReturningBody = Self::resolve_returning_body(type_params, &self.body, &return_type, context);
		
		UserFuncDef::new(
			self.name.clone(),
			args,
			return_type,
			func_body)
	}
	
	fn resolve_statements(template_statements: &Vec<Statement>, type_params: &HashMap<String, String>, next_context: &mut Context) -> Vec<Statement> {
		let mut statements = Vec::<Statement>::new();
		
		for st in template_statements.iter() {
			let st_kind: StatementKind = match st.kind() {
				StatementKind::VariableDeclareSet { var_name, data_type_name, value_expr } => {
					StatementKind::VariableDeclareSet {
						var_name: var_name.clone(), 
						data_type_name: Self::resolve_type_name(type_params, data_type_name, &next_context), 
						value_expr: value_expr.clone() 
					}
				},
				
				StatementKind::UserStructDeclare { new_type_name, .. } => {
					panic!(" Err( StructDefErr::StructDefNotInRootContext {}", new_type_name);
				},
				
				StatementKind::UserDefinedFuncDeclare { name, args, return_type_name, body } => {
					let (resolved_return_type_name, return_type): (Option<NameToken>, DataType) = match return_type_name {
						Some(tn) => {
							let resolved_tn: NameToken = Self::resolve_type_name(type_params, tn, &next_context);
							let return_type: DataType = next_context.find_type_by_name(&resolved_tn).unwrap();
							(Some(resolved_tn), return_type)
						},
						None => (None, DataType::Builtin (BuiltinType::None)),
					};
					
					let rb: ReturningBody = Self::resolve_returning_body(type_params, body.statements(), &return_type, &next_context);
					
					StatementKind::UserDefinedFuncDeclare { 
						name: name.clone(), // Use the same name
						args: Self::resolve_function_parsed_args(type_params, args, &next_context), 
						return_type_name: resolved_return_type_name, 
						body: rb,
					}
				},
				
				
				StatementKind::VariableSet { left_expr, right_expr } => 
					StatementKind::VariableSet {
						left_expr: Self::resolve_expr(type_params, left_expr, &next_context),
						right_expr: Self::resolve_expr(type_params, left_expr, &next_context),
					},
				StatementKind::FuncCall { left_expr } => 
					StatementKind::FuncCall {
						left_expr: Self::resolve_expr(type_params, left_expr, &next_context),
					},
				
				
				StatementKind::UserDefinedFuncReturn { return_expr } => match return_expr {
					Some(expr) => StatementKind::UserDefinedFuncReturn {
						return_expr: Some(Self::resolve_expr(type_params, expr, &next_context)),
					},
					None => StatementKind::UserDefinedFuncReturn {
						return_expr: None,
					},
				},
					
				StatementKind::BranchingIfElse { if_bodies, else_body } => {
					let mut resolved_if_bodies = Vec::<ConditionalBody>::new();					
					for b in if_bodies {
						let rb: ConditionalBody = Self::resolve_conditional_body(type_params, b, &next_context);
						resolved_if_bodies.push(rb);
					}
					
					let resolved_else_body: UnconditionalBody = Self::resolve_unconditional_body(type_params, else_body, &next_context);
					
					StatementKind::BranchingIfElse {
						if_bodies: resolved_if_bodies,
						else_body: resolved_else_body,
					}
				},
				
				StatementKind::BranchingWhile { body } => {
					StatementKind::BranchingWhile {
						body: Self::resolve_conditional_body(type_params, body, &next_context),
					}
				},
			};
			
			let statement = Statement::new(st.pos(), st_kind);
			
			statement.check(next_context).unwrap();
			
			statements.push(statement);
		}
		
		statements
	}
	
	fn resolve_function_parsed_args(type_params: &HashMap<String, String>, template_args: &Vec<ParsedFuncArgDef>, context: &Context) -> Vec<ParsedFuncArgDef> {
		let mut args = Vec::<ParsedFuncArgDef>::new();
		
		for temp_arg in template_args.iter() {
			let arg_type_name: NameToken = Self::resolve_type_name(type_params, &temp_arg.data_type_name(), context);
			let arg = ParsedFuncArgDef::new(temp_arg.name().clone(), arg_type_name);
			args.push(arg);
		}
		
		args
	}
	
	fn resolve_user_member_function_args(type_params: &HashMap<String, String>, template_args: &Vec<ParsedFuncArgDef>, context: &Context) -> Vec<UserFuncArg> {
		let mut args = Vec::<UserFuncArg>::new();
		
		for temp_arg in template_args.iter() {
			let arg_type_name: NameToken = Self::resolve_type_name(type_params, temp_arg.data_type_name(), context);
			let arg_data_type: DataType = context.find_type_by_name(&arg_type_name).unwrap();
			let arg = UserFuncArg::new(temp_arg.name().clone(), arg_data_type);
			args.push(arg);
		}
		
		args
	}
	
	fn resolve_expr(type_params: &HashMap<String, String>, expr: &Expr, context: &Context) -> Expr {
		let mut resolved_expr_stack = Vec::<Symbol>::new();
		
		for sym in expr.expr_stack() {
			let resolved_sym: Symbol = match sym.kind() {
				SymbolKind::Operand (opnd) => match opnd {
					Operand::Constant (_) => sym.clone(),
					Operand::Variable (_) => sym.clone(),
					Operand::FuncCall { func_name, arg_exprs } => {
						let mut resolved_arg_exprs = Vec::<Expr>::new();
						for arg_expr in arg_exprs {
							let re: Expr = Self::resolve_expr(type_params, arg_expr, context);
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
								Self::resolve_expr(type_params, f.value_expr(), context));
							resolved_fields.push(rf);
						}
						
						Symbol {
							pos: sym.pos(),
							kind: SymbolKind::Operand (Operand::StructLiteral {
								data_type_name: Self::resolve_type_name(type_params, data_type_name, context),
								fields: resolved_fields,
							}),
						}
					},
					Operand::ValueRef (_) => unreachable!(),
					Operand::StringCharRefByInd { .. } => unreachable!(),
					Operand::IndexExpr (expr) => Symbol {
						pos: sym.pos(),
						kind: SymbolKind::Operand (Operand::IndexExpr (
							Self::resolve_expr(type_params, expr, context),
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
	
	fn resolve_conditional_body(type_params: &HashMap<String, String>, cb: &ConditionalBody, context: &Context) -> ConditionalBody {
		let rce: Expr = Self::resolve_expr(type_params, cb.condition_expr(), context);
		let mut next_context = context.new_stack_frame_context();
		let rsts: Vec<Statement> = Self::resolve_statements(cb.statements(), type_params, &mut next_context);
		ConditionalBody::new(rce, rsts)
	}	
	
	fn resolve_unconditional_body(type_params: &HashMap<String, String>, ucb: &UnconditionalBody, context: &Context) -> UnconditionalBody {
		let mut next_context = context.new_stack_frame_context();
		let rsts: Vec<Statement> = Self::resolve_statements(ucb.statements(), type_params, &mut next_context);
		UnconditionalBody::new(rsts)
	}
	
	fn resolve_returning_body(type_params: &HashMap<String, String>, statements: &Vec<Statement>, return_type: &DataType, context: &Context) -> ReturningBody {
		let mut next_context = context.new_stack_frame_context();
		let rsts: Vec<Statement> = Self::resolve_statements(statements, type_params, &mut next_context);
		let ret_body = ReturningBody::new(rsts);		
		ret_body.check(return_type, &mut next_context).unwrap();
		ret_body
	}
	
	fn resolve_type_name(type_params: &HashMap<String, String>, type_name: &NameToken, context: &Context) -> NameToken {
		if let Ok(_) = context.find_type_by_name(type_name) {
			type_name.clone()
		} else if let Some(new_name) = type_params.get(type_name.value()) {
			NameToken::new_with_pos(
				new_name.clone(),
				CodePos::from(CharPos::new()),
				true)
		} else {
			panic!("TemplateErr::NotSatisfiedTypeParam '{}'", type_name);
		}
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
