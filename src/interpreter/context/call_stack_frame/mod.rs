mod scope;

use scope::Scope;
use super::super::var_data::{VarData, VarErr};
use super::super::user_func::{UserFuncDef, UserFuncErr};
use super::super::utils::NameToken;

#[derive(Debug)]
pub struct CallStackFrame {
	local_scopes_stack: Vec<Scope>,
}

impl CallStackFrame {
	pub fn new() -> Self {		
		Self {
			local_scopes_stack: vec![Scope::new()],
		}
	}
		
	pub fn push_scope(&mut self) {
		self.local_scopes_stack.push(Scope::new());
	}
	
	pub fn pop_scope(&mut self) {
		self.local_scopes_stack.pop().unwrap();
	}
	
	pub fn get_upper_scope_mut(&mut self) -> &mut Scope {
		self.local_scopes_stack.last_mut().unwrap()
	}
	
	pub fn find_user_func_def(&self, name: &NameToken) -> Result<&UserFuncDef, UserFuncErr> {
		for scope in self.local_scopes_stack.iter().rev() {
			match scope.find_func_def(name) {
				res @ Ok(_) => return res,
				Err(_) => {},
			}
		}
		Err( UserFuncErr::NotDefined { name: name.clone() } )
	}
	
	pub fn find_var(&self, name: &NameToken) -> Result<&VarData, VarErr> {
		for scope in self.local_scopes_stack.iter().rev() {
			match scope.find_var(name) {
				res @ Ok(_) => return res,
				Err(_) => {},
			}
		}
		Err( VarErr::NotDefined { name: name.clone() } )
	}
	
	pub fn find_var_mut(&mut self, name: &NameToken) -> Result<&mut VarData, VarErr> {
		for scope in self.local_scopes_stack.iter_mut().rev() {
			match scope.find_var_mut(name) {
				res @ Ok(_) => return res,
				Err(_) => {},
			}
		}
		Err( VarErr::NotDefined { name: name.clone() } )
	}
}
