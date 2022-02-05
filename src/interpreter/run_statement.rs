pub struct RunStatement {
	VariableDeclare {
		var_name: NameToken,
		data_type: DataType
	},
	VariableSet {
		var_name: NameToken,
		value_expr: ParsedExpr
	},
}