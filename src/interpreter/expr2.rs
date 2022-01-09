use super::tokens_iter::*;
use super::InterpErr;

pub struct Expr {
	root_node: Node,
}

struct Node {
	rank: u32,
	content: NodeContent,
}

enum NodeContent {
	UnaryOperator {
		lhs: Box<Node>,
		op: 
		rhs: Box<Node>,
}