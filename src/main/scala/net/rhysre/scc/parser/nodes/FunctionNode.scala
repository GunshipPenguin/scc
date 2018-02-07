package net.rhysre.scc.parser.nodes

case class FunctionNode(name: String, statements: List[AstNode]) extends AstNode;
