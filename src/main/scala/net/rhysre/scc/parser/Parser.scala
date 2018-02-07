package net.rhysre.scc.parser

import net.rhysre.scc.parser.nodes._
import net.rhysre.scc.lexer.Lexer._

object Parser {
  private def parseExpression(tokens: List[Token]): (AstNode, List[Token]) = tokens match {
    case IntToken(value) :: SemicolonToken() :: tail => (IntValueNode(value), tail)
    case _ => throw new UnsupportedOperationException("Expression type is not implemented")
  }

  private def parseReturn(tokens: List[Token]): (AstNode, List[Token]) = {
    val (retVal, tail) = parseExpression(tokens)
    (ReturnNode(retVal), tail)
  }

  private def parseStatement(tokens: List[Token]): (AstNode, List[Token]) = tokens match {
    case ReturnKeywordToken() :: tail => parseReturn(tail)
    case unsupported => throw new UnsupportedOperationException("Statement is not supported " + unsupported.toString)
  }

  private def parseStatementList(tokens: List[Token]): (List[AstNode], List[Token]) = {
    val (node, tail) = tokens match {
      case CloseBraceToken() :: _ => (NilNode, Nil)
      case _ => parseStatement(tokens)
    }

    if (node.equals(NilNode)) (Nil, tail)
    else (node :: parseStatementList(tail)._1, Nil)
  }

  private def parseFunction(tokens: List[Token]): (FunctionNode, List[Token]) = {
    val (name, statements, rest) = tokens match {
      case IdentifierToken(name) :: OpenParensToken() :: CloseParensToken() :: OpenBraceToken() :: tail => {
        val (statements, _): (List[AstNode], List[Token]) = parseStatementList(tail)
        (name, statements, tail)
      }
      case _ => throw new RuntimeException("Parse error in function header")
    }

    (FunctionNode(name, statements), rest)
  }


  def parseFunctions(tokens: List[Token]): FunctionNode = tokens match {
    case IntKeywordToken() :: tail => parseFunction(tail)._1
    case _ => throw new UnsupportedOperationException("Function return type not implemented")
  }
}
