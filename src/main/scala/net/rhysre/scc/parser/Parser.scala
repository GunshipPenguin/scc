package net.rhysre.scc.parser

import net.rhysre.scc.parser.nodes._
import net.rhysre.scc.lexer.Lexer._

object Parser {
  private def parseExpression(tokens: List[Token]): AstNode = tokens match {
    case IntToken(value) :: _ => IntValueNode(value)
    case _ => throw new UnsupportedOperationException("Expression type is not implemented")
  }

  private def parseReturn(tokens: List[Token]): ReturnNode =
    ReturnNode(parseExpression(tokens))

  private def parseFunction(tokens: List[Token]): AstNode = tokens match {
    case ReturnKeywordToken() :: tail => parseReturn(tail)
    case _ => throw new RuntimeException("Parse error")
  }

  def parseFunctions(tokens: List[Token]) = tokens match {
    case IntToken() :: tail => parseFunction(tail)
  }
}
