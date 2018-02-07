package net.rhysre.scc.lexer

object Lexer {
  abstract class Token

  // Character tokens
  case class OpenBraceToken() extends Token
  case class CloseBraceToken() extends Token
  case class OpenParensToken() extends Token
  case class CloseParensToken() extends Token
  case class SemicolonToken() extends Token
  case class SubtractToken() extends Token
  case class ComplementToken() extends Token

  // Reserved word tokens
  case class IntKeywordToken() extends Token
  case class ReturnKeywordToken() extends Token

  // Value tokens
  case class IntToken(value: Integer) extends Token

  // Identifier token
  case class IdentifierToken(value: String) extends Token

  class SyntaxErrorException extends Exception

  def getKeywordToken(s: String) = s match {
    case "return" => Some(ReturnKeywordToken())
    case "int" => Some(IntKeywordToken())
    case _ => None
  }

  def lexValue(stream: Stream[Char]): List[Token] = {
    val token = stream.takeWhile(_.isDigit).mkString
    IntToken(token.toInt) :: lex(stream.drop(token.length))
  }

  def lexKeywordOrIdentifier(stream: Stream[Char]): List[Token] = {
    val token = stream.takeWhile(c => c.isLetter || c.isDigit || c.equals('_')).mkString

    getKeywordToken(token) match {
      case Some(keywordToken) => keywordToken :: lex(stream.drop(token.length))
      case None => IdentifierToken(token) :: lex(stream.drop(token.length))
    }
  }

  def lex(stream: Stream[Char]): List[Token] = {
    stream match {
      case c #:: tail if c.isWhitespace => lex(tail)
      case '(' #:: tail => OpenParensToken() :: lex(tail)
      case ')' #:: tail => CloseParensToken() :: lex(tail)
      case '{' #:: tail => OpenBraceToken() :: lex(tail)
      case '}' #:: tail => CloseBraceToken() :: lex(tail)
      case ';' #:: tail => SemicolonToken() :: lex(tail)
      case '-' #:: tail => SubtractToken() :: lex(tail)
      case '~' #:: tail => ComplementToken() :: lex(tail)
      case c #:: _ if c.isDigit => lexValue(stream)
      case c #:: _ if c.isLetter => lexKeywordOrIdentifier(stream)
      case Stream.Empty => Nil
      case _ => {
        throw new SyntaxErrorException
      }
    }
  }
}
