package ast

import token.Token
import token.TokenType

trait Node {
  def tokenLiteral(): String
}

// doesnt produce a value
trait Statement extends Node {
  def token: Token;
  def tokenLiteral(): String = token.literal
}

// produces a value
trait Expression extends Node {}

case class LetStatement(name: Identifier, value: Expression) extends Statement {
  def token = Token(TokenType.LET, "let")
}

case class Identifier(value: String) {
  def token = Token(TokenType.IDENT, value)
}

class Program(private var _statements: List[Statement]) extends Node {
  def statements: List[Statement] = _statements

  def addStatement(statement: Statement): Unit = {
    _statements = _statements :+ statement
  }

  def tokenLiteral(): String =
    if (statements.isEmpty) "" else statements.head.tokenLiteral()

  override def toString: String = _statements.map(_.toString).mkString("\n")
}
