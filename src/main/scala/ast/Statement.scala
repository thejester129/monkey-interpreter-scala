package ast

import token.Token
import token.TokenType

// doesnt produce a value
trait Statement extends Node {
  def token: Token;
  def tokenLiteral(): String = token.literal
}

case class LetStatement(name: Identifier, value: Expression) extends Statement {
  def token = Token(TokenType.LET, "let")
}

case class ReturnStatement(returnValue: Expression) extends Statement {
  def token = Token(TokenType.RETURN, "return")
}
