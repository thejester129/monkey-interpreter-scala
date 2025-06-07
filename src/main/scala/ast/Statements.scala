package ast

import token.Token
import token.TokenType

// STATEMENTS - dont return values
trait Statement extends Node {
  def token: Token;
  def tokenLiteral(): String = token.literal
}

// let x = 5;
case class LetStatement(name: IdentifierExpression, value: Expression)
    extends Statement {
  def token = Token(TokenType.LET, "let")
  override def toString() = s"let $name = $value"
}

// return x;
case class ReturnStatement(returnValue: Expression) extends Statement {
  def token = Token(TokenType.RETURN, "return")
  override def toString(): String = s"return $returnValue"
}

// 5
// x + y
case class ExpressionStatement(
    token: Token, // first token of the expression
    expression: Expression
) extends Statement {
  override def toString(): String = expression.toString()
}
