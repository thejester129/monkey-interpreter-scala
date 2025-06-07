package ast

import token.Token
import token.TokenType

// EXPRESSIONS - return values

trait Expression extends Node {}

// x
case class IdentifierExpression(value: String) extends Expression {
  def token = Token(TokenType.IDENT, value)
  def tokenLiteral(): String = value
}

// 5
case class IntegerLiteral(value: Int) extends Expression {
  def token = Token(TokenType.INT, value.toString())
  def tokenLiteral(): String = value.toString()
}

// !true
case class PrefixExpression(
    token: Token, // prefix token e.g !
    operator: String, // !
    operand: Expression // true
) extends Expression {
  def tokenLiteral(): String = s"$operator$operand"
  override def toString(): String = s"($operator$operand)"
}

// 5 + 5
case class InfixExpression(
    token: Token, // infix token e.g +
    left: Expression,
    operator: String,
    right: Expression
) extends Expression {
  def tokenLiteral(): String =
    s"($left $operator $right)"
  override def toString(): String =
    s"($left $operator $right)"

}
