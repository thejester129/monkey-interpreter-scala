package ast

import token.Token
import token.TokenType

trait Node {
  def tokenLiteral(): String
}

// produces a value
trait Expression extends Node {}

case class Identifier(value: String) {
  def token = Token(TokenType.IDENT, value)
}
