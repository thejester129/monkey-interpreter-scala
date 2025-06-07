package ast

import token.Token
import token.TokenType

trait Node {
  def tokenLiteral(): String
  override def toString(): String = tokenLiteral()
}
