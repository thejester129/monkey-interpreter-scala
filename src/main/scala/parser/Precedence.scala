package parser

import token.TokenType

enum Precedence() {
  case LOWEST
  case EQUALS // ==
  case LESSGREATER // > or <
  case SUM // +
  case PRODUCT // *
  case PREFIX // -X or !X
  case CALL // myFunction(X)
}

def getPrecedence(tokenType: TokenType): Precedence = {
  return tokenType match {
    case TokenType.EQ       => Precedence.EQUALS;
    case TokenType.NOT_EQ   => Precedence.EQUALS;
    case TokenType.LT       => Precedence.LESSGREATER;
    case TokenType.GT       => Precedence.LESSGREATER;
    case TokenType.PLUS     => Precedence.SUM;
    case TokenType.MINUS    => Precedence.SUM;
    case TokenType.SLASH    => Precedence.PRODUCT;
    case TokenType.ASTERISK => Precedence.PRODUCT;
    case _                  => Precedence.LOWEST
  }
}
