package lexer

import token.*

class Lexer(val input: String):
  private var position = 0

  private val nullChar: Char = '\u0000'

  private def current: Char =
    if (position >= input.length) nullChar
    else input.charAt(position)

  private def next: Char =
    if (position >= input.length - 1) nullChar
    else input.charAt(position + 1)

  def peekToken(): Token = nextToken(true)

  def nextToken(peek: Boolean = false): Token = {
    val originalPos = position
    skipWhitespace()

    val (tokenType, literal) = current match
      // operators
      case '=' => singleOrDoubleToken(TokenType.ASSIGN, TokenType.EQ, '=')
      case '!' => singleOrDoubleToken(TokenType.BANG, TokenType.NOT_EQ, '=')
      case '+' => (TokenType.PLUS, current)
      case '-' => (TokenType.MINUS, current)
      case '/' => (TokenType.SLASH, current)
      case '<' => (TokenType.LT, current)
      case '>' => (TokenType.GT, current)
      case '*' => (TokenType.ASTERISK, current)
      // delimiters
      case ',' => (TokenType.COMMA, current)
      case ';' => (TokenType.SEMICOLON, current)
      case '(' => (TokenType.LPAREN, current)
      case ')' => (TokenType.RPAREN, current)
      case '{' => (TokenType.LBRACE, current)
      case '}' => (TokenType.RBRACE, current)
      // end of file
      case `nullChar` => (TokenType.EOF, "")
      // keywords, identifiers, literals
      case _ if isLetter(current) => parseIdent()
      case _ if isDigit(current)  => parseInt()
      case _                      => (TokenType.ILLEGAL, current)

    advancePosition()

    if (peek) {
      position = originalPos
    }

    return new Token(
      tokenType,
      literal.toString()
    )
  }

  private def advancePosition() = position += 1

  private def skipWhitespace(): Unit = {
    while (current.isWhitespace) {
      advancePosition()
    }
  }

  private def parseIdent(): (TokenType, String) = {
    val literal = readWhile(isLetter)
    return (Token.lookupIdent(literal), literal)
  }

  private def parseInt(): (TokenType, String) = {
    val literal = readWhile(isDigit)
    return (TokenType.INT, literal)
  }

  private def singleOrDoubleToken(
      single: TokenType,
      double: TokenType,
      doubleSecond: Char
  ): (TokenType, String) = {
    val first = current
    if (next == doubleSecond) {
      advancePosition() // consume the next character
      return (double, first + current.toString())
    }
    return (single, current.toString())
  }

  private def readWhile(fn: (c: Char) => Boolean): String = {
    val start = position
    while (fn(next)) {
      advancePosition()
    }
    input.substring(start, position + 1)
  }

  private def isLetter(ch: Char) =
    ch.isLetter || ch == '_'

  private def isDigit(ch: Char) =
    ch.isDigit

end Lexer
