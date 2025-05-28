package lexer

import token.*

class Lexer(val input: String):
  private var position = 0

  private val nullChar: Char = '\u0000'
  private val keywords: Map[String, TokenType] =
    Map.apply("fn" -> TokenType.FUNCTION, "let" -> TokenType.LET)

  private def current: Char =
    if (position >= input.length) nullChar
    else input.charAt(position)

  private def next: Char =
    if (position >= input.length - 1) nullChar
    else input.charAt(position + 1)

  def nextToken(): Token = {
    skipWhitespace()

    val (tokenType, literal) = current match
      case '='                    => (TokenType.ASSIGN, current)
      case '+'                    => (TokenType.PLUS, current)
      case ','                    => (TokenType.COMMA, current)
      case ';'                    => (TokenType.SEMICOLON, current)
      case '('                    => (TokenType.LPAREN, current)
      case ')'                    => (TokenType.RPAREN, current)
      case '{'                    => (TokenType.LBRACE, current)
      case '}'                    => (TokenType.RBRACE, current)
      case `nullChar`             => (TokenType.EOF, "")
      case _ if isLetter(current) => parseIdent()
      case _ if isDigit(current)  => parseInt()
      case _                      => (TokenType.ILLEGAL, current)

    advancePosition()

    return new Token(
      tokenType,
      literal.toString()
    )
  }

  private def advancePosition() = position += 1

  private def skipWhitespace(): Unit = {
    while (
      current == ' ' || current == '\t' || current == '\n' || current == '\r'
    ) {
      advancePosition()
    }
  }

  private def parseIdent(): (TokenType, String) = {
    val literal = readWhile(isLetter)
    return (getIdentifierType(literal), literal)
  }

  private def parseInt(): (TokenType, String) = {
    val literal = readWhile(isDigit)
    return (TokenType.INT, literal)
  }

  private def readWhile(fn: (c: Char) => Boolean): String = {
    val start = position
    while (fn(next)) {
      advancePosition()
    }
    input.substring(start, position + 1)
  }

  private def getIdentifierType(identifier: String) =
    keywords.getOrElse(identifier, TokenType.IDENT)

  private def isLetter(ch: Char) =
    ch.isLetter || ch == '_'

  private def isDigit(ch: Char) =
    ch.isDigit

end Lexer
