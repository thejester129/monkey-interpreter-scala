package parser

import lexer.Lexer
import ast.Program
import token.Token
import ast.Statement
import ast.LetStatement
import token.TokenType
import ast.Identifier
import scala.util.Try
import scala.util.Success

class Parser(lexer: Lexer) {
  private var currentToken: Token = null
  private def nextToken() = lexer.nextToken()

  def parseProgram(): Try[Program] = Try {
    val program = new Program(List())

    currentToken = nextToken()

    while currentToken.tokenType != token.TokenType.EOF
    do
      val statement = parseStatement()
      if (statement != null) {
        program.addStatement(statement)
      }
      currentToken = nextToken()

    program
  }

  private def parseStatement(): Statement = {
    currentToken.tokenType match {
      case TokenType.LET =>
        parseLetStatement()
      case _ =>
        throw new Exception(s"Unknown token type: ${currentToken.tokenType}")
    }
  }

  private def parseLetStatement(): LetStatement = {
    val identifier = nextToken()

    if (identifier.tokenType != TokenType.IDENT) {
      throw new Exception(s"Expected IDENT, got ${identifier.tokenType}")
    }

    val name = Identifier(identifier.literal)

    val assign = nextToken()

    if (assign.tokenType != TokenType.ASSIGN) {
      throw new Exception(s"Expected ASSIGN, got ${assign.tokenType}")
    }

    var next = nextToken()

    while next.tokenType != TokenType.SEMICOLON
    do
      if (next.tokenType == TokenType.EOF) {
        throw new Exception("Unexpected EOF while parsing let statement")
      }
      next = nextToken()

    return LetStatement(name, null)
  }
}
