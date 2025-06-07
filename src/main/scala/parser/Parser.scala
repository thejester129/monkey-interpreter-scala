package parser

import lexer.Lexer
import ast.Program
import token.Token
import ast.Statement
import ast.LetStatement
import token.TokenType
import ast.IdentifierExpression
import scala.util.Try
import scala.util.Success
import ast.ReturnStatement
import ast.ExpressionStatement
import ast.Expression
import ast.IntegerLiteral
import ast.PrefixExpression
import ast.InfixExpression

class Parser(lexer: Lexer) {
  private var currentToken: Token = null
  private def nextToken(): Token = lexer.nextToken()
  private def peekNext(): Token = lexer.peekToken()

  private def supportedPrefixTokens = List(
    TokenType.BANG,
    TokenType.MINUS
  )

  private def supportedInfixTokens = List(
    TokenType.PLUS,
    TokenType.MINUS,
    TokenType.SLASH,
    TokenType.ASTERISK,
    TokenType.EQ,
    TokenType.NOT_EQ,
    TokenType.LT,
    TokenType.GT
  )

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
      case TokenType.RETURN =>
        parseReturnStatement()
      case _ =>
        parseExpressionStatement()
    }
  }

  private def parseLetStatement(): LetStatement = {
    val identifier = nextToken()

    if (identifier.tokenType != TokenType.IDENT) {
      throw new Exception(s"Expected IDENT, got ${identifier.tokenType}")
    }

    val name = IdentifierExpression(identifier.literal)

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

  private def parseReturnStatement(): ReturnStatement = {
    var next = nextToken()

    while next.tokenType != TokenType.SEMICOLON
    do
      if (next.tokenType == TokenType.EOF) {
        throw new Exception("Unexpected EOF while parsing let statement")
      }
      next = nextToken()

    return ReturnStatement(null)
  }

  private def parseExpressionStatement(): ExpressionStatement = {
    var first = currentToken.copy()

    var expression =
      ExpressionStatement(first, parseExpression(Precedence.LOWEST))

    // optional ; at end of expressions
    if (peekNext().tokenType == TokenType.SEMICOLON) {
      nextToken()
    }

    return expression
  }

  private def parseExpression(precedence: Precedence): Expression = {
    val leftExp = currentToken.tokenType match {
      case TokenType.IDENT =>
        IdentifierExpression(currentToken.literal)
      case TokenType.INT =>
        IntegerLiteral(currentToken.literal.toInt)
      case t if supportedPrefixTokens.contains(t) => parsePrefixExpression()
      case _ => throw new Exception("no prefix parse fn")
    }

    var exp = leftExp

    while peekNext().tokenType != TokenType.SEMICOLON && precedence.ordinal < peekPrecedence().ordinal
    do
      var supportsInfixFn = supportedInfixTokens.contains(peekNext().tokenType)
      if (!supportsInfixFn) {
        return leftExp
      }

      currentToken = nextToken()

      exp = parseInfixExpression(exp)

    return exp
  }

  private def parsePrefixExpression(): PrefixExpression = {
    val operatorToken = currentToken.copy()

    currentToken = nextToken()

    val operand = parseExpression(Precedence.PREFIX)

    return PrefixExpression(operatorToken, operatorToken.literal, operand)
  }

  private def parseInfixExpression(left: Expression): InfixExpression = {
    val operatorToken = currentToken.copy()

    val precedence = currentPrecedence()

    currentToken = nextToken()

    val right = parseExpression(precedence)

    return InfixExpression(operatorToken, left, operatorToken.literal, right)
  }

  private def currentPrecedence(): Precedence = {
    return getPrecedence(currentToken.tokenType)
  }

  private def peekPrecedence(): Precedence = {
    return getPrecedence(peekNext().tokenType)
  }

}
