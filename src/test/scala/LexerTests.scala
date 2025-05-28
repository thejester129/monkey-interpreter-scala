package lexer_tests

import lexer.*
import token.*

class LexerTests extends munit.FunSuite {
  test("delimiters") {
    val input = "(){},;"
    val expected =
      List(
        TokenType.LPAREN,
        TokenType.RPAREN,
        TokenType.LBRACE,
        TokenType.RBRACE,
        TokenType.COMMA,
        TokenType.SEMICOLON,
        TokenType.EOF
      )

    val lexer = new Lexer(input)
    for (ex <- expected) {
      assertEquals(lexer.nextToken().tokenType, ex)
    }
  }

  test("operators") {
    val input = "=+-!*/<>"
    val expected =
      List(
        TokenType.ASSIGN,
        TokenType.PLUS,
        TokenType.MINUS,
        TokenType.BANG,
        TokenType.ASTERISK,
        TokenType.SLASH,
        TokenType.LT,
        TokenType.GT,
        TokenType.EOF
      )

    val lexer = new Lexer(input)
    for (ex <- expected) {
      assertEquals(lexer.nextToken().tokenType, ex)
    }

  }

  test("skip whitespace") {
    val input = " = + "
    val expected =
      List(
        TokenType.ASSIGN,
        TokenType.PLUS
      )

    val lexer = new Lexer(input)
    for (ex <- expected) {
      assertEquals(lexer.nextToken().tokenType, ex)
    }
  }

  test("double operators") {
    val input = "== != = !"
    val expected =
      List(
        TokenType.EQ,
        TokenType.NOT_EQ,
        TokenType.ASSIGN,
        TokenType.BANG
      )

    val lexer = new Lexer(input)
    for (ex <- expected) {
      assertEquals(lexer.nextToken().tokenType, ex)
    }
  }

  test("test next token (all)") {
    val input = """ let five = 5;
                    let ten = 10;
                    let add = fn(x, y) {
                      x + y;
                    };
                    let result = add(five, ten);
                    if (5 < 10) {
                      return true;
                    } else {
                      return false;
                    }
                    """
    val expected =
      List(
        TokenType.LET,
        TokenType.IDENT,
        TokenType.ASSIGN,
        TokenType.INT,
        TokenType.SEMICOLON,
        TokenType.LET,
        TokenType.IDENT,
        TokenType.ASSIGN,
        TokenType.INT,
        TokenType.SEMICOLON,
        TokenType.LET,
        TokenType.IDENT,
        TokenType.ASSIGN,
        TokenType.FUNCTION,
        TokenType.LPAREN,
        TokenType.IDENT,
        TokenType.COMMA,
        TokenType.IDENT,
        TokenType.RPAREN,
        TokenType.LBRACE,
        TokenType.IDENT,
        TokenType.PLUS,
        TokenType.IDENT,
        TokenType.SEMICOLON,
        TokenType.RBRACE,
        TokenType.SEMICOLON,
        TokenType.LET,
        TokenType.IDENT,
        TokenType.ASSIGN,
        TokenType.IDENT,
        TokenType.LPAREN,
        TokenType.IDENT,
        TokenType.COMMA,
        TokenType.IDENT,
        TokenType.RPAREN,
        TokenType.SEMICOLON,
        TokenType.IF,
        TokenType.LPAREN,
        TokenType.INT,
        TokenType.LT,
        TokenType.INT,
        TokenType.RPAREN,
        TokenType.LBRACE,
        TokenType.RETURN,
        TokenType.TRUE,
        TokenType.SEMICOLON,
        TokenType.RBRACE,
        TokenType.ELSE,
        TokenType.LBRACE,
        TokenType.RETURN,
        TokenType.FALSE,
        TokenType.SEMICOLON,
        TokenType.RBRACE,
        TokenType.EOF
      )

    val lexer = new Lexer(input)
    var position = 0
    for (ex <- expected) {
      assertEquals(lexer.nextToken().tokenType, ex, s"Position $position")
      position += 1
    }
  }
}
