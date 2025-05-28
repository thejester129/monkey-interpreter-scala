package lexer_tests

import lexer.*
import token.*

class LexerTests extends munit.FunSuite {
  test("test next token (delimiters)") {
    val input = "=+(){},;"
    val expected =
      List(
        TokenType.ASSIGN,
        TokenType.PLUS,
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
      assertEquals(ex, lexer.nextToken().tokenType)
    }
  }

  test("test skip whitespace") {
    val input = " = + "
    val expected =
      List(
        TokenType.ASSIGN,
        TokenType.PLUS
      )

    val lexer = new Lexer(input)
    for (ex <- expected) {
      assertEquals(ex, lexer.nextToken().tokenType)
    }
  }

  test("test next token (all)") {
    val input = """ let five = 5;
                    let ten = 10;
                    let add = fn(x, y) {
                    x + y;
                    };
                    let result = add(five, ten);
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
        TokenType.EOF
      )

    val lexer = new Lexer(input)
    var position = 0
    for (ex <- expected) {
      assertEquals(ex, lexer.nextToken().tokenType, s"Position $position")
      position += 1
    }
  }
}
