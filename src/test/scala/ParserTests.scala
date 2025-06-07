package tests

import lexer.*
import token.*
import parser.Parser
import ast.LetStatement
import org.junit.internal.runners.statements.Fail
import scala.util.Failure
import scala.util.Success
import ast.Program
import ast.ReturnStatement
import ast.ExpressionStatement
import ast.IdentifierExpression
import ast.IntegerLiteral
import ast.PrefixExpression
import ast.InfixExpression

class ParserTests extends munit.FunSuite {
  test("let statements") {
    val input = """
        let x = 5;
        let y = 10;
        let foobar = 838383;
        """

    val lexer = new Lexer(input)
    val parser = new Parser(lexer)
    val output = parser.parseProgram()
    val program: Program = output match {
      case Success(p) => p
      case Failure(err) =>
        fail(s"Parsing failed with error: ${err.getMessage()}")
    }

    val actual = program.statements

    val expectedIdentifiers = List("x", "y", "foobar")
    val expectedValues = List("5", "10", "838383")

    assertEquals(actual.size, 3)

    for (i <- 0 until actual.size) {
      val stmt = actual(i)
      assertEquals(stmt.tokenLiteral(), "let")
      assert(stmt.isInstanceOf[LetStatement])

      val letStmt = actual(i).asInstanceOf[LetStatement]
      assertEquals(letStmt.name.value, expectedIdentifiers(i))
      //   assertEquals(letStmt.value.tokenLiteral(), expectedValues(i))
    }
  }

  test("let statement error") {
    val input = """
        let x 5;
        """

    val lexer = new Lexer(input)
    val parser = new Parser(lexer)
    val output = parser.parseProgram()
    val error: Throwable = output match {
      case Failure(err) => err
      case Success(p) =>
        fail(s"Expected error")
    }

    assertEquals(error.getMessage, "Expected ASSIGN, got INT")
  }

  test("return statements") {
    val input = """
        return 5;
        return 10;
        return 993322;
        """

    val lexer = new Lexer(input)
    val parser = new Parser(lexer)
    val output = parser.parseProgram()
    val program: Program = output match {
      case Success(p) => p
      case Failure(err) =>
        fail(s"Parsing failed with error: ${err.getMessage()}")
    }

    val actual = program.statements

    val expectedReturnValues = List("5", "10", "993322")

    assertEquals(actual.size, 3)

    for (i <- 0 until actual.size) {
      val stmt = actual(i)
      assertEquals(stmt.tokenLiteral(), "return")
      assert(stmt.isInstanceOf[ReturnStatement])

      val letStmt = actual(i).asInstanceOf[ReturnStatement]
      // assertEquals(letStmt.name.value, expectedReturnValues(i))
    }
  }

  test("identifier expression") {
    val input = "foobar;";

    val lexer = new Lexer(input)
    val parser = new Parser(lexer)
    val output = parser.parseProgram()
    val program: Program = output match {
      case Success(p) => p
      case Failure(err) =>
        fail(s"Parsing failed with error: ${err.getMessage()}")
    }

    val actual = program.statements

    assertEquals(actual.size, 1)

    val stmt = actual(0)
    assertEquals(stmt.tokenLiteral(), "foobar")
    assert(stmt.isInstanceOf[ExpressionStatement])

    val expressionStatement = actual(0).asInstanceOf[ExpressionStatement]
    val identExpression =
      expressionStatement.expression.asInstanceOf[IdentifierExpression]

    assertEquals(identExpression.value, "foobar")

  }

  test("literal expression") {
    val input = "5;";

    val lexer = new Lexer(input)
    val parser = new Parser(lexer)
    val output = parser.parseProgram()
    val program: Program = output match {
      case Success(p) => p
      case Failure(err) =>
        fail(s"Parsing failed with error: ${err.getMessage()}")
    }

    val actual = program.statements

    assertEquals(actual.size, 1)

    val stmt = actual(0)
    assertEquals(stmt.tokenLiteral(), "5")
    assert(stmt.isInstanceOf[ExpressionStatement])

    val expressionStatement = actual(0).asInstanceOf[ExpressionStatement]
    val identExpression =
      expressionStatement.expression.asInstanceOf[IntegerLiteral]

    assertEquals(identExpression.value, 5)
  }

  test("prefix operators") {
    val input = """
        !5; 
        -15;
        """

    val lexer = new Lexer(input)
    val parser = new Parser(lexer)
    val output = parser.parseProgram()
    val program: Program = output match {
      case Success(p) => p
      case Failure(err) =>
        fail(s"Parsing failed with error: ${err.getMessage()}")
    }

    val actual = program.statements

    val expectedOperators = List("!", "-")
    val expectedValues = List(5, 15)

    assertEquals(actual.size, 2)

    for (i <- 0 until actual.size) {
      val stmt = actual(i)
      assert(stmt.isInstanceOf[ExpressionStatement])
      val expressionStmt = stmt.asInstanceOf[ExpressionStatement]
      val expression = expressionStmt.expression

      val prefixExp = expression.asInstanceOf[PrefixExpression]
      assertEquals(prefixExp.operator, expectedOperators(i))
      val operand = prefixExp.operand.asInstanceOf[IntegerLiteral]
      assertEquals(operand.value, expectedValues(i))
    }
  }

  test("infix operators") {
    val input = """
        1 + 2;
        1 - 2;
        1 * 2;
        1 / 2;
        1 > 2;
        1 < 2;
        1 == 2;
        1 != 2;
        """
    val expectedOperators = List("+", "-", "*", "/", ">", "<", "==", "!=")

    val lexer = new Lexer(input)
    val parser = new Parser(lexer)
    val output = parser.parseProgram()
    val program: Program = output match {
      case Success(p) => p
      case Failure(err) =>
        fail(s"Parsing failed with error: ${err.getMessage()}")
    }

    val actual = program.statements

    assertEquals(actual.size, 8)

    for (i <- 0 until actual.size) {
      val stmt = actual(i)
      assert(stmt.isInstanceOf[ExpressionStatement])
      val expressionStmt = stmt.asInstanceOf[ExpressionStatement]
      val expression = expressionStmt.expression

      val prefixExp = expression.asInstanceOf[InfixExpression]
      assertEquals(prefixExp.operator, expectedOperators(i))
      val left = prefixExp.left.asInstanceOf[IntegerLiteral]
      val right = prefixExp.right.asInstanceOf[IntegerLiteral]
      assertEquals(left.value, 1)
      assertEquals(right.value, 2)
    }
  }

  test("infix parsing precedence") {
    val cases = List(
      ("-a * b", "((-a) * b)"),
      ("!-a", "(!(-a))"),
      ("a * b + c", "((a * b) + c)"),
      ("a + b + c", "((a + b) + c)"),
      ("a + b - c", "((a + b) - c)"),
      ("a * b * c", "((a * b) * c)"),
      ("a * b / c", "((a * b) / c)"),
      ("a + b / c", "(a + (b / c))"),
      ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
      ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
      ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
      ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))")
    )

    for (testCase <- cases) {
      val input = testCase(0)
      val expected = testCase(1)

      val lexer = new Lexer(input)
      val parser = new Parser(lexer)
      val output = parser.parseProgram()
      val program: Program = output match {
        case Success(p) => p
        case Failure(err) =>
          fail(s"Parsing failed with error: ${err.getMessage()}")
      }

      val actual = program.statements
      assertEquals(actual.size, 1, input)

      val expression = program.statements(0)
      assertEquals(expression.toString(), expected, input)
    }

  }

}
