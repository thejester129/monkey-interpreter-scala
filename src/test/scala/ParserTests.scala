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

}
