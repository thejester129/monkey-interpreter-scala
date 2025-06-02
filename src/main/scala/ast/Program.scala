package ast

class Program(private var _statements: List[Statement]) extends Node {
  def statements: List[Statement] = _statements

  def addStatement(statement: Statement): Unit = {
    _statements = _statements :+ statement
  }

  def tokenLiteral(): String =
    if (statements.isEmpty) "" else statements.head.tokenLiteral()

  override def toString: String = _statements.map(_.toString).mkString("\n")
}
