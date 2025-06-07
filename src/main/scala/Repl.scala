import scala.io.StdIn.readLine
import lexer.Lexer
import token.TokenType
import parser.Precedence

@main def repl(): Unit =
  println()
  println("~ Welcome to the Jungle ~")
  println("type 'exit' to quit")
  println()

  while true
  do
    print("$ ")
    val input = readLine()

    if input == "exit" then
      println()
      println("Watch out for that tree!")
      println()
      return

    println()
    val lexer = new Lexer(input)
    var token = lexer.nextToken()
    while token.tokenType != TokenType.EOF
    do
      println(token)
      token = lexer.nextToken()
    println()
