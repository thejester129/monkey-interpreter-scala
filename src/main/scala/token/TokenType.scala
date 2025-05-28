package token

enum TokenType(val displayName: String):
  case ILLEGAL extends TokenType("ILLEGAL")
  case EOF extends TokenType("EOF")
  // Identifiers + literals
  case IDENT extends TokenType("IDENT")
  case INT extends TokenType("INT")
  // Operators
  case ASSIGN extends TokenType("=")
  case PLUS extends TokenType("+")
  // Delimiters
  case COMMA extends TokenType(",")
  case SEMICOLON extends TokenType(";")
  case LPAREN extends TokenType("(")
  case RPAREN extends TokenType(")")
  case LBRACE extends TokenType("{")
  case RBRACE extends TokenType("}")
  // Keywords
  case FUNCTION extends TokenType("FUNCTION")
  case LET extends TokenType("LET")
