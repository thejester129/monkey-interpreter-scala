package token

enum TokenType(val displayName: String):
  // operators
  case ASSIGN extends TokenType("=")
  case EQ extends TokenType("==")
  case NOT_EQ extends TokenType("!=")
  case PLUS extends TokenType("+")
  case MINUS extends TokenType("-")
  case BANG extends TokenType("!")
  case ASTERISK extends TokenType("*")
  case SLASH extends TokenType("/")
  case LT extends TokenType("<")
  case GT extends TokenType(">")
  // delimiters
  case COMMA extends TokenType(",")
  case SEMICOLON extends TokenType(";")
  case LPAREN extends TokenType("(")
  case RPAREN extends TokenType(")")
  case LBRACE extends TokenType("{")
  case RBRACE extends TokenType("}")
  // keywords
  case FUNCTION extends TokenType("FUNCTION")
  case LET extends TokenType("LET")
  case TRUE extends TokenType("TRUE")
  case FALSE extends TokenType("FALSE")
  case IF extends TokenType("IF")
  case ELSE extends TokenType("ELSE")
  case RETURN extends TokenType("RETURN")
// identifiers + literals
  case IDENT extends TokenType("IDENT")
  case INT extends TokenType("INT")
  // special
  case EOF extends TokenType("EOF")
  case ILLEGAL extends TokenType("ILLEGAL")
