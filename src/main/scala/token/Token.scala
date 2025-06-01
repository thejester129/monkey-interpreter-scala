package token

case class Token(tokenType: TokenType, literal: String) {
  override def toString: String = s"$tokenType: $literal"
}

object Token {
  private val keywords: Map[String, TokenType] = Map(
    "fn" -> TokenType.FUNCTION,
    "let" -> TokenType.LET,
    "true" -> TokenType.TRUE,
    "false" -> TokenType.FALSE,
    "if" -> TokenType.IF,
    "else" -> TokenType.ELSE,
    "return" -> TokenType.RETURN
  )

  def lookupIdent(ident: String): TokenType =
    keywords.getOrElse(ident, TokenType.IDENT)
}
