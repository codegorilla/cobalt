package org.cobalt

class Parser {
  
  var input: Lexer = null
  var lookahead: Token = null

  def setInput (input: Lexer) =
    this.input = input
    lookahead = input.getToken()

  def matchp (kind: Token.Kind) =
    if lookahead.kind == kind then
      consume()
    else
      print(s"invalid token: expected ${kind}, got ${lookahead.kind}")

  def consume () =
    lookahead = input.getToken()


}
