package org.cobalt

@main def hello () = {
  val lexer = new Lexer()
  lexer.setInput("var x: int = 0b_001u;")

  val parser = new Parser()
  parser.setInput(lexer)

  // var t = lexer.getToken()
  // println(t)
  // t = lexer.getToken()
  // println(t)
  // t = lexer.getToken()
  // println(t)
  // t = lexer.getToken()
  // println(t)
  // t = lexer.getToken()
  // println(t)
  // t = lexer.getToken()
  // println(t)
}
