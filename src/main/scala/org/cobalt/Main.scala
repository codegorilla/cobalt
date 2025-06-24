package org.cobalt

@main def hello () = {
  var lexer = new Lexer()
  lexer.setInput("var x: int = 0;")

  var t = lexer.getToken()
  println(t)
  t = lexer.getToken()
  println(t)
  t = lexer.getToken()
  println(t)
  t = lexer.getToken()
  println(t)
  t = lexer.getToken()
  println(t)
  t = lexer.getToken()
  println(t)
}
