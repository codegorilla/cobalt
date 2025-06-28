package org.cobalt

@main def hello () = {
  val lexer = new Lexer()
  lexer.setInput("var x: int = 0b_001u; class hello { }; var x; class Waypoint { }")
  val tokens = lexer.process()

  println(tokens)

  val parser = new Parser()
  parser.setInput(tokens)
  val symbolTable = parser.process()

  println(symbolTable.data)
}
