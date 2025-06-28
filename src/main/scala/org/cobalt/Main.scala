package org.cobalt

@main def hello () = {
  val lexer = Lexer()
  lexer.setInput("var x: int = 0b_001u; class hello [T] { }; var x; class Waypoint [T] { }")
  val tokens = lexer.process()

  println(tokens)

  val parser = Parser1()
  parser.setInput(tokens)
  val symbolTable = parser.process()

  println(symbolTable.data)
}
