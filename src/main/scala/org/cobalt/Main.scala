package org.cobalt

@main def hello () = {
  val lexer = Lexer()
  lexer.setInput("var x: int = 0b_001u;")
  //class hello [T] { }; var x; class Waypoint [T] { }")
  val tokens = lexer.process()

  println(tokens)

  val parser1 = Parser1()
  parser1.setInput(tokens)
  val symbolTable = parser1.process()

  println(symbolTable.data)

  val parser2 = Parser2()
  parser2.setInput(tokens)
  val node = parser2.process()

  println(node)
}
