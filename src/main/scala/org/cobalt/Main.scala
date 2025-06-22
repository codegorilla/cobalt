package org.cobalt

@main def hello(): Unit = {
  println("Hello world!")
  println(msg)
  var lexer = new Lexer()
  lexer.error("Invalid cast.")
  lexer.setInput("var")
  var t = lexer.getToken()
  print(t.kind)
}

def msg = "I was compiled by Scala 3. :)"
