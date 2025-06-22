package org.cobalt

@main def hello(): Unit = {
  println("Hello world!")
  println(msg)
  var lexer = new Lexer()
  lexer.error("Invalid cast.")
  lexer.consume()
  lexer.test()
}

def msg = "I was compiled by Scala 3. :)"
