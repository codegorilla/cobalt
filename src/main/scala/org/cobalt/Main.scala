package org.cobalt

@main def hello(): Unit = {
  println("Hello world!")
  println(msg)
  var lexer = new Lexer()
  lexer.doSomething()
  lexer.error("Invalid cast.")
  lexer.consume()
}

def msg = "I was compiled by Scala 3. :)"
