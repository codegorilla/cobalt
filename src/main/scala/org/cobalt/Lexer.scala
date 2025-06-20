package org.cobalt

class Lexer {

  // Only supports ASCII for now
  val EOF: Char = 1000

  var input = ""
  var position = 0
  var current = EOF
  var start = 0
  var end = 0
  var line = 1
  var column = 1

  def setInput (input: String) =
    this.input = input
    if (input.length > 0)
      current = input(position)

  def error (message: String) =
    val coords = s"(${line},${column})"
    println(s"${coords}: error: ${message}")

  def consume () =
    position += 1
    current = if position < input.length then input(position) else EOF
    column += 1

}

