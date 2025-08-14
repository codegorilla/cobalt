package org.cobalt

import scala.collection.mutable.Map


import java.util.LinkedList
import java.nio.file.Paths
import java.nio.file.Files
import java.io.IOException

@main def hello () =

  // Get program directory
  val programDir = this.getClass().getClassLoader().getResource("")

  // For now just hard-code the path
  val path = Paths.get(programDir.getPath() + "/hello.co")
  var content: String = null
  try {
    content = Files.readString(path)
    println(content)
  } catch {
    case e: IOException => e.printStackTrace()
  }

  val lexer = Lexer()
  lexer.setInput(content)
  val tokens = lexer.process()

  println(tokens)

  val parser = Parser()
  parser.setInput(tokens)
  val root = parser.process()

  val analyzer = Analyzer()
  analyzer.setInput(root)
  analyzer.process()

  val generator1 = Generator1()
  generator1.setInput(root)
  // Todo: The output should be text
  val template1 = generator1.process()

  val code1 = template1.render()
  println("---")
  println(code1)

  System.exit(0)

  val generator2 = Generator2()
  generator2.setInput(root)
  // Todo: The output should be text
  val template2 = generator2.process()

  // Eventually we need a renderer pass to assemble all of the generated code
  // snippets into the final output.

  // val code1 = template1.render()
  // println("---")
  // println(code1)

  val code2 = template2.render()
  println("---")
  println(code2)


  // Test errors
  // val error = ErrorMessage(lookahead.line, lookahead.column)
  // error.setMessage("Internal error in parser/modifiers.")
  // error.print()
