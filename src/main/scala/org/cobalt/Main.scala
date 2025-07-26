package org.cobalt

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

  // val generator = Generator()
  // generator.setInput(root)
  // // Todo: The output should be text
  // val template = generator.process()

  // val code = template.render()
  // println(code)
