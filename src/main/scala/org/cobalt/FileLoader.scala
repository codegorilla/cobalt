package org.cobalt

import java.nio.file.Paths
import java.nio.file.Files
import java.io.IOException

// The file processor is responsible for loading, tokenizing, and parsing a
// single source file.

class FileLoader {

  private val lexer = Lexer()
  private val parser = Parser()

  def process (): AstNode =
    // Get program directory
    val programDir = this.getClass().getClassLoader().getResource("program")

    // For now just hard-code the path
    val path = Paths.get(programDir.getPath() + "/hello.co")
    var content: String = null
    try
      content = Files.readString(path)
      println(content)
    catch
      case e: IOException => e.printStackTrace()

    lexer.setInput(content)
    val tokens = lexer.process()
    println(tokens)

    parser.setInput(tokens)
    val tree = parser.process()
    return tree

}
