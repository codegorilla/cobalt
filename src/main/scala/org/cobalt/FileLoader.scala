package org.cobalt

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files
import java.io.IOException

// The file loader is responsible for loading, tokenizing, and parsing a single
// source file.

class FileLoader (filepath: Path) {

  def process (): AstNode =
    var content: String = null
    try
      content = Files.readString(filepath)
    catch
      case e: IOException => e.printStackTrace()

    val lexer = Lexer()
    lexer.setInput(content)
    val tokens = lexer.process()
    println(tokens)

    val parser = Parser()
    parser.setInput(tokens)
    val node = parser.process()
    return node

}
