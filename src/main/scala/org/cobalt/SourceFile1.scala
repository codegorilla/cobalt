package org.cobalt

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files
import java.io.IOException

// The unit loader is responsible for loading, tokenizing, and parsing a single
// source file (a.k.a. module implementation unit, MIU, or just unit).

class SourceFile1 (filepath: Path) {

  private var root: AstNode = null

  def getRoot (): AstNode =
    return root

  def load () =
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
    root = parser.process()
}
