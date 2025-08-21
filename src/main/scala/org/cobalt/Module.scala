package org.cobalt

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters.*

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files
import java.io.IOException

// The module loader is responsible for identifying all source files in the
// module and instantiating a file loader to process each one. It then assembles
// the AST from each file loader into a complete AST for the entire module.
// Thus, the combination of all source files constitues a "translation unit".

// I might want to refactor this so that it gets all of the content first, then
// assembles it into a module.

// We need to build a dependency graph. For each unit loaded, the imports needed
// for that unit need to be added to a list of modules that need to be
// processed.

class Module (directory: String) {

  val units = ListBuffer[AstNode]()

  // Load the contents of each file in the module. Each file produces an AST,
  // which will be kept in a list to be processed later.
  
  def load () =
    val modulePath = Paths.get(directory)
    val filePaths = Files.list(modulePath).iterator().asScala
    for filePath <- filePaths do
      println(filePath.getFileName)
      val root = loadUnit(filePath)
      units += root

  def loadUnit (filename: Path): AstNode =
    // Read file
    val reader = Reader()
    reader.setInput(filename)
    val content = reader.process()
    // Tokenize content
    val lexer = Lexer()
    lexer.setInput(content)
    val tokens = lexer.process()
    println(tokens)
    // Parse tokens
    val parser = Parser()
    parser.setInput(tokens)
    val root = parser.process()
    return root

}
