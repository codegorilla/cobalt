package org.cobalt

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters.*

import java.nio.file.Paths
import java.nio.file.Files
import java.io.IOException

// The module loader is responsible for identifying all source files in the
// module and instantiating a file loader to process each one. It then assembles
// the AST from each file loader into a complete AST for the entire module.
// Thus, the combination of all source files constitues a "translation unit".

class ModuleLoader {

  val trees = ListBuffer[AstNode]()
  var directory: String = null

  def setDirectory (directory: String) =
    this.directory = directory

  def process (): AstNode =
    // Load the contents of each file in the module. Each file produces an AST,
    // which will be kept in a list to be assembled later.
    val modulePath = Paths.get(directory)
    try {
      val filePaths = Files.list(modulePath).iterator().asScala
      for filePath <- filePaths do
        val fileLoader = FileLoader(filePath)
        trees += fileLoader.process()
        println(filePath.getFileName)
    } catch {
      case e: Exception => println(s"Error listing files: ${e.getMessage}")
    }

    val tree = translationUnit()
    return tree

  // Each tree is a list of declarations, so we need to go one level deeper and
  // pull out the declarations.

  def translationUnit (): AstNode =
    val n = AstNode(AstNode.Kind.TRANSLATION_UNIT)
    for tree <- trees do
      for child <- tree.getChildren() do
      n.addChild(child)
    return n
}
