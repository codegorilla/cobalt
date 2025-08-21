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

// I might want to refactor this so that it gets all of the content first, then
// assembles it into a module.

class ModuleLoader {

  var directory: String = null

  def setDirectory (directory: String) =
    this.directory = directory

  def process (): AstNode =
    val n = AstNode(AstNode.Kind.MODULE)
    // Load the contents of each file in the module. Each file produces an AST,
    // which will be kept in a list to be assembled later.
    val modulePath = Paths.get(directory)
    try {
      val filePaths = Files.list(modulePath).iterator().asScala
      for filePath <- filePaths do
        println(filePath.getFileName)
        val unitLoader = UnitLoader(filePath)
        val p = unitLoader.process()
        for child <- p.getChildren() do
          n.addChild(child)
    } catch {
      case e: Exception => println(s"Error listing files: ${e.getMessage}")
    }
    return n
}
