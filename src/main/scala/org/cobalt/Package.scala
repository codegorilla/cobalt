package org.cobalt

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters.*

import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files

// The package loader is responsible for identifying all source files in the
// package and instantiating a file loader to process each one. It then
// assembles the AST from each file loader into a list. These ASTs are used to
// create a package interface unit. Each source file forms a package
// implementation unit.

// We need to build a dependency graph. For each unit loaded, the imports needed
// for that unit need to be added to a list of packages that need to be
// processed. However, this is NOT the job of the compiler; it is instead the
// responsibility of a build system.


// In a package implementation unit, any import declarations must occur after
// the package declaration, but before any other declarations. This allows the
// compiler to quickly and efficiently determine all dependencies in a single
// pass.

// It is not clear yet if the compiler should handle this recursively or
// iteratively. Recursion feels more elegant, but if the import dependency graph
// is very deep, then could it run out of memory? Perhaps we should build the
// dependency graph iteratively, or at least built it out first even if using
// recursion.

// The compiler needs to do one of two things: (1) Build a dependency graph and
// report it. (2) Compile this package.

// When it compiles the package, it produces a package interface unit in JSON.
// If a client package needs to be compiled, for each import, the package
// interface unit must exist in order to import it.

// So, we need to be able to product package interface units in JSON format.

class Package (directory: String) {

  // Load the contents of each file in the package. Each file produces an AST,
  // which will be kept in a list to be processed later.

  private val units = ListBuffer[AstNode]()

  // Files must be regular files (not directories or symbolic links) and must
  // have a ".co" file extension.

  def load (): ListBuffer[AstNode] =
    val packagePath = Paths.get(directory)
    val filePaths = Files.list(packagePath).iterator().asScala
    for filePath <- filePaths do
      if Files.isRegularFile(filePath) && filePath.getFileName.toString.endsWith(".co") then
        println(filePath.getFileName)
        val unit = loadFile(filePath)
        units += unit
    return units

  def loadFile (filename: Path): AstNode =
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
