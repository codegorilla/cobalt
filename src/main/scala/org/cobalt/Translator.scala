package org.cobalt

import java.nio.file.Paths
import java.nio.file.Files
import java.io.IOException

// The translator is responsible for carrying out a complete end-to-end
// translation of a cobalt module to a C++ module.

// Actually, we need to build an AST for each translation unit. These ASTs must
// then be combined to form the full module AST, which is then transpiled into
// C++.

class Translator {

  private val lexer = Lexer()
  private val parser = Parser()

  def process () =
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
    val root = parser.process()

    val generator1 = Generator1()
    generator1.setInput(root)
    // Todo: The output should be text
    val template1 = generator1.process()

    val code1 = template1.render()
    println("---")
    println(code1)


    // val generator2 = Generator2()
    // generator2.setInput(root)
    // // Todo: The output should be text
    // val template2 = generator2.process()

    // val code2 = template2.render()
    // println("---")
    // println(code2)

    // Test errors
    // val error = ErrorMessage(lookahead.line, lookahead.column)
    // error.setMessage("Internal error in parser/modifiers.")
    // error.print()

}
