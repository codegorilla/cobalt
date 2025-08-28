package org.cobalt

// The translator is responsible for carrying out a complete end-to-end
// translation of a cobalt package to a C++ module.

// Actually, we need to build an AST for each translation unit. These ASTs must
// then be combined to form the full package AST, which is then transpiled into
// C++.

class Translator {

  def process () =

    // Each source file in the package directory needs to be processed to form
    // an AST. These ASTs are then all combined to form a single AST.
    // Get the current working directory or 'program' subdirectory in classpath
    // val packageDir = System.getProperty("user.dir")
    // Get the 'program' subdirectory in classpath
    val packageDir = this.getClass().getClassLoader().getResource("program").getPath()
    val package1 = new Package(packageDir)
    val root = package1.load()

    // val root = package1.getUnits()

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
