package org.cobalt

// The translator is responsible for carrying out a complete end-to-end
// translation of a cobalt package to a C++ module.

// Actually, we need to build an AST for each translation unit. These ASTs must
// then be combined to form the full package AST, which is then transpiled into
// C++.

import java.io.File
import java.io.IOException

import com.fasterxml.jackson.core.JsonParseException
import com.fasterxml.jackson.databind.JsonMappingException
import com.fasterxml.jackson.databind.ObjectMapper

import org.cobalt.type_.*

class Translator {

  def process () =

    // Each source file in the package directory needs to be processed to form
    // an AST. These ASTs are then all combined to form a single AST.
    // Get the current working directory or 'program' subdirectory in classpath
    // val packageDir = System.getProperty("user.dir")
    // Get the 'program' subdirectory in classpath
    val packageDir = this.getClass().getClassLoader().getResource("program").getPath()
    val package1 = new Package(packageDir)
    val units = package1.load()

    var objectMapper = ObjectMapper()
    var type_ = PrimitiveTypeNode()
    type_.setKind(PrimitiveTypeNode.Kind.INT)
    type_.num = 10;

    var ptype_ = PointerTypeNode()
    ptype_.setBaseType(type_)
    objectMapper.writeValue(new File("bmi.json"), ptype_)


    // Units is a list of package implementation units (each of which happens to
    // be a Cobalt translation unit). We need to generate a package interface
    // unit file from this. The Package interface unit file should contain
    // everything needed to produce a C++ module interface unit file.

    // Generate package interface unit file
    // val generator0 = Generator0()
    // generator0.setInput(units)
    // generator0.process()

    // val generator1 = Generator1()
    // generator1.setInput(root)
    // // Todo: The output should be text
    // val template1 = generator1.process()

    // val code1 = template1.render()
    // println("---")
    // println(code1)


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
