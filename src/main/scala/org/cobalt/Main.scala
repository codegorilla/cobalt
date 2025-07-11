package org.cobalt

import org.stringtemplate.v4._

import freemarker.template._

import scala.collection.mutable.ListBuffer

import java.util.LinkedList
import java.nio.file.Paths
import java.nio.file.Files
import java.io.IOException

@main def hello () =

  // val cfg = Configuration(Configuration.VERSION_2_3_34)

  // Get template directory
  val templateDir = this.getClass().getClassLoader().getResource("templates")

  // Test StringTemplate
  val group = new STGroupDir(templateDir);
  val st = group.getInstanceOf("decl");
  st.add("type", "int");
  st.add("name", "x");
  st.add("value", 0);
  val result = st.render(); // yields "int x = 0;"
  println(result)

  // Get program directory
  val programDir = this.getClass().getClassLoader().getResource("")

  // For now just hard-code the path
  val path = Paths.get(programDir.getPath() + "/hello.co")
  var content: String = null
  try {
    content = Files.readString(path)
    println(content)
  } catch {
    case e: IOException => e.printStackTrace()
  }

  val lexer = Lexer()
  lexer.setInput(content)
  val tokens = lexer.process()

  println(tokens)

  // Exit
  //System.exit(0)

  val parser1 = Parser1()
  parser1.setInput(tokens)
  val symbolTable = parser1.process()

  println(symbolTable.data)

  val parser2 = Parser2()
  parser2.setInput(tokens)
  val root = parser2.process()

  println(root)

  val pass1a = Pass1a()
  pass1a.setInput(root)
  pass1a.process()

