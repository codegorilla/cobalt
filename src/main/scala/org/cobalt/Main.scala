package org.cobalt

import org.stringtemplate.v4.*

import scala.collection.mutable.ListBuffer

import java.util.LinkedList
import java.nio.file.Paths
import java.nio.file.Files
import java.io.IOException

@main def hello () =

  // Get template directory
  val templateDir = this.getClass().getClassLoader().getResource("templates")

  // Test StringTemplate
  val group = STGroupDir(templateDir)
  val st = group.getInstanceOf("decl")
  st.add("type", "int")
  st.add("name", "x")
  st.add("value", 0)
  val result = st.render()
  println(result)

  val st1 = group.getInstanceOf("enumerationDeclaration");
  st1.add("name", "TokenType")
  st1.add("value", 1)
  val result1 = st1.render()
  println(result1)


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

  
  // Center
  val center = LinkedList[String]()
  center.addLast("int")
  
  // Right
  val right = LinkedList[String]()
  right.addLast("1")
  right.addLast("2")
  right.addLast("3")

  // Combined
  val combined = LinkedList[String]()
  combined.addAll(center)
  combined.addAll(right)

  println(s"The list is ${combined}")


  // Exit
  System.exit(0)

  // val parser1 = Parser1()
  // parser1.setInput(tokens)
  // val symbolTable = parser1.process()

  // println(symbolTable.data)

  val parser = Parser()
  parser.setInput(tokens)
  val root = parser.process()

  println(root)
  
  // System.exit(0)

  // val pass1a = Pass1a()
  // pass1a.setInput(root)
  // pass1a.process()

  // val testPass2 = TestPass2()
  // testPass2.setInput(root)
  // testPass2.process()

  // val pass10 = Pass10(root)
  // val symtab = pass10.process()

  //val pass14 = Pass14(root, symtab)
  //pass14.process()

  // val pass20 = Pass20(root)
  // pass20.process()
