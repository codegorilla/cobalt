package org.cobalt

import scala.collection.mutable.ListBuffer

import java.util.LinkedList
import java.nio.file.Paths
import java.nio.file.Files
import java.io.IOException

@main def hello () =

  // For now just hard-code the path
  // Eventually, we'll want to provide robust CLI processing
  val mainPath = "C:/Users/clearm/workspace/alpha-sprint4/cobalt/src/main/resources"
  val path = Paths.get(mainPath + "/hello.co")
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

  val pass1 = Pass1()
  pass1.setInput(root)
  pass1.process()

