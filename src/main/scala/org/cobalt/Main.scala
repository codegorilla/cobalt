package org.cobalt

import org.stringtemplate.v4._

import freemarker.template._

import scala.collection.mutable.ListBuffer

import java.util.LinkedList
import java.nio.file.Paths
import java.nio.file.Files
import java.io.IOException

@main def hello () =

  val cfg = Configuration(Configuration.VERSION_2_3_34)

  val hello = ST("Hello, <name>!")
  hello.add("name", "World")
  val output = hello.render()
  println(output)

  // For now just hard-code the path
  // Eventually, we'll want to provide robust CLI processing
  // Windows
  // val mainPath = "C:/Users/clearm/workspace/alpha-sprint4/cobalt/src/main/resources"
  // MAC
  val mainPath = "/Users/matthewcleary/workspace/bravo-sprint1/cobalt/src/main/resources"
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

  val pass1a = Pass1a()
  pass1a.setInput(root)
  pass1a.process()

