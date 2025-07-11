package org.cobalt

import org.stringtemplate.v4.*

import scala.collection.mutable.ListBuffer

import java.net.URL
import java.util.LinkedList

// The purpose of this pass is to experiment with StringTemplate

class Pass2 {

  var input: AstNode = null

  var templateDir: URL = null
  var groupDir: STGroupDir = null

  var counter: Int = 0

  var symbolTable: SymbolTable = null

  var stack = LinkedList[AstNode]()

  var enumNameAttributes = Map[AstNode, AstNode]()
  var enumConstNameAttributes = Map[AstNode, LinkedList[AstNode]]()

  def setInput (input: AstNode) =
    this.input = input

  def process () =
    templateDir = this.getClass().getClassLoader().getResource("templates")
    groupDir = STGroupDir(templateDir);
    translationUnit(input)

  def translationUnit (current: AstNode) =
    for child <- current.getChildren() do
      if child.getKind() == AstNode.Kind.VARIABLE_DECLARATION then
        variableDeclaration(child)
  
  def variableDeclaration (current: AstNode) =
    // Child 0 is modifiers
    val st = name(current.getChild(1))
    println(st.render())
    // typeRoot(current.getChild(1))

  def name (current: AstNode): ST =
    val st = groupDir.getInstanceOf("name")
    st.add("name", current.getToken().lexeme)
    return st
}
