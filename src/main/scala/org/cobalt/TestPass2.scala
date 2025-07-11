package org.cobalt

import org.stringtemplate.v4.*

import scala.collection.mutable.ListBuffer

import java.net.URL
import java.util.LinkedList

// The purpose of this pass is to experiment with StringTemplate

class TestPass2 {

  var input: AstNode = null

  var templateDir: URL = null
  var groupDir: STGroupDir = null

  var templateGroupFile: URL = null
  var groupFile: STGroupFile = null

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

    templateGroupFile = this.getClass().getClassLoader().getResource("template-groups/modifiers.stg")
    groupFile = STGroupFile(templateGroupFile)
    translationUnit(input)

  def translationUnit (current: AstNode) =
    for child <- current.getChildren() do
      if child.getKind() == AstNode.Kind.VARIABLE_DECLARATION then
        variableDeclaration(child)
  
  def variableDeclaration (current: AstNode) =
    // Child 0 is modifiers
    val stm = modifiers(current.getChild(0))
    println(stm.render())
    // val stn = name(current.getChild(1))
    // println(stn.render())
    // typeRoot(current.getChild(1))

  // Modifiers are tricky for two reasons: First, keywords in the target
  // language are not always the same as in the source language; and sometimes
  // the mapping depends on context. For example, the keyword 'final' becomes
  // 'const' for variables, but remains 'final' for classes. Second, the
  // placement of modifiers might differ between source and target language. For
  // example, in class declarations, 'final' shifts from before a class name to
  // after. Also, the visibility modifiers (e.g. 'public', 'private') are lifted
  // out of each individual method and the methods are then grouped under
  // visibility "headings".

  def modifiers (current: AstNode): ST =
    val list = LinkedList[String]()
    list.add("public")
    list.add("static")
    list.add("final")
    list.add("const")
    val st = groupFile.getInstanceOf("modifiers")
    st.add("mods", list)
    return st

  def name (current: AstNode): ST =
    val st = groupDir.getInstanceOf("name")
    st.add("name", current.getToken().lexeme)
    return st
}
