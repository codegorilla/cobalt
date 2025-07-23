package org.cobalt

import org.stringtemplate.v4.*

// The code generator converts the AST into the target language.

class Generator {

  var input: AstNode = null

  // Load template group from template directory
  val templateDir = this.getClass().getClassLoader().getResource("templates")
  val group = STGroupDir(templateDir)
  
  // val st = group.getInstanceOf("decl")
  // st.add("type", "int")
  // st.add("name", "x")
  // st.add("value", 0)
  // val result = st.render()
  // println(result)
  // val st1 = group.getInstanceOf("enumerationDeclaration");
  // st1.add("name", "TokenType")
  // st1.add("value", 1)
  // val result1 = st1.render()
  // println(result1)

  def setInput (input: AstNode) =
    this.input = input

  def process (): ST =
    val template = translationUnit(input)
    return template

  def translationUnit (current: AstNode): ST =
    var t: ST = null
    for child <- current.getChildren() do
      if child.getKind() == AstNode.Kind.VARIABLE_DECLARATION then
        t = variableDeclaration(child)
    return t

  def variableDeclaration (current: AstNode): ST =
    val t = group.getInstanceOf("variableDeclaration")
    modifiers(current.getChild(0))
    val tName = variableName(current.getChild(1))
    val tType = typeSpecifier(current.getChild(2))
    t.add("name", tName)
    t.add("type", tType)
    return t

  def variableName (current: AstNode): ST =
    val t = group.getInstanceOf("variableName")
    t.add("name", current.getToken().lexeme)
    return t

  def typeSpecifier (current: AstNode): ST =
    if current.hasChildren() then
      return typeRoot(current.getChild(0))
    else
      return null

  def typeRoot (current: AstNode): ST =
    // Need to check kind here and dispatch accordingly. For now just use type_.
    val t = type_(current.getChild(0))
    return t

  def type_ (current: AstNode): ST =
    val t = group.getInstanceOf("type")
    t.add("name", current.getToken().lexeme)
    return t

  def modifiers (current: AstNode) =
    println(current)


}
