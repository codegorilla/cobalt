package org.cobalt

import scala.collection.mutable.Map

import java.util.LinkedList
import javax.xml.crypto.Data

// Semantic analyzer - convert enums to classes

// Enumerations are converted to final classes with public static
// final variables of type integer.

// We traverse the AST, gathering nodes needed for the translation
// phase, which comes in the next pass. So far, all we need are leaf
// nodes, so the manipulation is fairly straight-forward because
// there is no chance for cycles or unintentionally grabing an entire
// sub-tree. If the AST manipulation gets more complex, we might have
// to use a different approach, including making copies of things.

class Pass1a {

  var input: AstNode = null
  var counter: Int = 0

  var symbolTable: SymbolTable = null

  var stack = LinkedList[AstNode]()

  var enumNameAttributes = Map[AstNode, AstNode]()
  var enumConstNameAttributes = Map[AstNode, LinkedList[AstNode]]()

  def setInput (input: AstNode) =
    this.input = input

  def setSymbolTable (symbolTable: SymbolTable) =
    this.symbolTable = symbolTable

  def process () =
    translationUnit(input)

  def translationUnit (current: AstNode) =
    for child <- current.getChildren() do
      if child.getKind() == AstNode.Kind.ENUMERATION_DECLARATION then
        enumerationDeclaration(child)

  def enumerationDeclaration (current: AstNode) =
    name(current.getChild(0))
    enumNameAttributes += (current -> stack.pop())
    enumerationBody(current.getChild(1))
    enumConstNameAttributes += (current -> stack)

  def enumerationBody (current: AstNode) =
    for child <- current.getChildren() do
      enumerationMember(child)

  def enumerationMember (current: AstNode) =
    enumerationConstantDeclaration(current)

  def enumerationConstantDeclaration (current: AstNode) =
    name(current.getChild(0))

  def name (current: AstNode) =
    stack.push(current)



  def classDeclaration (current: AstNode): AstNode =
    val n = AstNode(AstNode.Kind.CLASS_DECLARATION)
    // To do: Might need to start with the enum modifiers and add to
    // them
    n.addChild(classDeclarationModifiers())
    val nameNode = current.getChild(1)
    n.addChild(nameNode)
    val enumerationBody = current.getChild(2)
    n.addChild(classBody(enumerationBody))
    return n

  def classDeclarationModifiers (): AstNode =
    val n = AstNode(AstNode.Kind.MODIFIERS)
    n.addChild(finalModifier())
    return n

  // Bug here index out of bounds, revisit parsing and make
  // sure the data structures match.

  def classBody (enumerationBody: AstNode): AstNode =
    val n = AstNode(AstNode.Kind.CLASS_BODY)
    for enumerationMember <- enumerationBody.getChildren() do
      n.addChild(classMember(enumerationMember))
    return n

  def classMember (enumerationMember: AstNode): AstNode =
    return variableDeclaration(enumerationMember)

  def variableDeclaration (enumerationConstantDeclaration: AstNode): AstNode =
    val n = AstNode(AstNode.Kind.VARIABLE_DECLARATION)
    n.addChild(variableModifiers())
    val enumerationConstantName = enumerationConstantDeclaration.getChild(0)
    n.addChild(variableName(enumerationConstantName))
    n.addChild(typeRoot())
    n.addChild(expression())
    return n

  def variableModifiers (): AstNode =
    val n = AstNode(AstNode.Kind.MODIFIERS)
    n.addChild(publicModifier())
    n.addChild(staticModifier())
    n.addChild(finalModifier())
    return n

  def variableName (enumerationConstantName: AstNode): AstNode =
    return enumerationConstantName

  def finalModifier (): AstNode =
    AstNode(AstNode.Kind.FINAL_MODIFIER)

  def publicModifier (): AstNode =
    AstNode(AstNode.Kind.PUBLIC_MODIFIER)

  def staticModifier (): AstNode =
    AstNode(AstNode.Kind.STATIC_MODIFIER)

  def expression (): AstNode =
    val n = literal()
    return n

  def literal (): AstNode =
    val n = AstNode(AstNode.Kind.INTEGER_LITERAL)
    val t = Token(Token.Kind.INT32_LITERAL, counter.toString(), 0, 0, 0)
    counter += 1
    n.setToken(t)
    return n

  def typeRoot (): AstNode =
    val n = AstNode(AstNode.Kind.TYPE_ROOT)
    n.addChild(primitiveType())
    return n

  def primitiveType (): AstNode =
    val n = AstNode(AstNode.Kind.PRIMITIVE_TYPE)
    // This is a virtual token, so position, line, and column are N/A
    val t = Token(Token.Kind.INT32, "int32", 0, 0, 0)
    n.setToken(t)
    return n

}
