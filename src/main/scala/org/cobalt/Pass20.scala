package org.cobalt

import scala.collection.mutable.Stack

// The purpose of this pass is to process modifiers

// Modifiers are tricky for two reasons: First, keywords in the target
// language are not always the same as in the source language; and sometimes
// the mapping depends on context. For example, the keyword 'final' becomes
// 'const' for variables, but remains 'final' for classes. Second, the
// placement of modifiers might differ between source and target language. For
// example, in class declarations, 'final' shifts from before a class name to
// after. Also, the visibility modifiers (e.g. 'public', 'private') are lifted
// out of each individual method and the methods are then grouped under
// visibility "headings".

// Possible ways to handle these include setting attributes on declaration
// nodes that can be processed instead of directly translating lists of
// modifiers, and mapping from one keyword to another. Such maps should be
// done in a latter stage to allow a possible re-targetable transpiler.

class Pass20 (val input: AstNode) {

  // Used to pass nodes up and down during tree traversal
  val stack = Stack[AstNode]()

  def process () =
    translationUnit(input)

  def translationUnit (current: AstNode) =
    for child <- current.getChildren() do
      if child.getKind() == AstNode.Kind.VARIABLE_DECLARATION then
        globalVariableDeclaration(child)
      else if child.getKind() == AstNode.Kind.FUNCTION_DECLARATION then
        functionDeclaration(child)
  
  // Valid modifers for global variables:
  // const = variable is a compile-time constant (also implies final)
  // final = variable cannot be re-assigned a new value
  // public = exported from module
  // private = not exported from module

  def globalVariableDeclaration (current: AstNode) =
    modifiers(current.getChild(0))
    while !stack.isEmpty do
      val node = stack.pop()
      node.getKind() match
        case AstNode.Kind.CONST_MODIFIER => current.setAttribute("const", true)
        case AstNode.Kind.FINAL_MODIFIER => current.setAttribute("final", true)
        case AstNode.Kind.PUBLIC_MODIFIER => current.setAttribute("public", true)
        case AstNode.Kind.PRIVATE_MODIFIER => current.setAttribute("private", true)
        case _ => println(s"error: invalid modifier on global variable ${current.token}")

  // Valid modifers for functions:
  // const = function yields a compile-time constant (should also imply final?)
  // final = function cannot be overidden (should only apply to methods)
  // public = exported from module
  // private = not exported from module

  // Do we need a different method for function modifiers vs. variable
  // modifiers?

  def functionDeclaration (current: AstNode) =
    modifiers(current.getChild(0))
    while !stack.isEmpty do
      val node = stack.pop()
      node.getKind() match
        case AstNode.Kind.CONST_MODIFIER => current.setAttribute("const", true)
        case AstNode.Kind.FINAL_MODIFIER => current.setAttribute("final", true)
        case AstNode.Kind.PUBLIC_MODIFIER => current.setAttribute("public", true)
        case AstNode.Kind.PRIVATE_MODIFIER => current.setAttribute("private", true)
        case _ => println(s"error: invalid modifier on function ${current.token}")

  def modifiers (current: AstNode) =
    for child <- current.getChildren() do
      modifier(child)

  def modifier (current: AstNode) =
    stack.push(current)
    ()
}
