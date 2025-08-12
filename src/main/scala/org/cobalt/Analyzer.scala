package org.cobalt

import scala.collection.mutable.Map

// The analyzer produces no output. Its job is simply to annotate the AST with
// information that is useful for further processing passes. There will
// eventually be many analyzer passes (e.g. type-checking, constexpr validation,
// optimization).

// The goal of this first pass is to annotate declaration nodes with modifier
// information. Modifiers have significant impact on how constructs are
// interpreted and transformed; we don't want to have to descend into modifier
// sub-trees in order to determine how to process declarations. It is better if
// the declarations themselves are annotated with the information needed.

class Analyzer {

  var input: AstNode = null

  def setInput (input: AstNode) =
    this.input = input

  def process () =
    translationUnit(input)

  def translationUnit (current: AstNode) =
    for child <- current.getChildren() do
      declaration(child)

  def declaration (current: AstNode) =
    val kind = current.getKind()
    kind match
      case AstNode.Kind.CLASS_DECLARATION =>
        classDeclaration(current)
      case AstNode.Kind.ROUTINE_DECLARATION =>
        routineDeclaration(current)
      case _ =>
        println("No match in analyzer/declaration")

  // An abstract class cannot be directly instantiated. Only a subclass that
  // implements the abstract methods within can be instantiated. A final class
  // cannot be inherited from. A public class at the top level is one that is
  // exported from the module, whereas a private class at the top level is not.
  // A public class nested inside of another class is visible outside of the
  // outer class, whereas a private class nested inside of another class is not.

  // Note: Private is the default for both top-level and nested classes; thus,
  // it is never necessary to declare a class private. I may remove the option
  // in the future.

  def classDeclaration (current: AstNode) =
    val modifiers = current.getChild(0)
    for modifier <- modifiers.getChildren() do
      val kind = modifier.getKind()
      kind.match
        case AstNode.Kind.ABSTRACT_MODIFIER =>
          current.setAttribute("abstract", true)
        case AstNode.Kind.FINAL_MODIFIER =>
          current.setAttribute("final", true)
        case AstNode.Kind.PRIVATE_MODIFIER =>
          current.setAttribute("private", true)
        case AstNode.Kind.PUBLIC_MODIFIER =>
          current.setAttribute("public", true)
        case _ =>
          println("Invalid modifier on class")

  def routineDeclaration (current: AstNode) =
    val modifiers = current.getChild(0)
    for modifier <- modifiers.getChildren() do
      val kind = modifier.getKind()
      kind match
        case AstNode.Kind.CONST_MODIFIER =>
          current.setAttribute("const", true)
        case AstNode.Kind.PRIVATE_MODIFIER =>
          current.setAttribute("private", true)
        case AstNode.Kind.PUBLIC_MODIFIER =>
          current.setAttribute("public", true)
        case _ =>
          println("Invalid modifier on routine!")

      
    

}
