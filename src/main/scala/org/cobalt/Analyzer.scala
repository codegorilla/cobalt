package org.cobalt

import scala.collection.mutable.Map

// The analyzer produces no output. Its job is simply to annotate the AST with
// information that is useful for further processing passes. There will
// eventually be many analyzer passes (e.g. type-checking, constexpr validation,
// optimization).

// The goal of this first pass is to annotate declaration nodes with information
// about access specifiers and modifiers. These have significant impact on how
// constructs are interpreted and transformed; we don't want to have to descend
// into modifier sub-trees in order to determine how to process declarations. It
// is better if the declarations themselves are annotated with the information
// needed.

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
      case AstNode.Kind.VARIABLE_DECLARATION =>
        variableDeclaration(current)
      case _ =>
        println("No match in analyzer/declaration")

  // An abstract class cannot be directly instantiated. Only a subclass that
  // implements the abstract methods within can be instantiated. A final class
  // cannot be inherited from. A public class at the top level is one that is
  // exported from the module, whereas a private class at the top level is not.
  // A public class nested inside of another class is visible outside of the
  // outer class, whereas a private class nested inside of another class is not.

  // For now, classes default to public. Helper and auxiliary classes are often
  // private, but most class use cases require them to be public.

  def classDeclaration (current: AstNode) =
    classAccessSpecifier(current)
    classModifiers(current)

  def classAccessSpecifier (current: AstNode) =
    val accessSpecifier = current.getChild(0)
    val token = accessSpecifier.getToken()
    if token == null then
      // Classes are public by default
      current.setAttribute("public", true)
    else
      token.kind match
        case Token.Kind.PUBLIC =>
          current.setAttribute("public", true)
        case Token.Kind.PRIVATE =>
          current.setAttribute("private", true)
        case _ =>
          println("Invalid access specifier on class!")

  def classModifiers (current: AstNode) =
    val modifiers = current.getChild(1)
    for modifier <- modifiers.getChildren() do
      val kind = modifier.getKind()
      kind.match
        case AstNode.Kind.ABSTRACT_MODIFIER =>
          current.setAttribute("abstract", true)
        case AstNode.Kind.FINAL_MODIFIER =>
          current.setAttribute("final", true)
        case _ =>
          println("Invalid modifier on class")

  def routineDeclaration (current: AstNode) =
    routineAccessSpecifier(current)
    routineModifiers(current)

  def routineAccessSpecifier (current: AstNode) =
    val accessSpecifier = current.getChild(0)
    val token = accessSpecifier.getToken()
    if token == null then
      // Routines are public by default
      current.setAttribute("public", true)
    else
      token.kind match
      case Token.Kind.PUBLIC =>
        current.setAttribute("public", true)
      case Token.Kind.PRIVATE =>
        current.setAttribute("private", true)
      case _ =>
        println("Invalid access specifier on routine!")

  def routineModifiers (current: AstNode) =
    val modifiers = current.getChild(1)
    for modifier <- modifiers.getChildren() do
      val kind = modifier.getKind()
      kind match
        case AstNode.Kind.ABSTRACT_MODIFIER =>
          current.setAttribute("abstract", true)
        case AstNode.Kind.CONST_MODIFIER =>
          current.setAttribute("const", true)
        case AstNode.Kind.FINAL_MODIFIER =>
          current.setAttribute("final", true)
        case AstNode.Kind.OVERRIDE_MODIFIER =>
          current.setAttribute("override", true)
        case AstNode.Kind.STATIC_MODIFIER =>
          current.setAttribute("static", true)
        case AstNode.Kind.VIRTUAL_MODIFIER =>
          current.setAttribute("virtual", true)
        case AstNode.Kind.VOLATILE_MODIFIER =>
          current.setAttribute("volatile", true)
        case _ =>
          println("Invalid modifier on routine!")

  def variableDeclaration (current: AstNode) =
    variableAccessSpecifier(current)
    variableModifiers(current)

  def variableAccessSpecifier (current: AstNode) =
    val accessSpecifier = current.getChild(0)
    val token = accessSpecifier.getToken()
    if token == null then
      // Variables are private by default
      current.setAttribute("private", true)
      println("PRIVATE DEFAULT")
    else
      token.kind match
      case Token.Kind.PUBLIC =>
        current.setAttribute("public", true)
        println("PUBLIC FOUND")
      case Token.Kind.PRIVATE =>
        current.setAttribute("private", true)
        println("PRIVATE FOUND")
      case _ =>
        println("Invalid access specifier on variable!")

  def variableModifiers (current: AstNode) =
    val modifiers = current.getChild(1)
    for modifier <- modifiers.getChildren() do
      val kind = modifier.getKind()
      kind match
        case AstNode.Kind.CONST_MODIFIER =>
          current.setAttribute("const", true)
        case AstNode.Kind.STATIC_MODIFIER =>
          current.setAttribute("static", true)
        case AstNode.Kind.VOLATILE_MODIFIER =>
          current.setAttribute("volatile", true)
        case _ =>
          println("Invalid modifier on variable!")

}
