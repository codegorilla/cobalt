package org.cobalt

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

class AstNode (kind: AstNode.Kind):

  var children = ListBuffer[AstNode]()
  var token: Token = null
  val attributes = Map[String, Any]()

  def addChild (node: AstNode) =
    children += node

  def getKind (): AstNode.Kind =
    return kind

  def setAttribute (name: String, value: Any) =
    attributes += (name -> value)

  def setToken (token: Token) =
    this.token = token

  override def toString (): String =
    return s"AstNode(${kind}, ${token})"


object AstNode:
  enum Kind:
    case PLACEHOLDER
    case TRANSLATION_UNIT

    // Declarations
    case NAME

    // Modifiers
    case FINAL_MODIFIER
    case MODIFIERS
    case PUBLIC_MODIFIER
    case STATIC_MODIFIER

    // Class declaration
    case CLASS_DECLARATION

    // Function declaration
    case FUNCTION_DECLARATION
    case PARAMETERS
    case PARAMETER
    case RESULT
    case FUNCTION_BODY
    case BLOCK

    // Variable declaration
    case VARIABLE_DECLARATION
    case TYPE_SPECIFIER
    case INITIALIZER

    case IDENTIFIER

    // Statements
    case BREAK_STATEMENT
    case CONTINUE_STATEMENT

    // Type expressions
    case TYPE_ROOT
    case ARRAY_TYPE
    case FUNCTION_POINTER_TYPE
    case NOMINAL_TYPE
    case POINTER_TYPE
    case PRIMITIVE_TYPE
