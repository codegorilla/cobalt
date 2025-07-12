package org.cobalt

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

class AstNode (kind: AstNode.Kind):

  var children = ListBuffer[AstNode]()
  var token: Token = null
  val attributes = Map[String, Any]()

  def addChild (node: AstNode) =
    children += node

  def getChild (index: Int): AstNode =
    return children(index)

  def getChildCount (): Int =
    return children.size

  def getChildren (): Iterator[AstNode] =
    return children.iterator

  def getKind (): AstNode.Kind =
    return kind

  def setAttribute (name: String, value: Any) =
    attributes += (name -> value)

  def getToken (): Token =
    return token

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
    case CONST_MODIFIER
    case FINAL_MODIFIER
    case MODIFIERS
    case OVERRIDE_MODIFIER
    case PRIVATE_MODIFIER
    case PUBLIC_MODIFIER
    case STATIC_MODIFIER

    // Class declaration
    case CLASS_DECLARATION
    case CLASS_BODY

    // Method declaration
    case METHOD_DECLARATION
    case METHOD_BODY

    // Eumeration declaration
    case ENUMERATION_DECLARATION
    case ENUMERATION_BODY
    case ENUMERATION_CONSTANT_DECLARATION

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
    case RETURN_STATEMENT

    // Expressions
    case FLOATING_POINT_LITERAL
    case INTEGER_LITERAL

    // Type expressions
    case TYPE_ROOT
    case ARRAY_TYPE
    case FUNCTION_POINTER_TYPE
    case NOMINAL_TYPE
    case POINTER_TYPE
    case PRIMITIVE_TYPE
