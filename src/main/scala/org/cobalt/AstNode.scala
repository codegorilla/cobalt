package org.cobalt

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

// I prefer to use a homogenous AST design. This means that we tag each node
// with a 'kind' field instead of using separate subclasses. I also prefer that
// the AST nodes use normalized children. This means that instead of having
// unique fields for children, we just use a generic array or linked list. The
// homogenous AST with normalized children makes it easier to change the tree
// structure as the language grammar develops over time. For more information,
// see Parr, Ch. 4.

class AstNode (
  private var kind: AstNode.Kind,
  private var token: Token = null
):

  var children = ListBuffer[AstNode]()
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

  // Not sure we'll ever need to mutate kind in place. If not, then we can
  // remove the setter and make the the field immutable.

  def setKind (kind: AstNode.Kind) =
    this.kind = kind

  def setAttribute (name: String, value: Any) =
    attributes += (name -> value)

  def getToken (): Token =
    return token

  // Also not sure if we'll ever need to mutate this in place. It depends on
  // whether or not token is always known at time of AstNode creation.

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
    // ARGUMENTS?
    case ARGUMENT_LIST
    case ARRAY_ACCESS
    case BINARY_EXPRESSION
    case DEREF_ACCESS
    case EXPRESSION_ROOT
    case FIELD_ACCESS
    case FLOATING_POINT_LITERAL
    case FUNCTION_CALL
    case IF_EXPRESSION
    case INTEGER_LITERAL
    case ROUTINE_CALL
    case UNARY_EXPRESSION

    // Type expressions
    case TYPE_ROOT
    case ARRAY_TYPE
    case FUNCTION_POINTER_TYPE
    case NOMINAL_TYPE
    case POINTER_TYPE
    case PRIMITIVE_TYPE
