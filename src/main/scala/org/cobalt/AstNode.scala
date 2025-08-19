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

  def getAttribute (name: String): Any =
    return attributes.get(name)

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

  def hasChildren () =
    if !children.isEmpty then true else false

  override def toString (): String =
    return s"AstNode(${kind}, ${token})"


object AstNode:
  enum Kind:
    case PLACEHOLDER
    case TRANSLATION_UNIT

    // Declarations
    case DECLARATIONS
    case NAME

    // Access specifiers
    case ACCESS_SPECIFIER
    case PRIVATE_SPECIFIER
    case PROTECTED_SPECIFIER
    case PUBLIC_SPECIFIER

    // Modifiers
    case ABSTRACT_MODIFIER
    case CONST_MODIFIER
    case CONSTEXPR_MODIFIER
    case FINAL_MODIFIER
    case MODIFIERS
    case OVERRIDE_MODIFIER
    case STATIC_MODIFIER
    case VIRTUAL_MODIFIER
    case VOLATILE_MODIFIER

    // Import declaration
    case IMPORT_DECLARATION

    // Member class declaration
    case MEMBER_CLASS_DECLARATION

    // Class declaration
    case CLASS_DECLARATION
    case CLASS_BODY
    case BASE_CLASS
    case BASE_CLASSES
    case BASE_CLAUSE

    // Eumeration declaration
    case ENUMERATION_DECLARATION
    case ENUMERATION_BODY
    case ENUMERATION_CONSTANT_DECLARATION

    // Member routine declaration
    case MEMBER_ROUTINE_BODY
    case MEMBER_ROUTINE_DECLARATION
    case MEMBER_ROUTINE_PARAMETER
    case MEMBER_ROUTINE_PARAMETERS
    case MEMBER_ROUTINE_RESULT

    // Routine declaration
    case ROUTINE_BODY
    case ROUTINE_DECLARATION
    case ROUTINE_PARAMETER
    case ROUTINE_PARAMETER_NAME
    case ROUTINE_PARAMETERS
    case ROUTINE_RETURN_TYPE

    // Template declaration
    case TEMPLATE_DECLARATION
    case TEMPLATE_PARAMETERS
    case TEMPLATE_PARAMETER

    // Member variable declaration
    case MEMBER_VARIABLE_DECLARATION

    // Variable declaration
    case LOCAL_VARIABLE_DECLARATION
    case VARIABLE_DECLARATION
    case TYPE_SPECIFIER
    case INITIALIZER

    case IDENTIFIER

    // SPECIAL
    case EMPTY

    // Statements
    case BREAK_STATEMENT
    case COMPOUND_STATEMENT
    case CONDITION
    case CONTINUE_STATEMENT
    case DO_STATEMENT
    case ELSE_CLAUSE
    case EMPTY_STATEMENT
    case EXPRESSION_STATEMENT
    case FOR_STATEMENT
    case FOR_CONDITION
    case FOR_INIT
    case FOR_INIT_DECLARATION
    case FOR_INIT_EXPRESSION_LIST
    case FOR_UPDATE
    case FOREACH_STATEMENT
    case IF_STATEMENT
    case RETURN_STATEMENT
    case UNTIL_STATEMENT
    case WHILE_STATEMENT


    // Expressions
    case BINARY_EXPRESSION
    case EXPRESSION
    case UNARY_EXPRESSION

    // Postfix expressions
    case DEREFERENCING_MEMBER_ACCESS
    case MEMBER_ACCESS
    case ROUTINE_CALL
    case ARGUMENTS
    case SUBSCRIPT

    // Primary expressions
    case IF_EXPRESSION
    case THIS

    // Literal expressions
    case BOOLEAN_LITERAL
    case FLOATING_POINT_LITERAL
    case INTEGER_LITERAL
    case UNSIGNED_INTEGER_LITERAL
    case NULL_LITERAL
    case CHARACTER_LITERAL
    case STRING_LITERAL

    // Type expressions
    case TYPE_ROOT
    case ARRAY_TYPE
    case ROUTINE_POINTER_TYPE
    case NOMINAL_TYPE
    case POINTER_TYPE
    case PRIMITIVE_TYPE
    case TEMPLATE_TYPE

    case TEMPLATE_ARGUMENTS
    case TEMPLATE_ARGUMENT
