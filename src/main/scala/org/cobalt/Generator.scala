package org.cobalt

import scala.collection.mutable.Stack

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

  // Used to pass string templates up and down during tree traversal
  val stack = Stack[ST]()

  def setInput (input: AstNode) =
    this.input = input

  def process (): ST =
    val st = translationUnit(input)
    return st

  def translationUnit (current: AstNode): ST =
    var st: ST = null
    var st1 = group.getInstanceOf("translationUnit")
    for child <- current.getChildren() do
      if child.getKind() == AstNode.Kind.VARIABLE_DECLARATION then
        st = variableDeclaration(child)
        st1.add("item", st)
    return st1

  // VARIABLE DECLARATION

  // We need to trade the variable name for the basic declarator. This will be
  // done with the generator stack.

  // <type> <name> = <init>;
  // should be
  // <typeSpec> <declarator> = <init>;


  def variableDeclaration (current: AstNode): ST =
    val st = group.getInstanceOf("variableDeclaration")
    modifiers(current.getChild(0))
    // We need to push the variable name onto the stack, which becomes the
    // declarator.
    //st.add("name", variableName(current.getChild(1)))
    variableName(current.getChild(1))
    // We'll get the type specifier off of the stack. It should just be a basic
    // type like 'int'.
    typeSpecifier(current.getChild(2))

    // Get translated type specifier
    val typeSpec = stack.pop()
    st.add("type", typeSpec)
    //Get translated declarator
    val declarator = stack.pop()
    st.add("declarator", declarator)

    val initST = initializer(current.getChild(3))
    if initST != null then
      st.add("init", initST)
    return st

  def variableName (current: AstNode) =
    val st = group.getInstanceOf("variableName")
    st.add("name", current.getToken().lexeme)
    stack.push(st)

  // Type specifiers may actually be empty, in which case type inference is
  // used. By the time we get to the code generation phase, a type will have
  // already been inferred. However, it turns out that this information is
  // stored in type objects and/or the symbol table, so we wouldn't actually be
  // populating the string templates using token lexemes from the AST. Instead,
  // we would be pulling information from the symbol table.

  def typeSpecifier (current: AstNode) =
    if current.hasChildren() then
      typeRoot(current.getChild(0))
    // else
    //   return null

  // Initializers may be expressions or array/struct building code. The latter
  // still needs to be implemented.

  def initializer (current: AstNode): ST =
    val child = current.getChild(0)
    if child.getKind() == AstNode.Kind.EXPRESSION_ROOT then
      return expressionRoot(child)
    else
      return null

  // EXPRESSIONS

  def expressionRoot (current: AstNode): ST =
    return expression(current.getChild(0))

  def expression (current: AstNode): ST =
    val kind = current.getKind()
    val st = kind match
      case AstNode.Kind.BINARY_EXPRESSION => binaryExpression(current)
      case AstNode.Kind.BOOLEAN_LITERAL => booleanLiteral(current)
      case AstNode.Kind.FLOATING_POINT_LITERAL => floatingPointLiteral(current)
      case AstNode.Kind.INTEGER_LITERAL => integerLiteral(current)
      case _ => null
    return st

  def binaryExpression (current: AstNode): ST =
    val st = group.getInstanceOf("expressions/binaryExpression")
    // Note: If operators need to be translated, then we can map based on token
    // kind, but for now just use the token lexeme.
    st.add("op", current.getToken().lexeme)
    st.add("leftExpr",  expression(current.getChild(0)))
    st.add("rightExpr", expression(current.getChild(1)))
    return st

  def booleanLiteral (current: AstNode): ST =
    val st = group.getInstanceOf("expressions/booleanLiteral")
    st.add("value", current.getToken().lexeme)
    return st

  def floatingPointLiteral (current: AstNode): ST =
    val st = group.getInstanceOf("expressions/floatingPointLiteral")
    st.add("value", current.getToken().lexeme)
    return st

  def integerLiteral (current: AstNode): ST =
    val st = group.getInstanceOf("expressions/integerLiteral")
    st.add("value", current.getToken().lexeme)
    return st

  // TYPES

  // Translation just seems to need AST nodes from the parser rather than the
  // types computed during semantic analysis, which will just be used for type
  // checking.

  // We can create a stack of string templates and push/pull them as we go down.
  // Probably don't need to return anything. Just use the stack.

  def typeRoot (current: AstNode) =
    // Variable name should be on the stack at this point.
    type_(current.getChild(0))
    // At this point, the stack should contain a C++ declarator as the bottom
    // element and a C++ type specifier as the top element.

  def type_ (current: AstNode): Unit =
    val kind = current.getKind()
    kind match
      case AstNode.Kind.ARRAY_TYPE =>
        arrayType(current)
      case AstNode.Kind.POINTER_TYPE =>
        pointerType(current)
      case AstNode.Kind.PRIMITIVE_TYPE =>
        primitiveType(current)

  def arrayType (current: AstNode) =
    val st = group.getInstanceOf("types/arrayType")
    st.add("type", stack.pop())
    stack.push(st)
    type_(current.getChild(1))

  def pointerType (current: AstNode) =
    val st = group.getInstanceOf("types/pointerType")
    st.add("type", stack.pop())
    stack.push(st)
    type_(current.getChild(0))

  def primitiveType (current: AstNode) =
    val st = group.getInstanceOf("types/primitiveType")
    val kind = current.getToken().kind
    // We can probably just use the token lexeme, but we could also map from
    // token kind to a string representing the target language type.
    val type_ = kind match
      case Token.Kind.INT => "int"
      case Token.Kind.FLOAT => "float"
    st.add("declarator", type_)
    stack.push(st)

  def modifiers (current: AstNode) =
    println(current)


}
