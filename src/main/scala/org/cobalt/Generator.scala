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

  // Since cobalt supports type inference, we probably need to use the computed
  // type expression rather than the AST nodes since some variable declarations
  // will not have AST nodes for the type specifier.

  def variableDeclaration (current: AstNode): ST =
    val st = group.getInstanceOf("variableDeclaration")
    modifiers(current.getChild(0))
    variableName(current.getChild(1))
    typeSpecifier(current.getChild(2))
    // Get translated type specifier and declarator from stack. The type
    // specifier should be something basic like 'int'.
    st.add("typeSpecifier", stack.pop())
    st.add("declarator", stack.pop())

    val initST = initializer(current.getChild(3))
    if initST != null then
      st.add("initializer", initST)
    return st

  // The variable name becomes a "simple declarator", which is the core of the
  // overall C++ declarator that gets built up. A C++ declaration is of the form
  // "typeSpecifier declarator", which is essentially the reverse of the cobolt
  // declaration of the form "variableName: typeSpecifier" (setting aside the
  // fact that the term "type specifier" has a different interpretation between
  // the two). Due to the need to swap the variable name with the base type from
  // the type specifier, and that the base type may be several levels down in
  // the type expression tree, we will use an explicit stack to facilitate the
  // exchange.

  def variableName (current: AstNode) =
    val st = group.getInstanceOf("declarators/simpleDeclarator")
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
      case AstNode.Kind.NOMINAL_TYPE =>
        nominalType(current)
      case AstNode.Kind.POINTER_TYPE =>
        pointerType(current)
      case AstNode.Kind.PRIMITIVE_TYPE =>
        primitiveType(current)
      case AstNode.Kind.TEMPLATE_TYPE =>
        templateType(current)

  // Todo: We might be able to reduce the number of parenthesis by peeking at
  // the next item on the stack. If it is the same type, then no parenthesis
  // should be required.

  def arrayType (current: AstNode) =
    val st = group.getInstanceOf("declarators/arrayDeclarator")
    st.add("declarator", stack.pop())
    stack.push(st)
    type_(current.getChild(1))

  def nominalType (current: AstNode) =
    val st = group.getInstanceOf("types/nominalType")
    val lexeme = current.getToken().lexeme
    st.add("name", lexeme)
    stack.push(st)

  def pointerType (current: AstNode) =
    val st = group.getInstanceOf("declarators/pointerDeclarator")
    st.add("declarator", stack.pop())
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
    st.add("name", type_)
    stack.push(st)

  def templateType (current: AstNode) =
    val st = group.getInstanceOf("types/templateType")
    val lexeme = current.getToken().lexeme
    st.add("name", lexeme)
    st.add("arguments", templateArguments(current.getChild(0)))
    stack.push(st)

  def templateArguments (current: AstNode): ST =
    val st = group.getInstanceOf("types/templateArguments")
    for child <- current.getChildren() do
      st.add("arguments", templateArgument(child))
    return st

  def templateArgument (current: AstNode): ST =
    // The template argument is just a dummy node. We need to get the child
    // node. We can change this in the parser later if we want.
    val st = group.getInstanceOf("types/templateArgument")
    val kind = current.getKind()
    if kind == AstNode.Kind.PRIMITIVE_TYPE then
      primitiveType(current)
    // Get the primitive type off of the stack
    st.add("argument", stack.pop())
    return st

  def modifiers (current: AstNode) =
    println(current)


}
