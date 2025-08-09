package org.cobalt

import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer

// If something goes wrong, and this is underlined in red, check that there are
// a bunch of libraries in metals. If not, use metals doctor. Might need to
// delete .bloop and .metals and re-import project.

import org.stringtemplate.v4.*

// The code generator converts the AST into the target language. Pass 2 handles
// most implementation concerns. It needs to revisit member routines because in
// C++, member functions are defined outside of the actual class in most cases
// (except inline) using fully qualified names.

class Generator2 {

  var input: AstNode = null

  // Load template group from template directory
  val templateDir = this.getClass().getClassLoader().getResource("templates")
  val group = STGroupDir(templateDir)

  // val templateFile = this.getClass().getClassLoader().getResource("templates/declarations/functionDeclaration1.stg")
  // val group1: STGroup = new STGroupFile(templateFile);

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
    var st = group.getInstanceOf("translationUnit")
    for child <- current.getChildren() do
      st.add("item", declaration(child))
    return st

  // For class declarations, for their member routine declarations, we need to
  // translate to both a member function declaration and a member function
  // definition. This is why there are two separate passes.

  // Note: We may need a somewhat more sophisticated approach for nested classes
  // because they can be nested arbitrarily deep and routine definitions must be
  // "lifted" to the outside (but then qualified) in order to avoid them being
  // treated as implicitly inline.

  def declaration (current: AstNode): ST =
    val kind = current.getKind()
    val st = kind match
      case AstNode.Kind.CLASS_DECLARATION =>
        classDeclaration(current)
      case AstNode.Kind.ROUTINE_DECLARATION =>
        routineDeclaration(current)
      case AstNode.Kind.VARIABLE_DECLARATION =>
        variableDeclaration(current)
      case _ =>
        println("No match in generator2/declaration.")
        null
    return st

  // CLASS DECLARATION

  def classDeclaration (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/classDeclaration2")
    // Not sure if we'll use this or not as prefix, just keep it around for now
    val name = className(current.getChild(1))
    st.add("classBody", classBody(current.getChild(2)))
    return st

  def className (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/className")
    st.add("name", current.getToken().lexeme)
    return st

  // Should we create artificial C++ string template constructs such as "Method
  // aggregation" to contain a group of methods that need to be passed back up?
  // Or should we just uses raw data structures or a stack for that?

  def classBody (current: AstNode): ListBuffer[ST] =
    val group = ListBuffer[ST]()
    for child <- current.getChildren() do
      val kind = child.getKind()
      if kind == AstNode.Kind.METHOD_DECLARATION then
        group += memberDeclaration(child)
    return group

  // Need to handle the fact that constructors do not have a return type (not
  // even auto or void). This can be handled by marking it as a constructor
  // during semantic analysis, or based on its name. Ideally, the code
  // generation phase just performs a straightforward translation and doesn't
  // have too much logic to decide such factors. Thus, I like the idea of
  // marking it during semantic analysis.

  def memberDeclaration (current: AstNode): ST =
    val kind = current.getKind()
    val st = kind match
      // case AstNode.Kind.CLASS_DECLARATION =>
      //   classDeclaration1(current)
      case AstNode.Kind.METHOD_DECLARATION =>
        memberRoutineDeclaration(current)
      case _ =>
        println("No match in generator2/memberDeclaration.")
        null
    return st

  // MEMBER ROUTINE DECLARATION

  def memberRoutineDeclaration (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/functionDefinition")
    st.add("functionModifiers1", routineModifiers1(current.getChild(0)))
    st.add("functionModifiers2", routineModifiers2(current.getChild(0)))
    st.add("functionModifiers3", routineModifiers3(current.getChild(0)))
    st.add("functionModifiers4", routineModifiers4(current.getChild(0)))
    st.add("functionName", routineName(current.getChild(1)))
    st.add("functionParameters", routineParameters(current.getChild(2)))
    st.add("functionReturnType", routineReturnType(current.getChild(3)))
    st.add("functionBody", routineBody(current.getChild(4)))
    return st

  // ROUTINE DECLARATION

  // C++ function modifiers are seemingly very inconsistent. Some go at the
  // front, some go in the middle, and some go at the back; depending on what
  // the modifier in question is.

  // A good way to handle this might be to get all of the modifiers into a list.
  // Then for each element of the list, figure out if it goes at the front,
  // middle, or back.

  // Having separate modifier processing functions probably isn't the most
  // sophisticated way to handle this, but its super simple to understand.

  def routineDeclaration (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/functionDefinition")
    st.add("functionModifiers1", routineModifiers1(current.getChild(0)))
    st.add("functionModifiers2", routineModifiers2(current.getChild(0)))
    st.add("functionModifiers3", routineModifiers3(current.getChild(0)))
    st.add("functionModifiers4", routineModifiers4(current.getChild(0)))
    st.add("functionName", routineName(current.getChild(1)))
    st.add("functionParameters", routineParameters(current.getChild(2)))
    st.add("functionReturnType", routineReturnType(current.getChild(3)))
    st.add("functionBody", routineBody(current.getChild(4)))
    return st

  def routineModifiers1 (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/functionModifiers1")
    for child <- current.getChildren() do
      val kind = child.getKind()
      if kind == AstNode.Kind.STATIC_MODIFIER then
        st.add("functionModifier", "static")
    return st

  def routineModifiers2 (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/functionModifiers2")
    for child <- current.getChildren() do
      val kind = child.getKind()
      if kind == AstNode.Kind.VIRTUAL_MODIFIER then
        st.add("functionModifier", "virtual")
    return st

  def routineModifiers3 (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/functionModifiers3")
    for child <- current.getChildren() do
      val kind = child.getKind()
      if kind == AstNode.Kind.CONST_MODIFIER then
        st.add("functionModifier", "const")
    return st

  def routineModifiers4 (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/functionModifiers4")
    for child <- current.getChildren() do
      val kind = child.getKind()
      if kind == AstNode.Kind.FINAL_MODIFIER then
        st.add("functionModifier", "final")
      else if kind == AstNode.Kind.OVERRIDE_MODIFIER then
        st.add("functionModifier", "override")
    return st

  val routineModifierMap = Map (
    "constexpr" -> "constexpr",
    "const" -> "const",
    "final" -> "final",
    "static" -> "static",
    "override" -> "override",
    "virtual" -> "virtual"
  )

  def routineModifier (current: AstNode): String =
    return routineModifierMap(current.getToken().lexeme)

  def routineName (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/functionName")
    st.add("name", current.getToken().lexeme)
    return st

  def routineParameters (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/functionParameters")
    for child <- current.getChildren() do
      st.add("functionParameter", routineParameter(child))
    return st

  def routineParameter (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/functionParameter")
    routineParameterName(current.getChild(0))
    typeRoot(current.getChild(1))
    st.add("typeSpecifier", stack.pop())
    st.add("declarator", stack.pop())
    return st

  def routineParameterName (current: AstNode) =
    val st = group.getInstanceOf("declarators/simpleDeclarator")
    st.add("name", current.getToken().lexeme)
    stack.push(st)

  // The routineReturnType node is somewhat like the variable declaration's type
  // specifier node.

  def routineReturnType (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/functionReturnType")
    if current.hasChildren() then
      // Push an empty ST because there isn't a name to go with the type.
      val emptyST = group.getInstanceOf("declarators/emptyDeclarator")
      stack.push(emptyST)
      typeRoot(current.getChild(0))
      st.add("typeSpecifier", stack.pop())
      st.add("declarator", stack.pop())
    return st

  def routineBody (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/functionBody")
    st.add("compoundStatement", compoundStatement(current.getChild(0)))
    return st

  // VARIABLE DECLARATION

  // Since cobalt supports type inference, we probably need to use the computed
  // type expression rather than the AST nodes since some variable declarations
  // will not have AST nodes for the type specifier.

  def variableDeclaration (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/variableDeclaration")
    st.add("variableModifiers", variableModifiers(current.getChild(0)))
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

  def variableModifiers (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/variableModifiers")
    for child <- current.getChildren() do
      st.add("variableModifier", variableModifier(child))
    return st

  // We might use 'public' and 'private' in cobalt to decide whether variables
  // should be exported or not from C++ modules. But they could mean something
  // different for global variables vs. local variables and member variables. So
  // this would have to be checked during semantic analysis phases.

  // Cobalt doesn't intend to make use of 'static' for global variables to mean
  // "do not export". I believe this is the only use of static at the global
  // level so it means this would be an invalid keyword on global variables.

  // We could use pattern matching instead of a map lookup. That would allow us
  // better control over output handling.

  val variableModifierMap = Map (
    "constexpr" -> "constexpr",
    "const" -> "const",
    "static" -> "static"
  )

  // String templates are only needed if we actually need to embed a string
  // inside other content. Otherwise a plain string can be used directly.

  def variableModifier (current: AstNode): String =
    return variableModifierMap(current.getToken().lexeme)

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
    if child.getKind() == AstNode.Kind.EXPRESSION then
      return expression(child)
    else
      return null

  // STATEMENTS

  def statement (current: AstNode): ST =
    val st = current.getKind() match
      case AstNode.Kind.BREAK_STATEMENT =>
        breakStatement(current)
      case AstNode.Kind.COMPOUND_STATEMENT =>
        compoundStatement(current)
      case AstNode.Kind.CONTINUE_STATEMENT =>
        continueStatement(current)
      case AstNode.Kind.EMPTY_STATEMENT =>
        emptyStatement(current)
      case AstNode.Kind.EXPRESSION_STATEMENT =>
        expressionStatement(current)
      case AstNode.Kind.IF_STATEMENT =>
        ifStatement(current)
      case AstNode.Kind.VARIABLE_DECLARATION =>
        variableDeclaration(current)
      case AstNode.Kind.RETURN_STATEMENT =>
        returnStatement(current)
      case AstNode.Kind.UNTIL_STATEMENT =>
        untilStatement(current)
      case AstNode.Kind.WHILE_STATEMENT =>
        whileStatement(current)
      case _ =>
        println("No match in generator/statement")
        null
    return st

  def breakStatement (current: AstNode): ST =
    val st = group.getInstanceOf("statements/breakStatement")
    return st

  def compoundStatement (current: AstNode): ST =
    val st = group.getInstanceOf("statements/compoundStatement")
    for child <- current.getChildren() do
      st.add("statement", statement(child))
    return st

  def continueStatement (current: AstNode): ST =
    val st = group.getInstanceOf("statements/continueStatement")
    return st

  // The empty statement could simply be omitted from the output since it
  // constitutes a "no-op" that has no effect. But it might serve as a useful
  // marker for troubleshooting purposes and is trivial to implement, so we
  // might as well keep it around for now.

  def emptyStatement (current: AstNode): ST =
    val st = group.getInstanceOf("statements/emptyStatement")
    return st

  def expressionStatement (current: AstNode): ST =
    val st = group.getInstanceOf("statements/expressionStatement")
    st.add("expression", expression(current.getChild(0)))
    return st

  def ifStatement (current: AstNode): ST =
    val st = group.getInstanceOf("statements/ifStatement")
    st.add("ifCondition", ifCondition(current.getChild(0)))
    st.add("ifBody", ifBody(current.getChild(1)))
    if current.getChildCount() == 3 then
      st.add("elseClause", elseClause(current.getChild(2)))
    return st

  def ifCondition (current: AstNode): ST =
    val st = group.getInstanceOf("statements/ifCondition")
    st.add("expression", expression(current))
    return st

  def ifBody (current: AstNode): ST =
    return statement(current)

  def elseClause (current: AstNode): ST =
    val st = group.getInstanceOf("statements/elseClause")
    st.add("elseBody", elseBody(current.getChild(0)))
    return st

  def elseBody (current: AstNode): ST =
    return statement(current)

  def returnStatement (current: AstNode): ST =
    val st = group.getInstanceOf("statements/returnStatement")
    if current.hasChildren() then
      st.add("expression", expression(current.getChild(0)))
    return st

  def untilStatement (current: AstNode): ST =
    val st = group.getInstanceOf("statements/untilStatement")
    st.add("untilCondition", untilCondition(current.getChild(0)))
    st.add("untilBody", whileBody(current.getChild(1)))
    return st

  def untilCondition (current: AstNode): ST =
    val st = group.getInstanceOf("statements/untilCondition")
    st.add("expression", expression(current))
    return st

  def untilBody (current: AstNode): ST =
    return statement(current)

  def whileStatement (current: AstNode): ST =
    val st = group.getInstanceOf("statements/whileStatement")
    st.add("whileCondition", whileCondition(current.getChild(0)))
    st.add("whileBody", whileBody(current.getChild(1)))
    return st

  def whileCondition (current: AstNode): ST =
    val st = group.getInstanceOf("statements/whileCondition")
    st.add("expression", expression(current))
    return st

  def whileBody (current: AstNode): ST =
    return statement(current)

  // EXPRESSIONS

  def expression (current: AstNode): ST =
    val kind = current.getKind()
    val st = kind match
      case AstNode.Kind.EXPRESSION => expression(current.getChild(0))
      case AstNode.Kind.BINARY_EXPRESSION => binaryExpression(current)
      case AstNode.Kind.UNARY_EXPRESSION => unaryExpression(current)
      case AstNode.Kind.BOOLEAN_LITERAL => booleanLiteral(current)
      case AstNode.Kind.FLOATING_POINT_LITERAL => floatingPointLiteral(current)
      case AstNode.Kind.INTEGER_LITERAL => integerLiteral(current)
      case _ => null
    return st

  def binaryExpression (current: AstNode): ST =
    val st = group.getInstanceOf("expressions/binaryExpression")
    // Note: If operators need to be translated, then we can map based on token
    // kind, but for now just use the token lexeme.
    st.add("operation", current.getToken().lexeme)
    st.add("leftExpr",  expression(current.getChild(0)))
    st.add("rightExpr", expression(current.getChild(1)))
    return st

  def unaryExpression (current: AstNode): ST =
    val st = group.getInstanceOf("expressions/unaryExpression")
    st.add("operation", current.getToken().lexeme)
    st.add("expression", expression(current.getChild(0)))
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
      case Token.Kind.FLOAT64 => "float64"
      case Token.Kind.VOID => "void"
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
    // The template argument is just a dummy method with no AST node. Can use
    // more thought.
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
