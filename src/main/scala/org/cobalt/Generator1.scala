package org.cobalt

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack

import java.util.LinkedList

import scala.jdk.CollectionConverters._

// If something goes wrong, and this is underlined in red, check that there are
// a bunch of libraries in metals. If not, use metals doctor. Might need to
// delete .bloop and .metals and re-import project.

import org.stringtemplate.v4.*

// The code generator converts the AST into the target language. Pass 1 handles
// mostly interface concerns.

class Generator1 {

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
        println("No match in generator/declaration.")
        null
    return st

  // CLASS DECLARATION

  // Todo: Add inheritance

  def classDeclaration (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/classDeclaration")
    st.add("classAccessSpecifier", classAccessSpecifier(current.getChild(0)))
    // StringTemplate can only work with Java collections for aggregates so we
    // need to convert Scala ListBuffer to Java LinkedList.
    val classModifiers = LinkedList(this.classModifiers(current.getChild(1)).asJava)
    st.add("classModifiers", classModifiers)
    st.add("className", className(current.getChild(2)))
    st.add("classBody", classBody(current.getChild(3)))
    return st

  def classAccessSpecifier (current: AstNode): String =
    var s: String = null
    val token = current.getToken()
    if token == null then
      // Classes are public by default
      s = "export"
    else
      s = token.kind match
      // Export is only used at module level
      case Token.Kind.PUBLIC  => "export"
      case Token.Kind.PRIVATE => null
      case _ =>
        println("Invalid access specifier on class!")
        null
    return s

  // C++ does not actually have an 'abstract' modifier for classes themselves.
  // Any class with at least one pure virtual function is considered an abstract
  // class.

  def classModifiers (current: AstNode): ListBuffer[String] =
    var s = ListBuffer[String]()
    for child <- current.getChildren() do
      val kind = child.getKind()
      if kind == AstNode.Kind.FINAL_MODIFIER then
        s += classModifier(child)
    return s

  val classModifierMap = Map (
    "final" -> "final"
  )

  def classModifier (current: AstNode): String =
    return classModifierMap(current.getToken().lexeme)

  def className (current: AstNode): String =
    return current.getToken().lexeme

  // Todo: Research and add protected member declarations as necessary

  def classBody (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/classBody")
    st.add("privateMemberDeclarations", privateMemberDeclarations(current))
    st.add("publicMemberDeclarations", publicMemberDeclarations(current))
    return st

  // We assume all member declarations have an access specifier, whether
  // explicit or implicit. If this is not true, we can refactor later.

  // What other members are allowed inside classes besides variables,
  // routines, and other classes?

  // To do: Handle member (i.e. nested) class declarations

  def privateMemberDeclarations (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/privateMemberDeclarations")
    for child <- current.getChildren() do
      val kind = child.getKind()
      kind match
        case AstNode.Kind.MEMBER_CLASS_DECLARATION =>
          val accessSpecifier = child.getChild(0)
          val token = accessSpecifier.getToken()
          if token != null && token.kind == Token.Kind.PRIVATE then
            st.add("memberDeclaration", classDeclaration(child))
        case AstNode.Kind.MEMBER_ROUTINE_DECLARATION =>
          val accessSpecifier = child.getChild(0)
          val token = accessSpecifier.getToken()
          if token != null && token.kind == Token.Kind.PRIVATE then
            st.add("memberDeclaration", memberRoutineDeclaration(child))
        case AstNode.Kind.MEMBER_VARIABLE_DECLARATION =>
          val accessSpecifier = child.getChild(0)
          val token = accessSpecifier.getToken()
          if token == null || token.kind == Token.Kind.PRIVATE then
            st.add("memberDeclaration", memberVariableDeclaration(child))
    return st

  def publicMemberDeclarations (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/publicMemberDeclarations")
    for child <- current.getChildren() do
      val kind = child.getKind()
      kind match
        case AstNode.Kind.MEMBER_CLASS_DECLARATION =>
          val accessSpecifier = child.getChild(0)
          val token = accessSpecifier.getToken()
          if token == null || token.kind == Token.Kind.PUBLIC then
            st.add("memberDeclaration", classDeclaration(child))
        case AstNode.Kind.MEMBER_ROUTINE_DECLARATION =>
          val accessSpecifier = child.getChild(0)
          val token = accessSpecifier.getToken()
          if token == null || token.kind == Token.Kind.PUBLIC then
            st.add("memberDeclaration", memberRoutineDeclaration(child))
        case AstNode.Kind.MEMBER_VARIABLE_DECLARATION =>
          val accessSpecifier = child.getChild(0)
          val token = accessSpecifier.getToken()
          if token != null && token.kind == Token.Kind.PUBLIC then
            st.add("memberDeclaration", memberVariableDeclaration(child))
    return st

  // MEMBER ROUTINE DECLARATION

  // Need to handle the fact that constructors do not have a return type (not
  // even auto or void). This can be handled by marking it as a constructor
  // during semantic analysis, or based on its name. Ideally, the code
  // generation phase just performs a straightforward translation and doesn't
  // have too much logic to decide such factors. Thus, I like the idea of
  // marking it during semantic analysis.

  def memberRoutineDeclaration (current: AstNode): ST =
    // val st = publicMemberRoutineDeclarations(current)
    val st = group.getInstanceOf("declarations/memberFunctionDeclaration")
    // StringTemplate can only work with Java collections for aggregates so we
    // need to convert Scala ListBuffer to Java LinkedList.
    val memberRoutineModifiers1 = LinkedList(this.memberRoutineModifiers1(current.getChild(1)).asJava)
    st.add("memberFunctionModifiers1", memberRoutineModifiers1)
    val memberRoutineModifiers2 = LinkedList(this.memberRoutineModifiers2(current.getChild(1)).asJava)
    st.add("memberFunctionModifiers2", memberRoutineModifiers2)
    // val memberRoutineModifiers3 = LinkedList(this.memberRoutineModifiers3(current.getChild(1)).asJava)
    // st.add("memberFunctionModifiers3", memberRoutineModifiers3)
    // val memberRoutineModifiers4 = LinkedList(this.memberRoutineModifiers4(current.getChild(1)).asJava)
    // st.add("memberFunctionModifiers4", memberRoutineModifiers4)
    st.add("memberFunctionName", memberRoutineName(current.getChild(2)))
    st.add("memberFunctionParameters", routineParameters(current.getChild(3)))
    st.add("memberFunctionReturnType", routineReturnType(current.getChild(4)))
    // Only needed for inline member routines
    // st.add("functionBody", routineBody(current.getChild(4)))
    return st

  // These modifiers go before the 'auto' keyword.

  def memberRoutineModifiers1 (current: AstNode): ListBuffer[String] =
    var s = ListBuffer[String]()
    for child <- current.getChildren() do
      val kind = child.getKind()
      if
        kind == AstNode.Kind.CONSTEXPR_MODIFIER ||
        kind == AstNode.Kind.STATIC_MODIFIER
      then
        s += memberRoutineModifier(child)
    return s

  // These modifiers would normally become CV qualifiers and go after the
  // parameters list. They include 'const' and 'volatile'. But these don't exist
  // on non-member routines.

  // Following that, there are ref qualifiers, which go after the CV qualifiers.
  // Again, these don't apply to non-member routines.

  def memberRoutineModifiers2 (current: AstNode): ListBuffer[String] =
    var s = ListBuffer[String]()
    for child <- current.getChildren() do
      val kind = child.getKind()
      if kind == AstNode.Kind.VIRTUAL_MODIFIER then
        s += routineModifier(child)
    return s

  def memberRoutineModifiers3 (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/functionModifiers3")
    for child <- current.getChildren() do
      val kind = child.getKind()
      if kind == AstNode.Kind.CONST_MODIFIER then
        st.add("functionModifier", "const")
    return st

  def memberRoutineModifiers4 (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/functionModifiers4")
    for child <- current.getChildren() do
      val kind = child.getKind()
      if kind == AstNode.Kind.FINAL_MODIFIER then
        st.add("functionModifier", "final")
      else if kind == AstNode.Kind.OVERRIDE_MODIFIER then
        st.add("functionModifier", "override")
    return st

  val memberRoutineModifierMap = Map (
    "const" -> "const",
    "constexpr" -> "constexpr",
    "final" -> "final",
    "override" -> "override",
    "static" -> "static",
    "virtual" -> "virtual"
  )

  def memberRoutineModifier (current: AstNode): String =
    return memberRoutineModifierMap(current.getToken().lexeme)

  def memberRoutineName (current: AstNode): String =
    return current.getToken().lexeme

  // MEMBER VARIABLE DECLARATION

  def memberVariableDeclaration (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/memberVariableDeclaration")
    // StringTemplate can only work with Java collections for aggregates so we
    // need to convert Scala ListBuffer to Java LinkedList.
    val variableModifiers = LinkedList(this.variableModifiers(current.getChild(1)).asJava)
    st.add("variableModifiers", variableModifiers)
    variableName(current.getChild(2))
    typeSpecifier(current.getChild(3))
    // Get translated type specifier and declarator from stack. The type
    // specifier should be something basic like 'int'.
    st.add("typeSpecifier", stack.pop())
    st.add("declarator", stack.pop())
    val initST = initializer(current.getChild(4))
    if initST != null then
      st.add("initializer", initST)
    return st

  // ROUTINE DECLARATION

  // C++ function modifiers are seemingly very inconsistent. Some go at the
  // front, some go in the middle, and some go at the back; depending on what
  // the modifier in question is.

  def routineDeclaration (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/functionDeclaration")
    st.add("functionAccessSpecifier", routineAccessSpecifier(current.getChild(0)))
    // StringTemplate can only work with Java collections for aggregates so we
    // need to convert Scala ListBuffer to Java LinkedList.
    val routineModifiers1 = LinkedList(this.routineModifiers1(current.getChild(1)).asJava)
    st.add("functionModifiers1", routineModifiers1)
    st.add("functionName", routineName(current.getChild(2)))
    st.add("functionParameters", routineParameters(current.getChild(3)))
    st.add("functionReturnType", routineReturnType(current.getChild(4)))
    return st

  def routineAccessSpecifier (current: AstNode): String =
    var s: String = null
    val token = current.getToken()
    if token == null then
      // Routines are public by default
      s = "export"
    else
      s = token.kind match
      // Export is only used at module level
      case Token.Kind.PUBLIC  => "export"
      case Token.Kind.PRIVATE => null
      case _ =>
        println("Invalid access specifier on routine!")
        null
    return s

  // Note: We don't use 'static' or 'virtual' modifiers on non-member routines.
  // Still need to research 'friend' modifier.

  def routineModifiers1 (current: AstNode): ListBuffer[String] =
    var s = ListBuffer[String]()
    for child <- current.getChildren() do
      val kind = child.getKind()
      if kind == AstNode.Kind.CONSTEXPR_MODIFIER then
        s += routineModifier(child)
    return s

  val routineModifierMap = Map (
    "constexpr" -> "constexpr"
  )

  def routineModifier (current: AstNode): String =
    return routineModifierMap(current.getToken().lexeme)

  def routineName (current: AstNode): String =
    return current.getToken().lexeme

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

  // VARIABLE DECLARATION

  // Since cobalt supports type inference, we probably need to use the computed
  // type expression rather than the AST nodes since some variable declarations
  // will not have AST nodes for the type specifier.

  // We need to know if something is a global variable or local variable or
  // member variable because this affects how it is translated. If it is global
  // then it might be placed in an interface file or implementation file,
  // depending on whether or not it is exported.

  def variableDeclaration (current: AstNode): ST =
    val st = group.getInstanceOf("declarations/variableDeclaration")
    st.add("variableAccessSpecifier", variableAccessSpecifier(current.getChild(0)))
    // StringTemplate can only work with Java collections for aggregates so we
    // need to convert Scala ListBuffer to Java LinkedList.
    val variableModifiers = LinkedList(this.variableModifiers(current.getChild(1)).asJava)
    st.add("variableModifiers", variableModifiers)
    variableName(current.getChild(2))
    typeSpecifier(current.getChild(3))
    // Get translated type specifier and declarator from stack. The type
    // specifier should be something basic like 'int'.
    st.add("typeSpecifier", stack.pop())
    st.add("declarator", stack.pop())
    val initST = initializer(current.getChild(4))
    if initST != null then
      st.add("initializer", initST)
    return st

  def variableAccessSpecifier (current: AstNode): String =
    var s: String = null
    val token = current.getToken()
    if token == null then
      // Variables are private by default
      s = null
    else
      s = token.kind match
      // Export is only used at module level
      case Token.Kind.PUBLIC  => "export"
      case Token.Kind.PRIVATE => null
      case _ =>
        println("Invalid access specifier on variable!")
        null
    return s

  def variableModifiers (current: AstNode): ListBuffer[String] =
    var s = ListBuffer[String]()
    for child <- current.getChildren() do
      s += variableModifier(child)
    return s

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
    "const" -> "const",
    "constexpr" -> "constexpr",
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
