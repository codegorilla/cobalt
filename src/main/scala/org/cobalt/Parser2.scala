package org.cobalt

import org.cobalt.AstNode.Kind

import scala.collection.mutable.ArrayDeque
import scala.collection.mutable.Map

// Try java linked list instead of scala deque
import java.util.LinkedList

// This first parser just needs to create a class table so we know if
// certain productions are classes or not. We also need to be able to
// follow typealiases to their target types.

// We cannot construct an AST in the first parsing pass because that
// would require that we be able to fully parse the input, but we
// cannot do that because there might be forward references, e.g.
// List[Int].someMethod(), where List[T] is defined later. The parser
// cannot tell if L[X] is an array dereference or the beginning of a
// static method call. (Same issue arises with List<int>.)

// First pass parser just looks for classes

class Parser2 {

  val SLEEP_TIME = 200

  var symbolTable: SymbolTable = null

  var input: List[Token] = null
  var position = 0
  var lookahead: Token = null

  def setInput (input: List[Token]) =
    this.input = input
    lookahead = input(position)

  def setSymbolTable (symbolTable: SymbolTable) =
    this.symbolTable = symbolTable

  def match_ (kind: Token.Kind) =
    if lookahead.kind == kind then
      consume()
    else
      print(s"invalid token: expected ${kind}, got ${lookahead.kind}")

  def match_ (lexeme: String) =
    // Note: If re-writing in java, we need to compare using .equals() method
    if lookahead.lexeme == lexeme then
      consume()
    else
      print(s"invalid token: expected '${lexeme}', got '${lookahead.lexeme}'")

  def consume () =
    position += 1
    lookahead = input(position)

  def process (): AstNode =
    val node = translationUnit()
    return node

  def translationUnit (): AstNode =
    val n = AstNode(AstNode.Kind.TRANSLATION_UNIT)
    while lookahead.kind != Token.Kind.EOF do
      // Infinite loop, need to consume
      Thread.sleep(SLEEP_TIME)
      n.addChild(declaration())
    return n

  // DECLARATIONS

  def declaration (): AstNode =
    val p = modifiers()
    val n = lookahead.kind match
      case Token.Kind.CLASS => classDeclaration(p)
      case Token.Kind.VAL => variableDeclaration(p, true)
      case Token.Kind.VAR => variableDeclaration(p, false)
      case Token.Kind.DEF => functionDeclaration(p)
      case _ =>
        println("Found something else!")
        null
    return p

  def modifiers (): AstNode =
    val n = AstNode(AstNode.Kind.MODIFIERS)
    while lookahead.kind == Token.Kind.PUBLIC ||
          lookahead.kind == Token.Kind.STATIC
    do
      val modifier = lookahead.kind match
        case Token.Kind.PUBLIC => publicModifier()
        case Token.Kind.STATIC => staticModifier()
        case _ =>
          print("error: This can only happen if there is a parser error.")
          null
      n.addChild(modifier)
    return n

  def finalModifier (): AstNode =
    // No corresponding token, so match not needed
    AstNode(AstNode.Kind.FINAL_MODIFIER)

  def publicModifier (): AstNode =
    match_(Token.Kind.PUBLIC)
    AstNode(AstNode.Kind.PUBLIC_MODIFIER)

  def staticModifier (): AstNode =
    match_(Token.Kind.STATIC)
    AstNode(AstNode.Kind.STATIC_MODIFIER)

  def classDeclaration (modifiers: AstNode): AstNode =
    val n = AstNode(AstNode.Kind.CLASS_DECLARATION)
    match_(Token.Kind.CLASS)
    n.addChild(identifier())
    match_(Token.Kind.L_BRACE)
    while lookahead.kind != Token.Kind.R_BRACE do
      n.addChild(classMember())
    match_(Token.Kind.R_BRACE)
    return n

  def classMember (): AstNode =
    // To do
    return AstNode(AstNode.Kind.PLACEHOLDER)

  def variableDeclaration (modifiers: AstNode, finalFlag: Boolean): AstNode =
    val n = AstNode(AstNode.Kind.VARIABLE_DECLARATION)
    if finalFlag then
      match_(Token.Kind.VAL)
      modifiers.addChild(finalModifier())
    else
      match_(Token.Kind.VAR)
    n.addChild(modifiers)
    n.addChild(name())
    n.addChild(typeSpecifier())
    match_(Token.Kind.SEMICOLON)
    return n

  def typeSpecifier (): AstNode =
    val n = AstNode(AstNode.Kind.TYPE_SPECIFIER)
    if lookahead.kind == Token.Kind.COLON then
      match_(Token.Kind.COLON)
      // Need to fix up array handling
      n.addChild(typeRoot())
    return n

  def initializer (): AstNode =
    val n = AstNode(AstNode.Kind.INITIALIZER)
    if lookahead.kind == Token.Kind.EQUAL then
      match_(Token.Kind.EQUAL)
      n.addChild(expression())
    return n

  def functionDeclaration (modifiers: AstNode): AstNode =
    val n = AstNode(AstNode.Kind.FUNCTION_DECLARATION)
    match_(Token.Kind.DEF)
    n.addChild(modifiers)
    n.addChild(name())
    n.addChild(parameters())
    n.addChild(result())
    return n

  def parameters (): AstNode =
    val n = AstNode(AstNode.Kind.PARAMETERS)
    match_(Token.Kind.L_PARENTHESIS)
    if lookahead.kind == Token.Kind.IDENTIFIER then
      n.addChild(parameter())
    while lookahead.kind == Token.Kind.COMMA do
      match_(Token.Kind.COMMA)
      n.addChild(parameter())
    match_(Token.Kind.R_PARENTHESIS)
    return n

  def parameter (): AstNode =
    val n = AstNode(AstNode.Kind.PARAMETER)
    n.addChild(name())
    match_(Token.Kind.COLON)
    n.addChild(typeRoot())
    return n

  def result (): AstNode =
    val n = AstNode(AstNode.Kind.RESULT)
    if lookahead.kind == Token.Kind.MINUS_GREATER then
      match_(Token.Kind.MINUS_GREATER)
      n.addChild(typeRoot())
    return n

  def functionBody (): AstNode =
    val n = AstNode(AstNode.Kind.FUNCTION_BODY)
    if lookahead.kind == Token.Kind.SEMICOLON then
      match_(Token.Kind.SEMICOLON)
    else
      n.addChild(block())
    return n

  // The top-most block needs to use the scope of the function itself
  // so we might need a topBlock production. Alternatively, we can
  // pass in a parameter that says whether or not to create a new
  // scope.

  def block (): AstNode =
    val n = AstNode(AstNode.Kind.BLOCK)
    match_(Token.Kind.L_BRACE)
    while lookahead.kind != Token.Kind.R_BRACE do
      // Do blocks only contain statements? If so, then we don't need
      // blockElement. Otherwise, if we full distinguish between
      // declarations and statements, then we need blockElement
      n.addChild(statement())
      match_(Token.Kind.R_BRACE)
    return n

  def name (): AstNode =
    val n = AstNode(AstNode.Kind.NAME)
    n.setToken(lookahead)
    match_(Token.Kind.IDENTIFIER)
    return n

  // STATEMENTS

  def statement (): AstNode =
    val kind = lookahead.kind
    var n: AstNode = null
    if kind == Token.Kind.IDENTIFIER ||
       kind == Token.Kind.NULL ||
       kind == Token.Kind.FALSE ||
       kind == Token.Kind.THIS ||
       kind == Token.Kind.TRUE ||
       kind == Token.Kind.INT32_LITERAL ||
       kind == Token.Kind.INT64_LITERAL ||
       kind == Token.Kind.UINT32_LITERAL ||
       kind == Token.Kind.UINT64_LITERAL ||
       kind == Token.Kind.FLOAT32_LITERAL ||
       kind == Token.Kind.FLOAT64_LITERAL
    then
      n = expressionStatement()
    else if kind == Token.Kind.BREAK then
      n = breakStatement()
    else if kind == Token.Kind.CONTINUE then
      n = continueStatement()
    // else if kind == Token.Kind.DO then
    //   n = doStatement()
    // else if kind == Token.Kind.FOR then
    //   n = forStatement()
    // else if kind == Token.Kind.IF then
    //   n = ifStatement()
    // else if kind == Token.Kind.RETURN then
    //   n = returnStatement()
    // else if kind == Token.Kind.WHILE then
    //   n = whileStatement()
    // else if kind == Token.Kind.SEMICOLON then
    //   n = nullStatement()
    // else if kind == Token.Kind.VAL || kind == Token.Kind.VAR then
    //   n = declarationStatement()
    else
      print("Error: Invalid statement")
    return n

  def breakStatement (): AstNode =
    val n = AstNode(AstNode.Kind.BREAK_STATEMENT)
    match_(Token.Kind.BREAK)
    match_(Token.Kind.SEMICOLON)
    return n

  def continueStatement (): AstNode =
    val n = AstNode(AstNode.Kind.CONTINUE_STATEMENT)
    match_(Token.Kind.CONTINUE)
    match_(Token.Kind.SEMICOLON)
    return n


  def expressionStatement(): AstNode =
    return null




  // EXPRESSIONS

  def expression (): AstNode =
    println("EXPRESSION***")
    val n = AstNode(AstNode.Kind.PLACEHOLDER)
    consume()
    return n

  def identifier (): AstNode =
    val n = AstNode(AstNode.Kind.IDENTIFIER)
    n.setToken(lookahead)
    match_(Token.Kind.IDENTIFIER)
    return n

  // TYPES

  // Type processing is interesting because Cobalt uses a form of the
  // C-declaration style, so parsing types requires following the
  // "spiral rule". To make this easier, we make use of doubly-linked
  // lists provided by the language rather than complicating AST
  // node class definition with parent links. We can re-think this in
  // the future if we wish.

  def typeRoot (): AstNode =
    val n = AstNode(AstNode.Kind.TYPE_ROOT)
    n.addChild(type_())
    return n

  def type_ (): AstNode =
    val combined = directType()
    println(combined)
    // Pop base type node
    var n = combined.removeFirst()
    // Pop each type node from list, constructing chain of pointers
    // and arrays as we go. When done, the list should be empty.
    while combined.size() > 0 do
      val p = combined.removeFirst()
      p.getKind() match
        case AstNode.Kind.POINTER_TYPE =>
          p.addChild(n)
          n = p
        case AstNode.Kind.ARRAY_TYPE =>
          p.addChild(n)
          n = p
        case _ =>
          println("error: This is not possible without a parser error.")
    return n

  def directType (): LinkedList[AstNode] =
    // Build type fragments in order they appear in token stream
    val left   = leftFragment()
    val center = centerFragment()
    val right  = rightFragment()
    // Assemble type fragments in "spiral rule" order
    val combined = LinkedList[AstNode]()
    combined.addAll(center)
    combined.addAll(right)
    combined.addAll(left)
    return combined

  def leftFragment (): LinkedList[AstNode] =
    println("FOUND LEFT_FRAGMENT")
    val fragment = LinkedList[AstNode]()
    while lookahead.kind == Token.Kind.ASTERISK do
      val n = pointerType()
      fragment.addFirst(n)
    return fragment

  def centerFragment (): LinkedList[AstNode] =
    println("FOUND CENTER_FRAGMENT")
    var fragment = LinkedList[AstNode]()
    if lookahead.kind == Token.Kind.CARAT then
      fragment.addLast(functionPointerType())
    else if lookahead.kind == Token.Kind.BOOL    ||
            lookahead.kind == Token.Kind.INT     ||
            lookahead.kind == Token.Kind.INT8    ||
            lookahead.kind == Token.Kind.INT16   ||
            lookahead.kind == Token.Kind.INT32   ||
            lookahead.kind == Token.Kind.INT64   ||
            lookahead.kind == Token.Kind.UINT    ||
            lookahead.kind == Token.Kind.UINT8   ||
            lookahead.kind == Token.Kind.UINT16  ||
            lookahead.kind == Token.Kind.UINT32  ||
            lookahead.kind == Token.Kind.UINT64  ||
            lookahead.kind == Token.Kind.FLOAT32 ||
            lookahead.kind == Token.Kind.FLOAT64 ||
            lookahead.kind == Token.Kind.VOID
    then
      fragment.addLast(primitiveType())
    else if lookahead.kind == Token.Kind.IDENTIFIER then
      // Nominal type. Need to look up name in symbol table to tell
      // what kind it is (e.g. struct, class). For now assume class.
      // What if we don't want to require a symbol table for
      // parsing? In this case, all we can say is that it is a
      // 'nominal' type. Update: This comment was from python
      // prototype - why is this required?
      fragment.addLast(nominalType())
    else if lookahead.kind == Token.Kind.L_PARENTHESIS then
      match_(Token.Kind.L_PARENTHESIS)
      val n = directType()
      fragment = n
      match_(Token.Kind.R_PARENTHESIS)
    return fragment

  def rightFragment (): LinkedList[AstNode] =
    println("FOUND RIGHT_FRAGMENT")
    val fragment = LinkedList[AstNode]()
    while lookahead.kind == Token.Kind.L_BRACKET do
      val n = arrayType()
      fragment.addLast(n)
    return fragment

  def arrayType (): AstNode =
    val n = AstNode(AstNode.Kind.ARRAY_TYPE)
    match_(Token.Kind.L_BRACKET)
    return n

  def functionPointerType (): AstNode =
    val n = AstNode(AstNode.Kind.FUNCTION_POINTER_TYPE)
    n.setToken(lookahead)
    match_(Token.Kind.CARAT)
    return n

  def nominalType (): AstNode =
    val n = AstNode(AstNode.Kind.NOMINAL_TYPE)
    // I don't think we want to add a name here, we just want to set
    // the token instead, but we can revisit this later.
    // n.add_child(self.name())
    n.setToken(lookahead)
    match_(Token.Kind.IDENTIFIER)
    // Need to eventually allow for type parameters. (This would allow
    // us to know that this was a class type, if that matters.)
    return n

  def pointerType (): AstNode =
    val n = AstNode(AstNode.Kind.POINTER_TYPE)
    n.setToken(lookahead)
    match_(Token.Kind.ASTERISK)
    return n

  def primitiveType (): AstNode =
    val n = AstNode(AstNode.Kind.PRIMITIVE_TYPE)
    n.setToken(lookahead)
    match_(lookahead.kind)
    return n

}
