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
    matchp(kind)

  def matchp (kind: Token.Kind) =
    if lookahead.kind == kind then
      consume()
    else
      print(s"invalid token: expected ${kind}, got ${lookahead.kind}")

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
      case Token.Kind.CLASS => classDeclaration()
      case Token.Kind.VAL => variableDeclaration(p, true)
      case Token.Kind.VAR => variableDeclaration(p, false)
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
    matchp(Token.Kind.PUBLIC)
    AstNode(AstNode.Kind.PUBLIC_MODIFIER)

  def staticModifier (): AstNode =
    matchp(Token.Kind.STATIC)
    AstNode(AstNode.Kind.STATIC_MODIFIER)

  def classDeclaration (): AstNode =
    val n = AstNode(AstNode.Kind.CLASS_DECLARATION)
    matchp(Token.Kind.CLASS)
    n.addChild(identifier())
    matchp(Token.Kind.L_BRACE)
    while lookahead.kind != Token.Kind.R_BRACE do
      n.addChild(classMember())
    matchp(Token.Kind.R_BRACE)
    return n

  def classMember (): AstNode =
    // To do
    return AstNode(AstNode.Kind.PLACEHOLDER)

  // To do: Implement type inference
  // def variableDeclarationFinal (modifiers: AstNode): AstNode =
  //   modifiers.addChild(finalModifier())
  //   val n = AstNode(AstNode.Kind.VARIABLE_DECLARATION)
  //   matchp(Token.Kind.VAL)
  //   n.addChild(modifiers)
  //   n.addChild(identifier())
  //   matchp(Token.Kind.COLON)
  //   n.addChild(typeRoot())
  //   matchp(Token.Kind.SEMICOLON)
  //   return n

  // To do: Implement type inference
  def variableDeclaration (modifiers: AstNode, finalFlag: Boolean): AstNode =
    val n = AstNode(AstNode.Kind.VARIABLE_DECLARATION)
    if finalFlag then
      match_(Token.Kind.VAL)
      modifiers.addChild(finalModifier())
    else
      match_(Token.Kind.VAR)
    n.addChild(modifiers)
    n.addChild(identifier())
    if lookahead.kind == Token.Kind.COLON then
      match_(Token.Kind.COLON)
      n.addChild(typeRoot())
    if lookahead.kind == Token.Kind.EQUAL then
      match_(Token.Kind.EQUAL)
      n.addChild(expression())
    match_(Token.Kind.SEMICOLON)
    return n

  def identifier (): AstNode =
    val n = AstNode(AstNode.Kind.IDENTIFIER)
    n.setToken(lookahead)
    matchp(Token.Kind.IDENTIFIER)
    return n

    // Expressions

  def expression (): AstNode =
    return null

  // Types

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
    // Build fragments
    val left   = leftFragment()
    val center = centerFragment()
    val right  = rightFragment()
    // Assemble fragments
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
