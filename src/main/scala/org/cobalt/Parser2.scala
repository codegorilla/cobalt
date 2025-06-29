package org.cobalt

import org.cobalt.AstNode.Kind

import scala.collection.mutable.ArrayDeque
import scala.collection.mutable.Map

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
      Thread.sleep(100)
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
  def variableDeclarationFinal (modifiers: AstNode): AstNode =
    modifiers.addChild(finalModifier())
    val n = AstNode(AstNode.Kind.VARIABLE_DECLARATION)
    matchp(Token.Kind.VAL)
    n.addChild(modifiers)
    n.addChild(identifier())
    matchp(Token.Kind.COLON)
    n.addChild(typeRoot())
    matchp(Token.Kind.SEMICOLON)
    return n

  // To do: Implement type inference
  def variableDeclaration (modifiers: AstNode, finalFlag: Boolean): AstNode =
    val n = AstNode(AstNode.Kind.VARIABLE_DECLARATION)
    matchp(Token.Kind.VAR)
    n.addChild(modifiers)
    n.addChild(identifier())
    if lookahead.kind == Token.Kind.COLON then
      matchp(Token.Kind.COLON)
      n.addChild(typeRoot())
    if lookahead.kind == Token.Kind.EQUAL then
      matchp(Token.Kind.EQUAL)
      n.addChild(expression())
    matchp(Token.Kind.SEMICOLON)
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

    // val n = lookahead.kind match
    //   case Token.Kind.INT =>
    //   case Token.Kind.FLOAT =>
    //     ;
    //   case _ =>
    //     println("Found something else!")
    // AstNode(AstNode.Kind.PLACEHOLDER)

  def type_ (): AstNode =
    val combined = directType()
    // Pop base type node
    null

  def directType (): AstNode =
    // Build left fragment
    val left = ArrayDeque[AstNode]()
    // while lookahead.kind == Token.Kind.ASTERISK do
    //   val n = pointerType()
//      left.appendLeft(n)
    // Build center fragment
    null

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
