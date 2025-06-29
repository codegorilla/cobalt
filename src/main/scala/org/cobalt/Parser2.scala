package org.cobalt

import org.cobalt.AstNode.Kind

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
      Thread.sleep(10)
      n.addChild(declaration())
    return n

  def declaration (): AstNode =
    // Deal with modifiers first
    var n: AstNode = null
    if lookahead.kind == Token.Kind.PUBLIC ||
       lookahead.kind == Token.Kind.STATIC
    then
      n = modifiers()

    val p = lookahead.kind match
      case Token.Kind.CLASS =>
        classDeclaration()
      case Token.Kind.VAL =>
        variableDeclarationFinal()
      case Token.Kind.VAR =>
        variableDeclaration(n)
      case _ =>
        println("Found something else!")
        AstNode(AstNode.Kind.PLACEHOLDER)
    return p

  val modifierAttributes = Map[AstNode, ModifierSet]()

  val modifierMap = Map[Token.Kind, Modifier.Kind](
    Token.Kind.PUBLIC -> Modifier.Kind.PUBLIC,
    Token.Kind.STATIC -> Modifier.Kind.STATIC,
  )

  def modifiers (): AstNode =
    val n = AstNode(AstNode.Kind.MODIFIERS)
    val ms = ModifierSet()
    while lookahead.kind == Token.Kind.PUBLIC ||
          lookahead.kind == Token.Kind.STATIC
    do
      ms.add(Modifier(modifierMap(lookahead.kind)))
    modifierAttributes(n) = ms
    return n

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
  def variableDeclarationFinal (): AstNode =
    val n = AstNode(AstNode.Kind.VARIABLE_DECLARATION)
    matchp(Token.Kind.VAL)
    n.setAttribute("final", true)
    n.addChild(identifier())
    matchp(Token.Kind.COLON)
    n.addChild(typeExpression())
    matchp(Token.Kind.SEMICOLON)
    return n

  // To do: Implement type inference
  def variableDeclaration (modifiersNode: AstNode): AstNode =
    val n = AstNode(AstNode.Kind.VARIABLE_DECLARATION)
    matchp(Token.Kind.VAR)
    n.addChild(identifier())
    matchp(Token.Kind.COLON)
    n.addChild(typeExpression())
    matchp(Token.Kind.SEMICOLON)
    return n

  def identifier (): AstNode =
    val n = AstNode(AstNode.Kind.IDENTIFIER)
    n.setToken(lookahead)
    matchp(Token.Kind.IDENTIFIER)
    return n

  // Types

  def typeExpression (): AstNode =
    val n = lookahead.kind match
      case Token.Kind.INT =>
        ;
      case Token.Kind.FLOAT =>
        ;
      case _ =>
        println("Found something else!")
    AstNode(AstNode.Kind.PLACEHOLDER)

}
