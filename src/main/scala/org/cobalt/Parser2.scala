package org.cobalt

import org.cobalt.AstNode.Kind

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

  val symbolTable = SymbolTable()

  var input: List[Token] = null
  var position = 0
  var lookahead: Token = null

  def setInput (input: List[Token]) =
    this.input = input
    lookahead = input(position)

  def matchp (kind: Token.Kind) =
    if lookahead.kind == kind then
      consume()
    else
      print(s"invalid token: expected ${kind}, got ${lookahead.kind}")

  def consume () =
    position += 1
    lookahead = input(position)


  def translationUnit (): AstNode = {
    val n = AstNode(AstNode.Kind.TRANSLATION_UNIT)
    while lookahead.kind != Token.Kind.EOF do
      // Infinite loop, need to consume
      Thread.sleep(10)
      //println(lookahead)
      n.addChild(declaration())
    return n
  }

  def declaration (): AstNode = {

    if lookahead.kind == Token.Kind.CLASS then

      symbolTable.insert(new Symbol("hello"))
    // lookahead.kind match
    //   case Token.Kind.VAR =>
    //     println("Found var!")
    //   case _ =>
    //     println("Found something else!")

    return AstNode(AstNode.Kind.VAR)
  }

}
