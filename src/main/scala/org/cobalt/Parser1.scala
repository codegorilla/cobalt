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

// First pass parser just looks for the pattern 'class Name [', which
// indicates a class template. These get added to the symbol table so
// that they can serve as semantic predicates in later parsing.

class Parser1 {
  
  val symbolTable = SymbolTable()

  var input: List[Token] = null
  var position = 0
  var lookahead: Token = null

  def setInput (input: List[Token]) =
    this.input = input
    lookahead = input(position)

  def pmatch (kind: Token.Kind) =
    if lookahead.kind == kind then
      consume()
    else
      print(s"invalid token: expected ${kind}, got ${lookahead.kind}")

  def consume () =
    position += 1
    lookahead = input(position)

  def process (): SymbolTable =
    translationUnit()
    return symbolTable

  def translationUnit () =
    while lookahead.kind != Token.Kind.EOF do
      // Infinite loop
      Thread.sleep(10)
      declaration()

  def declaration () =
    while lookahead.kind != Token.Kind.CLASS && lookahead.kind != Token.Kind.EOF do
      consume()
    if lookahead.kind == Token.Kind.CLASS then
      classDeclaration()

  def classDeclaration () =
    // A lookahead of 2 would be helpful here because it would avoid
    // saving the name before we know if its even needed. This is
    // because we only need to create the symbol table entry if this
    // is a class template.
    pmatch(Token.Kind.CLASS)
    if lookahead.kind == Token.Kind.IDENTIFIER then
      val name = identifier()
      if lookahead.kind == Token.Kind.L_BRACKET then
        pmatch(Token.Kind.L_BRACKET)
        symbolTable.insert(new Symbol(name))

  def identifier (): String =
    val name = lookahead.lexeme
    pmatch(Token.Kind.IDENTIFIER)
    return name
}
