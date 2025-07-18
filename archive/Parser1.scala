package org.cobalt

import org.cobalt.symbol.Scope
import org.cobalt.symbol.Symbol

// This first parser just needs to create the symbol table so we can
// start populating it with template classes so that we know if
// certain productions in a later parsing pass are classes or not. We
// also need to be able to follow typealiases to their target types.

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
  
  val scope = Scope(Scope.Kind.BUILT_IN)

  var input: List[Token] = null
  var position = 0
  var lookahead: Token = null

  def setInput (input: List[Token]) =
    this.input = input
    lookahead = input(position)

  def match_ (kind: Token.Kind) =
    if lookahead.kind == kind then
      consume()
    else
      print(s"invalid token: expected ${kind}, got ${lookahead.kind}")

  def consume () =
    position += 1
    lookahead = input(position)

  def process (): Scope =
    translationUnit()
    return scope

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
    match_ (Token.Kind.CLASS)
    name()
    // if lookahead.kind == Token.Kind.IDENTIFIER then
    //   val name = identifier()
    //   if lookahead.kind == Token.Kind.L_BRACKET then
    //     match_ (Token.Kind.L_BRACKET)
    //     scope.define()

  def name () = 
    val s = new Symbol(Symbol.Kind.CLASS_TYPE, lookahead.lexeme)
    scope.define(s)
    // TODO: We may wish to link the AST node to the symbol table entry somehow
    match_ (Token.Kind.IDENTIFIER)
}
