package org.cobalt

class Scope (private var kind: Scope.Kind) {

  val symbolTable = SymbolTable()
  var enclosingScope: Scope = null

  // Should caller provide name and kind, or already constructed symbol?

  def define (symbol: Symbol) =
    symbolTable.insert(symbol)

  def resolve (name: String, recurse: Boolean = true): Symbol =
    // Recurse through scope stack, looking for symbol
    var symbol = symbolTable.lookup(name)
    if symbol == null then
      if recurse && enclosingScope != null then
        symbol = enclosingScope.resolve(name)
    return symbol

  def getEnclosingScope (): Scope =
    enclosingScope

  def setEnclosingScope (scope: Scope) =
    enclosingScope = scope

  def getKind (): Scope.Kind =
    return kind
}

object Scope {
  enum Kind {
    case BUILT_IN
    case CLASS
    case GLOBAL
    case LOCAL
  }
}
