package org.cobalt

// We can probably get rid of symbol kind enum because it is already encoded in
// the class hierarchy.

class TypeSymbol (kind: Symbol.Kind, name: String, var type_ : TypeNode = null) extends Symbol (kind, name) {
  
  def setType (type_ : TypeNode) =
    this.type_ = type_
}
