package org.cobalt.symbol

import org.cobalt.type_.*

// Parameter symbols are not held directly in the symbol table. They are just
// referenced by routine symbols, which are held directly in the symbol table.

// We might not need to consider these to be symbols, in which case we may
// adjust the class name and definition. But for now, just consider them to be
// symbols.

class RoutineParameterSymbol (name: String) extends Symbol (name: String) {

  private var type_ : TypeNode = null

  def getType (): TypeNode =
    return type_

  def setType (type_ : TypeNode) =
    this.type_ = type_

}
