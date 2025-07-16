package org.cobalt.symbol

import org.cobalt.TypeNode

class TypeSymbol (name: String, var type_ : TypeNode = null) extends Symbol (name) {

  def setType (type_ : TypeNode) =
    this.type_ = type_

}
