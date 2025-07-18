package org.cobalt.symbol

import org.cobalt.TypeNode

class TypeSymbol (name: String, private var type_ : TypeNode = null) extends Symbol (name) {

  def getType (): TypeNode =
    return type_

  def setType (type_ : TypeNode) =
    this.type_ = type_

}
