package org.cobalt.symbol

import org.cobalt.type_.*

class VariableSymbol (name: String) extends Symbol (name: String) {

  private var type_ : TypeNode = null

  def getType (): TypeNode =
    return type_

  def setType (type_ : TypeNode) =
    this.type_ = type_

}
