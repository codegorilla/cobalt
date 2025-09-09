package org.cobalt.type_

class PointerTypeNode extends TypeNode {

  private var baseType: TypeNode = null

  def getBaseType (): TypeNode =
    return baseType

  def setBaseType (baseType: TypeNode) =
    this.baseType = baseType

}
