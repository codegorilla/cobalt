package org.cobalt.type_

class ArrayTypeNode extends TypeNode {

  private var baseType: TypeNode = null
  private var size: Int = 0

  def getBaseType (): TypeNode =
    return baseType

  def getSize (): Int =
    return size

  def setBaseType (baseType: TypeNode) =
    this.baseType = baseType

  def setSize (size: Int) =
    this.size = size

}
