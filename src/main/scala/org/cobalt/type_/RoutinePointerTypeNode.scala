package org.cobalt.type_

import scala.collection.mutable.ListBuffer

class RoutinePointerTypeNode {

  private var parameterTypes = ListBuffer[TypeNode]()
  private var returnType: TypeNode = null

  def getParameterType (index: Int): TypeNode =
    return parameterTypes(index)

  def getReturnType (): TypeNode =
    return returnType

  def addParameterType (parameterType: TypeNode) =
    parameterTypes += parameterType

  def setReturnType (returnType: TypeNode) =
    this.returnType = returnType

}
