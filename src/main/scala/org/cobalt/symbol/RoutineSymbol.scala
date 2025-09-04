package org.cobalt.symbol

import java.util.LinkedList

import org.cobalt.type_.*

class RoutineSymbol (name: String) extends Symbol (name: String) {

  private var parameters = LinkedList[RoutineParameterSymbol]()
  private var returnType: TypeNode = null

  def getParameter (index: Int): RoutineParameterSymbol =
    return parameters.get(index)

  def getReturnType (): TypeNode =
    return returnType

  def addParameter (parameter: RoutineParameterSymbol) =
    parameters.add(parameter)

  def setReturnType (returnType: TypeNode) =
    this.returnType = returnType

}
