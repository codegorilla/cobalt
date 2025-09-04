package org.cobalt.symbol

import java.util.LinkedList

import org.cobalt.type_.*

// We don't necessarily want to hold all of these items inside of lists, but
// rather give the class symbol its own scope.

class ClassSymbol (name: String) extends Symbol (name: String) {

  private var memberRoutines = LinkedList[RoutineSymbol]()
  private var memberVariables = LinkedList[VariableSymbol]()

  def getMemberRoutine (index: Int): RoutineSymbol =
    return memberRoutines.get(index)

  def getMemberVariable (index: Int): VariableSymbol =
    return memberVariables.get(index)

  def addMemberRoutine (routine: RoutineSymbol) =
    memberRoutines.add(routine)

  def addMemberVariable (variable: VariableSymbol) =
    memberVariables.add(variable)
}
