package org.cobalt.symbol

// DEPRECATED, USE SYMBOL1 INSTEAD

class Symbol2 (kind: Symbol2.Kind, name: String) {

  def getKind (): Symbol2.Kind =
    return kind

  def getName (): String =
    return name

}

object Symbol2 {
  enum Kind {
    case CLASS
    case CLASS_TEMPLATE
    case METHOD
    case METHOD_TEMPLATE
    case MODULE
    case PACKAGE
    case PRIMITIVE_TYPE
    case ROUTINE
    case ROUTINE_TEMPLATE
    case VARIABLE
  }
}
