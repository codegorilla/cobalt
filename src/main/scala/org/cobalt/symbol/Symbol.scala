package org.cobalt.symbol

class Symbol (kind: Symbol.Kind, name: String) {

  def getKind (): Symbol.Kind =
    return kind

  def getName (): String =
    return name

}

object Symbol {
  enum Kind {
    case CLASS_TYPE
    case PRIMITIVE_TYPE
    case ROUTINE
    case VARIABLE
  }
}
