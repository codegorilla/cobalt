package org.cobalt

class Symbol (kind: Symbol.Kind, name: String) {

  def getKind (): Symbol.Kind =
    return kind

  def getName (): String =
    return name

}

object Symbol {
  enum Kind:
    case FUNCTION
    case TYPE
    case VARIABLE
}
