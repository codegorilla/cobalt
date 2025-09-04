package org.cobalt.type_

// We may need to use separate derived types for classes, enums, etc. For now,
// try to cover all of these with a generically named "nominal type".

class NominalTypeNode extends TypeNode {

  private var name: String = null
  private var kind: NominalTypeNode.Kind = null

  def getKind (): NominalTypeNode.Kind =
    return kind

  def getName (): String =
    return name

  def setKind (kind: NominalTypeNode.Kind) =
    this.kind = kind

  def setName (name: String) =
    this.name = name

}

object NominalTypeNode:
  enum Kind:
    case CLASS
    case ENUMERATION
