package org.cobalt.type_

// C/C++ use the term "fundamental type", whereas Java and some other languages
// use the term "primitive type". These words are more or less interchangeable.
// We will use the later term for now, but we may reconsider in the future.
// Primitive is a bit shorter, but takes us out of alignment with C++.

class PrimitiveTypeNode extends TypeNode {

  private var kind: PrimitiveTypeNode.Kind = null
  var num: Int = 0

  def getKind (): PrimitiveTypeNode.Kind =
    return kind

  def setKind (kind: PrimitiveTypeNode.Kind) =
    this.kind = kind

  def getNum (): Int =
    return num

  def setNum (num: Int) =
    this.num = num

}

// Need to double-check if void is considered a primitive type. I believe it is,
// but is also categorized as an "incomplete type".

object PrimitiveTypeNode:
  enum Kind:
    case BOOL
    case BYTE
    case SHORT
    case INT
    case LONG
    case INT8
    case INT16
    case INT32
    case INT64
    case UINT8
    case UINT16
    case UINT32
    case UINT64
    case FLOAT32
    case FLOAT64
    case VOID
