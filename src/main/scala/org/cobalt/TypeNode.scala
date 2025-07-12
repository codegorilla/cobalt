package org.cobalt

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

class TypeNode (private var kind: TypeNode.Kind):

  var children = ListBuffer[TypeNode]()

  def addChild (node: TypeNode) =
    children += node

  def getChild (index: Int): TypeNode =
    return children(index)

  def getChildCount (): Int =
    return children.size

  def getChildren (): Iterator[TypeNode] =
    return children.iterator

  def getKind (): TypeNode.Kind =
    return kind

  // Not sure we'll ever need to mutate kind in place. If not, then we can
  // remove the setter and make the the field immutable.

  def setKind (kind: TypeNode.Kind) =
    this.kind = kind

  // def getToken (): Token =
  //   return token

  // // Also not sure if we'll ever need to mutate this in place. It depends on
  // // whether or not token is always known at time of AstNode creation.

  // def setToken (token: Token) =
  //   this.token = token

  override def toString (): String =
    return s"TypeNode(${kind})"


object TypeNode:
  enum Kind:
    case ERROR
    case ARRAY_SIZE
    case ARRAY_TYPE
    case FUNCTION_POINTER_TYPE
    case NOMINAL_TYPE
    case POINTER_TYPE
    case PRIMITIVE_TYPE
