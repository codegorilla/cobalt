package org.cobalt

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

class AstNode (kind: AstNode.Kind) {

  var children = ListBuffer[AstNode]()
  var token: Token = null
  val attributes = Map[String, Any]()

  def addChild (node: AstNode) =
    children += node

  def setAttribute (name: String, value: Any) =
    attributes += (name -> value)

  def setToken (token: Token) =
    this.token = token
}

object AstNode {
  enum Kind {
    case PLACEHOLDER

    case TRANSLATION_UNIT
    case CLASS_DECLARATION
    case DEF
    case IDENTIFIER
    case MODIFIERS
    case PUBLIC
    case VARIABLE_DECLARATION
  }
}
