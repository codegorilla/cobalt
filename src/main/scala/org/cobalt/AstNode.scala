package org.cobalt

import scala.collection.mutable.ListBuffer

class AstNode (kind: AstNode.Kind) {

  var children = ListBuffer[AstNode]()

  def addChild (node: AstNode) = {
    children += node
  }
}

object AstNode {
    enum Kind {
        case TRANSLATION_UNIT
        case CLASS
        case DEF
        case VAL
        case VAR
    }
}
