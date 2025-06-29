package org.cobalt

case class Modifier (val kind: Modifier.Kind)

object Modifier {
  enum Kind {
    case PUBLIC
    case STATIC
  }
}
