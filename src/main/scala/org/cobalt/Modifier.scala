package org.cobalt

// Might not use this

case class Modifier (val kind: Modifier.Kind)

object Modifier {
  enum Kind {
    case PUBLIC
    case STATIC
  }
}
