package org.cobalt

// Might not use this

case class Modifier (val kind: Modifier.Kind)

object Modifier {
  enum Kind {
    case ABSTRACT
    case PUBLIC
    case STATIC
  }
}
