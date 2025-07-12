package org.cobalt

// Might not use this

class ModifierSet {

  var modifiers = List[Modifier]()

  def add (modifier: Modifier) =
    modifiers = modifier :: modifiers
}
