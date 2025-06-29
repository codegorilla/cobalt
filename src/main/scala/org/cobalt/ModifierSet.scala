package org.cobalt

class ModifierSet {

  var modifiers = List[Modifier]()

  def add (modifier: Modifier) =
    modifiers = modifier :: modifiers
}
