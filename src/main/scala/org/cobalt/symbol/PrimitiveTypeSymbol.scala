package org.cobalt.symbol

// Primitive types can also be symbols. Should we use traits or multiple
// inheritance to model this, or just composition? Or just leave symbols and
// types completely separate and "link" them based on common text
// representations?

class PrimitiveTypeSymbol (name: String) extends Symbol (name: String) {}
