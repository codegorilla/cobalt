package org.cobalt.symbol

import scala.collection.mutable.Map

class SymbolTable {

  val data = Map[String, Symbol]()

  def insert (symbol: Symbol) =
    data += (symbol.getName() -> symbol)

  def lookup (name: String) =
    data.getOrElse(name, null)

}
