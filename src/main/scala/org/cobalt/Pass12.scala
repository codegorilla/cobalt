package org.cobalt
import symbol.Scope

// This pass continues building the symbol table.

// We will presumably passing in the scope from a previous pass where types were
// defined.

class Pass12 (val input: AstNode, val scope: Scope) {

  // Need to traverse AST looking for variable declarations, and possibly
  // function declarations to build up symbol table.

  def process () =
    inspect(input)

  def inspect (current: AstNode): Unit =
    if current.getKind() == AstNode.Kind.ARRAY_TYPE then
      expressionRoot(current.getChild(0))
      inspect(current.getChild(1))
    else
      for child <- current.getChildren() do
        inspect(child)

  // TODO: We need to store the result here

  def expressionRoot (current: AstNode) =
    ;

}