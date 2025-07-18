package org.cobalt

import symbol.Symbol
import symbol.Scope

// The purpose of this pass is to process type declarations.

// This should occur fairly early because we need to know what types exist for
// later passes. One difficulty is that we don't know anything about types
// declared outside of the current file. That is where a study of modules,
// linking, etc. are needed.

// We need to start creating a symbol table. This symbol table is accessed in
// multiple passes, so we need a way to retrieve it. Or it can be created
// externally and passed in.

// We also need to decide if we will re-use the same symbol table created for
// parsing or if that should be isolated to the parsing phase and discarded. The
// answer depends on whether the content of that other symbol table is useful
// and can be built-upon, or if it is useless past the parsing stage.

class Pass10 (val input: AstNode) {

  val builtinScope = Scope(Scope.Kind.BUILT_IN)
  var currentScope = builtinScope
  definePrimitiveTypes()

  // Used to pass nodes up and down during tree traversal
  // val stack = Stack[AstNode]()

  def definePrimitiveTypes () =
    // To do: Types need to be made up of type trees
    //builtinScope.define(TypeSymbol("bool", TypeNode(TypeNode.Kind.PRIMITIVE_TYPE)))
    ;

  def process (): Scope =
    translationUnit(input)
    // Return built-in scope?
    return builtinScope

  def translationUnit (current: AstNode) =
    for child <- current.getChildren() do
      if child.getKind() == AstNode.Kind.CLASS_DECLARATION then
        classDeclaration(child)

  def classDeclaration (current: AstNode) =
    name(current.getChild(1))

  def name (current: AstNode) =
    // Add to symbol table as a type
    // Might need to be more specific, e.g. CLASS type vs. STRUCT type
    //val symbol = new Symbol(current.getToken().lexeme)
    //currentScope.define(symbol)
    ;
}
