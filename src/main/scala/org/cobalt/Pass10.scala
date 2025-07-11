package org.cobalt

// The purpose of this pass is to process type declarations

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

  val symbolTable = SymbolTable()

  // Used to pass nodes up and down during tree traversal
  // val stack = Stack[AstNode]()

  def process (): SymbolTable =
    translationUnit(input)
    return symbolTable

  def translationUnit (current: AstNode) =
    ;
    // for child <- current.getChildren() do
    //   if child.getKind() == AstNode.Kind.VARIABLE_DECLARATION then
    //     globalVariableDeclaration(child)
    //   else if child.getKind() == AstNode.Kind.FUNCTION_DECLARATION then
    //     functionDeclaration(child)


}