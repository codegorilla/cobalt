package org.cobalt

// The purpose of this pass is to process variable declarations.

// This has to occur after processing of type declarations. We need to do
// several things. First, we need to establish the types of variables if
// specified. If not specified, then we need to compute the types. Finally, we
// need to perform type checking. Not all of this will necessarily happen in
// this pass.

class Pass12 (val input: AstNode, val symtab: SymbolTable) {

  // Used to pass nodes up and down during tree traversal
  // val stack = Stack[AstNode]()

  def process () =
    translationUnit(input)

  def translationUnit (current: AstNode) =
    for child <- current.getChildren() do
      if child.getKind() == AstNode.Kind.CLASS_DECLARATION then
        classDeclaration(child)

  def classDeclaration (current: AstNode) =
    name(current.getChild(1))

  def name (current: AstNode) =
    // Add to symbol table as a type
    // Might need to be more specific, e.g. CLASS type vs. STRUCT type
    val symbol = new Symbol(Symbol.Kind.TYPE, current.token.lexeme)
    symtab.insert(symbol)
}
