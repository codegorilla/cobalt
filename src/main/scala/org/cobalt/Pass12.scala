package org.cobalt

// The purpose of this pass is to process type specifiers on variable
// declarations.

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
      if child.getKind() == AstNode.Kind.VARIABLE_DECLARATION then
        println(current.getKind())
        println(current.getToken())
        //globalVariableDeclaration(child)

  def globalVariableDeclaration (current: AstNode) =
    typeSpecifier(current.getChild(2))

  def typeSpecifier (current: AstNode) =
    println(current.getKind())
    // If the type specifier has no children then it means no type was specified
    // in the declaration and type inference is needed
    if current.getChildCount() > 0 then
      // Type inference is NOT required - we just need to compute the type
      typeRoot(current.getChild(0))
      println(current.getToken())
  
  def typeRoot (current: AstNode) =
    println(current.getToken())


  // def name (current: AstNode) =
  //   // Add to symbol table as a type
  //   // Might need to be more specific, e.g. CLASS type vs. STRUCT type
  //   val symbol = new Symbol(Symbol.Kind.TYPE, current.token.lexeme)
  //   symtab.insert(symbol)
}
