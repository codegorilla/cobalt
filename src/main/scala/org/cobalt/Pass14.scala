package org.cobalt
import symbol.SymbolTable

// The purpose of this pass is to process type specifiers on global variable
// declarations.

// Processing types on local variable declarations will either be added to this
// pass later, or will be put into its own pass. Since global variable
// declarations may occur out of order, even after the functions that reference
// them, it makes sense to fully process them first.

// This has to occur after processing of type declarations. We need to do
// several things. First, we need to establish the types of variables if
// specified. If not specified, then we need to compute the types. Finally, we
// need to perform type checking. Not all of this will necessarily happen in
// this pass.

// We need a pass before this where we build a symbol table tree.

class Pass14 (val input: AstNode, val symtab: SymbolTable) {

  def process () =
    translationUnit(input)

  // We could just do a search for global variable declarations, but they are
  // know to always occur at the top scope, so that might not be worth it.

  def translationUnit (current: AstNode) =
    for child <- current.getChildren() do
      if child.getKind() == AstNode.Kind.VARIABLE_DECLARATION then
        globalVariableDeclaration(child)

  def globalVariableDeclaration (current: AstNode) =
    typeSpecifier(current.getChild(2))

  def typeSpecifier (current: AstNode) =
    // If the type specifier has no children then it means no type was specified
    // in the declaration and the type must be inferred
    if current.getChildCount() > 0 then
      // Type inference is NOT required - we just need to compute the type
      typeRoot(current.getChild(0))

  // Not sure we need a type root, we can remove it if it isn't required.

  def typeRoot (current: AstNode) =
    type_(current.getChild(0))

  def type_ (current: AstNode): TypeNode =
    val t = current.getKind() match
      case AstNode.Kind.ARRAY_TYPE => arrayType(current)
      case AstNode.Kind.NOMINAL_TYPE => nominalType(current)
      case AstNode.Kind.POINTER_TYPE => pointerType(current)
      case AstNode.Kind.PRIMITIVE_TYPE => primitiveType(current)
      case _ => println(s"error: not possible without bug in type_()")
        TypeNode(TypeNode.Kind.ERROR)
    println(s"Type node is ${t}")
    return t

  // The array size must be a compile-time computable expression, which may be
  // a simple integer literal, or a complex arithmetic expression that includes
  // identifiers (variable references). Size is not part of the array type.

  def arrayType (current: AstNode): TypeNode =
    val t = TypeNode(TypeNode.Kind.ARRAY_TYPE)
    t.addChild(type_(current.getChild(1)))
    return t

  // Nominal types definitely belong in the symbol table

  def nominalType (current: AstNode): TypeNode =
    val t = TypeNode(TypeNode.Kind.NOMINAL_TYPE)
    return t

  def pointerType (current: AstNode): TypeNode =
    val t = TypeNode(TypeNode.Kind.POINTER_TYPE)
    t.addChild(type_(current.getChild(0)))
    return t

  // Should we have separate kinds for INT, FLOAT, etc. or just rely on token
  // or some other value? Should primitive types be in the symbol table?

  // I am leaning towards having separate primitive types, just to make things
  // very simple and uniform in later processing.

  def primitiveType (current: AstNode): TypeNode =
    val t = TypeNode(TypeNode.Kind.PRIMITIVE_TYPE)
    return t


  // def name (current: AstNode) =
  //   // Add to symbol table as a type
  //   // Might need to be more specific, e.g. CLASS type vs. STRUCT type
  //   val symbol = new Symbol(Symbol.Kind.TYPE, current.token.lexeme)
  //   symtab.insert(symbol)
}
