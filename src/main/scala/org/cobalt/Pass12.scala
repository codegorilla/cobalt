package org.cobalt

// The purpose of this pass is to process type specifiers on variable
// declarations.

// This has to occur after processing of type declarations. We need to do
// several things. First, we need to establish the types of variables if
// specified. If not specified, then we need to compute the types. Finally, we
// need to perform type checking. Not all of this will necessarily happen in
// this pass.

class Pass12 (val input: AstNode, val symtab: SymbolTable) {

  def process () =
    translationUnit(input)

  def translationUnit (current: AstNode) =
    for child <- current.getChildren() do
      if child.getKind() == AstNode.Kind.VARIABLE_DECLARATION then
        globalVariableDeclaration(child)

  def globalVariableDeclaration (current: AstNode) =
    typeSpecifier(current.getChild(2))

  def typeSpecifier (current: AstNode) =
    println(current.getKind())
    // If the type specifier has no children then it means no type was specified
    // in the declaration and type inference is needed
    if current.getChildCount() > 0 then
      // Type inference is NOT required - we just need to compute the type
      typeRoot(current.getChild(0))

  // Not sure we need a type root, we can remove it if it isn't required.

  def typeRoot (current: AstNode) =
    type_(current.getChild(0))

  def type_ (current: AstNode): TypeNode =
    val t = current.getKind() match
      case AstNode.Kind.NOMINAL_TYPE => nominalType(current)
      case AstNode.Kind.POINTER_TYPE => pointerType(current)
      case AstNode.Kind.PRIMITIVE_TYPE => primitiveType(current)
      case _ => println(s"error: not implemented yet (type_)")
        TypeNode(TypeNode.Kind.ERROR)
    println(s"Type node is ${t}")
    return t

  // The array size can be any compile-time computable expression, which may be
  // as simple as an integer literal, but may be complex arithmetic expressions
  // that include identifiers (variable references). In all cases, they are
  // considered type nodes because they are part of the "type expression". We
  // don't necessarily need to validate that the expression is a compile-time
  // constant here. It can be done in a later pass.
  // var x: int[size + 2];

  def arrayType (current: AstNode): TypeNode =
    val t = TypeNode(TypeNode.Kind.ARRAY_TYPE)
    // Need to add size as one child
    t.addChild(arraySizeExpression(current.getChild(0)))
    t.addChild(type_(current.getChild(1)))
    return t

  def arraySizeExpression (current: AstNode): TypeNode =
    val t = TypeNode(TypeNode.Kind.ARRAY_SIZE)
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

  def primitiveType (current: AstNode): TypeNode =
    val t = TypeNode(TypeNode.Kind.PRIMITIVE_TYPE)
    return t


  // def name (current: AstNode) =
  //   // Add to symbol table as a type
  //   // Might need to be more specific, e.g. CLASS type vs. STRUCT type
  //   val symbol = new Symbol(Symbol.Kind.TYPE, current.token.lexeme)
  //   symtab.insert(symbol)
}
