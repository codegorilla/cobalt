package org.cobalt

// The purpose of this pass is to process array size expressions.

// Array size expressions must meet the following criteria:
// - All nodes must evaluate to integer types
// - All nodes must be constants (literals or const expressions)

// We might need to handle these as two separate passes. If so, an optimization
// is to do three passes -- one to gather lists of indices to array type nodes,
// and the other two to go through the lists without having to re-traverse the
// AST.

// For now, lets just work with literal values, so we avoid needing a symbol
// table.

class Pass12 (val input: AstNode, val symtab: SymbolTable) {

  // Need to recurse through tree looking for array size expressions. These
  // always appear under AST nodes of kind AstNode.Kind.ARRAY_TYPE, so search
  // for those. Only array type nodes under variable declarations have sizes.
  // Other locations, such as routine parameters omit the sizes.

  // We may want to limit the search to only locations where type expressions
  // can appear. To do so, we need to come up with a list of all nodes that may
  // appear in the chain from the array type node to the root. Then, when
  // traversing, we only continue down sub-trees whose node type is of one of
  // those types. For now, we won't worry about that optimization.

  def process () =
    inspect(input)

  def inspect (current: AstNode): Unit =
    if current.getKind() == AstNode.Kind.ARRAY_TYPE then
      expressionRoot(current.getChild(0))
      inspect(current.getChild(1))
    else
      for child <- current.getChildren() do
        inspect(child)

  def expressionRoot (current: AstNode) =
    val isIntegral = expression(current.getChild(0))
    print(isIntegral)

  // Other expressions we need to match against include postfix expressions,
  // such as names, routine calls, member access, and array subscripts. We need
  // symbol table information for those cases.

  def expression (current: AstNode): Boolean =
    current.getKind() match
      case AstNode.Kind.BINARY_EXPRESSION        => binaryExpression(current)
      case AstNode.Kind.UNARY_EXPRESSION         => unaryExpression(current)
      case AstNode.Kind.INTEGER_LITERAL          => integerLiteral(current)
      case AstNode.Kind.UNSIGNED_INTEGER_LITERAL => unsignedIntegerLiteral(current)
      case _ => false

  def binaryExpression (current: AstNode): Boolean =
    expression(current.getChild(0)) && expression(current.getChild(1))

  def unaryExpression (current: AstNode): Boolean =
    expression(current.getChild(0))

  def integerLiteral (current: AstNode): Boolean =
    true

  def unsignedIntegerLiteral (curren: AstNode): Boolean =
    true
}
