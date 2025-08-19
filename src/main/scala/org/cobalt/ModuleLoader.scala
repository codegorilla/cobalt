package org.cobalt

// The module loader is responsible for identifying all source files in the
// module and instantiating a file loader to process each one. It then assembles
// the AST from each file loader into a complete AST for the entire module.
// Thus, the combination of all source files constitues a "translation unit".

class ModuleLoader {

  def process (): AstNode =
    val tree = translationUnit()
    return tree

  def translationUnit (): AstNode =
    val n = AstNode(AstNode.Kind.TRANSLATION_UNIT)
    // We need to loop through each source file and process them. They will then
    // get combined into a single translation unit.
    // We could process each file separately in parallel, but we want to keep
    // things simple for now.
    val fileLoader = FileLoader()
    val p = fileLoader.process()
    for child <- p.getChildren() do
      n.addChild(child)
    return n
}
