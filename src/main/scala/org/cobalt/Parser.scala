package org.cobalt

import scala.collection.mutable.Queue
import scala.collection.mutable.Stack

import symbol.Symbol
import symbol.Scope

// Thie parser needs to create a symbol table so we know if certain
// productions are classes or not. We also need to be able to follow
// typealiases to their target types.

// We need to be able to tell if X[Y].z() is a template instantiation
// or an array subscript operation. To do that, we need to know if X
// is a class or not. If it is a class, then this must be an attempt
// to instantiate a template because types cannot be subscripted --
// only values can. If it is not a class, then it must be an attempt
// to perform a subscript operation. (Similar issue arises with
// List<int>, so switching to that syntax doesn't help.)

// First pass parser just looks for classes

class Parser {

  val SLEEP_TIME = 10

  var input: List[Token] = null
  var position = 0
  var lookahead: Token = null

  // Used to pass nodes up and down during tree traversal
  val stack = Stack[AstNode]()

  // Used for symbol table operations. Cobalt requires a symbol table during
  // parsing in order to disambiguate a few grammar rules. We cannot wait until
  // the semantic analysis phase to begin constructing symbol tables.
  val builtinScope = Scope(Scope.Kind.BUILT_IN)
  var currentScope = builtinScope

  // Todo: we may also need a 'null_t' type, for which there is exactly one
  // value, which is 'null'. This is to match the C++ 'nullptr_t' type and its
  // corresponding single 'nullptr' value. I am not sure if this is a primitive
  // type or not. Needs research.

  // Todo: We may decide that 'int', 'short', 'float', etc. should just be
  // typealiases for the various fixed size types.

  def definePrimitiveTypes () =
    builtinScope.define(Symbol(Symbol.Kind.PRIMITIVE_TYPE, "int"))
    builtinScope.define(Symbol(Symbol.Kind.PRIMITIVE_TYPE, "int8"))
    builtinScope.define(Symbol(Symbol.Kind.PRIMITIVE_TYPE, "int16"))
    builtinScope.define(Symbol(Symbol.Kind.PRIMITIVE_TYPE, "int32"))
    builtinScope.define(Symbol(Symbol.Kind.PRIMITIVE_TYPE, "int64"))
    builtinScope.define(Symbol(Symbol.Kind.PRIMITIVE_TYPE, "float"))
    builtinScope.define(Symbol(Symbol.Kind.PRIMITIVE_TYPE, "float32"))
    builtinScope.define(Symbol(Symbol.Kind.PRIMITIVE_TYPE, "float64"))
    builtinScope.define(Symbol(Symbol.Kind.PRIMITIVE_TYPE, "void"))

  def setInput (input: List[Token]) =
    this.input = input
    lookahead = input(position)

  def match_ (kind: Token.Kind) =
    if lookahead.kind == kind then
      consume()
    else
      print(s"invalid token: expected ${kind}, got ${lookahead.kind}")

  def match_ (lexeme: String) =
    // Note: If re-writing in java, we need to compare using .equals() method
    if lookahead.lexeme == lexeme then
      consume()
    else
      print(s"invalid token: expected '${lexeme}', got '${lookahead.lexeme}'")

  def consume () =
    position += 1
    lookahead = input(position)

  def process (): AstNode =
    definePrimitiveTypes()
    val node = translationUnit()
    return node

  // Not every AST node has a corresponding token. Case in point is
  // translationUnit.

  // A cobalt translation unit is a directory of source files. We might rename
  // this to 'moduleUnit' later if it turns out that a translationUnit maps 1:1
  // to a module.

  def translationUnit (): AstNode =
    val n = AstNode(AstNode.Kind.TRANSLATION_UNIT)
    while lookahead.kind != Token.Kind.EOF do
      // Infinite loop, need to consume
      println(s"Sleeping for ${SLEEP_TIME} seconds in translationUnit...")
      Thread.sleep(SLEEP_TIME)
      n.addChild(declaration())
    return n

  // DECLARATIONS

  // Template must come first before any modifiers.

  def declaration (): AstNode =
    var n: AstNode = null
    val spec = accessSpecifier()
    if lookahead.kind == Token.Kind.TEMPLATE then
      n = templateDeclaration(spec)
    else
      val mods = modifiers()
      n = lookahead.kind match
        case Token.Kind.CLASS => classDeclaration(spec, mods)
        case Token.Kind.ENUM  => enumerationDeclaration(spec, mods)
        case Token.Kind.DEF   => routineDeclaration(spec, mods)
        case Token.Kind.VAL   => variableDeclaration(spec, mods)
        case Token.Kind.VAR   => variableDeclaration(spec, mods)
        case _ =>
          // We REALLY need to start some real error handling...
          println(s"Found something else! ${lookahead.kind}")
          null
    return n

  // Callers can explicitly request an empty access specifier. This is useful
  // for template declarations, where the access specifier is on the template
  // declaration itself rather than the templated entity. In this case, the
  // templated entity's AST node will still have an access specifier node, but
  // it will not have any children. This will be interpreted later to mean that
  // there is no access specifier. I prefer to handle it this way instead of not
  // having an access specifier node at all because it avoids having to create
  // specialized grammar rules for each case. However, I may change this later
  // if the alternative proves better.

  // Update: Just use the token as the discriminator. A missing token means that
  // the access specifier was not specified. Also might need to add protected as
  // another option.

  def accessSpecifier (empty: Boolean = false): AstNode =
    val n = AstNode(AstNode.Kind.ACCESS_SPECIFIER)
    if
      lookahead.kind == Token.Kind.PRIVATE ||
      lookahead.kind == Token.Kind.PUBLIC
    then
      n.setToken(lookahead)
      match_(lookahead.kind)
    return n

  // According to Parr, there is no need to have an AstNode kind -- you can just
  // use the token to determine what kind of node it is. This works only for
  // Simple cases. Sometimes, there is no corresponding token. So for that
  // reason, we choose to have a AstNode kind field. That said, this means that
  // sometimes we can leave the kind field generic and distinguish with more
  // granularity by looking at the token.

  // Todo: We might just want to have one kind of modifier node and let the
  // token indicate what kind of modifier it is. The problem with this is that
  // some modifiers are added implicitly (e.g. 'final' in the case of 'val'), so
  // such modifiers do not actually have tokens. We could create a virtual token
  // but it would not have a position in the character stream, so it would lack
  // things like a column and line number.

  def modifiers (): AstNode =
    val n = AstNode(AstNode.Kind.MODIFIERS)
    while
      lookahead.kind == Token.Kind.ABSTRACT  ||
      lookahead.kind == Token.Kind.CONST     ||
      lookahead.kind == Token.Kind.CONSTEXPR ||
      lookahead.kind == Token.Kind.FINAL     ||
      lookahead.kind == Token.Kind.OVERRIDE  ||
      lookahead.kind == Token.Kind.STATIC    ||
      lookahead.kind == Token.Kind.VIRTUAL   ||
      lookahead.kind == Token.Kind.VOLATILE
    do
      println(s"Sleeping for ${SLEEP_TIME} seconds in translationUnit...")
      Thread.sleep(SLEEP_TIME)
      val modifier = lookahead.kind match
        case Token.Kind.ABSTRACT  => abstractModifier()
        case Token.Kind.CONST     => constModifier()
        case Token.Kind.CONSTEXPR => constexprModifier()
        case Token.Kind.FINAL     => finalModifier()
        case Token.Kind.OVERRIDE  => overrideModifier()
        case Token.Kind.STATIC    => staticModifier()
        case Token.Kind.VIRTUAL   => virtualModifier()
        case Token.Kind.VOLATILE  => volatileModifier()
        case _ =>
          throw new Exception("Parser error: impossible case reached.")
      n.addChild(modifier)
    return n

  // I would like the 'final' modifier on variables to be equivalent to using
  // 'const' in C++ because I want to use the 'const' modifier to mean the same
  // as 'constexpr' in C++. This might not be possible because 'const' in C++
  // is not really equivalent to 'final' in Java -- it implies additional
  // constraints on the code, where it requires other things to be 'const' as
  // well. It would be strange if we made 'final' require other things to be
  // 'final' as well, if in that other context, 'final' didn't sound right or
  // 'final' was already defined to mean something else. If we have to use
  // 'const' instead of 'final' then we'll need to find something else to use
  // instead of 'const' for compile-time constants, such as 'comptime'. I have a
  // natural aversion to 'constexpr' for some reason... it's a bit obtuse for my
  // taste.

  def abstractModifier (): AstNode =
    val n = AstNode(AstNode.Kind.ABSTRACT_MODIFIER, lookahead)
    match_(Token.Kind.ABSTRACT)
    return n

  def constModifier (): AstNode =
    val n = AstNode(AstNode.Kind.CONST_MODIFIER, lookahead)
    match_(Token.Kind.CONST)
    return n

  def constexprModifier (): AstNode =
    val n = AstNode(AstNode.Kind.CONSTEXPR_MODIFIER, lookahead)
    match_(Token.Kind.CONSTEXPR)
    return n

  def finalModifier (): AstNode =
    val n = AstNode(AstNode.Kind.FINAL_MODIFIER, lookahead)
    match_(Token.Kind.FINAL)
    return n

  def overrideModifier (): AstNode =
    val n = AstNode(AstNode.Kind.OVERRIDE_MODIFIER, lookahead)
    match_(Token.Kind.OVERRIDE)
    return n

  def staticModifier (): AstNode =
    val n = AstNode(AstNode.Kind.STATIC_MODIFIER, lookahead)
    match_(Token.Kind.STATIC)
    return n

  def virtualModifier (): AstNode =
    val n = AstNode(AstNode.Kind.VIRTUAL_MODIFIER, lookahead)
    match_(Token.Kind.VIRTUAL)
    return n

  def volatileModifier (): AstNode =
    val n = AstNode(AstNode.Kind.VOLATILE_MODIFIER, lookahead)
    match_(Token.Kind.VOLATILE)
    return n

  // TEMPLATE DECLARATION

  def templateDeclaration (accessSpecifier : AstNode): AstNode =
    val n = AstNode(AstNode.Kind.TEMPLATE_DECLARATION, lookahead)
    match_(Token.Kind.TEMPLATE)
    n.addChild(accessSpecifier)
    n.addChild(templateParameters())
    val p = modifiers()
    val q = lookahead.kind match
      case Token.Kind.CLASS => classDeclaration(empty(), p)
      case Token.Kind.DEF   => routineDeclaration(empty(), p)
      case _ =>
        println(s"Found something else in template declaration! ${lookahead.kind}")
        null
    n.addChild(q)
    return n

  def templateParameters (): AstNode =
    val n = AstNode(AstNode.Kind.TEMPLATE_PARAMETERS, lookahead)
    match_ (Token.Kind.L_BRACKET)
    // There must be at least one template parameter
    n.addChild(templateParameter())
    while lookahead.kind == Token.Kind.COMMA do
      match_(Token.Kind.COMMA)
      n.addChild(templateParameter())
    match_ (Token.Kind.R_BRACKET)
    return n

  // For now just support type names as template Parameters. Later, we can
  // expand on this. Can we allow templates as parameters (i.e. nesting)?

  def templateParameter (): AstNode =
    val n = AstNode(AstNode.Kind.TEMPLATE_PARAMETER, lookahead)
    // Add to symbol table?
    return n

  // CLASS DECLARATION

  def classDeclaration (accessSpecifier: AstNode, modifiers: AstNode): AstNode =
    val n = AstNode(AstNode.Kind.CLASS_DECLARATION, lookahead)
    match_(Token.Kind.CLASS)
    n.addChild(accessSpecifier)
    n.addChild(modifiers)
    n.addChild(className())
    n.addChild(classBody())
    return n

  // Todo: Should symbols point to AST node, and/or vice versa? This might come
  // in handy later on, but wait until its needed before adding the code.

  def className (): AstNode =
    val n = AstNode(AstNode.Kind.NAME)
    n.setToken(lookahead)
    match_(Token.Kind.IDENTIFIER)
    val s = Symbol(Symbol.Kind.CLASS, n.getToken().lexeme)
    currentScope.define(s)
    return n

  // The token here is simply the curly brace '{'. Do we need to track this?
  // It will depend on whether or not this sort of thing helps with error
  // reporting and debugging.

  def classBody (): AstNode =
    val n = AstNode(AstNode.Kind.CLASS_BODY, lookahead)
    match_(Token.Kind.L_BRACE)
    while lookahead.kind != Token.Kind.R_BRACE do
      n.addChild(classMember())
    match_(Token.Kind.R_BRACE)
    return n

  // Do we need separate rules for methods and fields? Syntactically, they seem
  // to parse the same as non-member routines and variables, but semantically,
  // they have different modifiers and possibly scoping rules. However, those
  // should be discernable during any semantic analysis phases without requiring
  // different AST node types.

  // Note: One exception for methods is that we might have qualifiers after the
  // parameter list (e.g. const, &, &&) that don't exist for regular routines.

  def classMember (): AstNode =
    val spec = accessSpecifier()
    val mods = modifiers()
    val n = lookahead.kind match
      case Token.Kind.DEF => methodDeclaration(spec, mods)
      case Token.Kind.VAL => variableDeclaration(spec, mods)
      case Token.Kind.VAR => variableDeclaration(spec, mods)
      case _ => 
          print("error: This can only happen if there is a parser error.")
          null
    return n

  // METHOD DECLARATION

  // Note: The C++ specification calls these "member functions" rather than
  // "methods". We may decide to call them "member routines" in keeping with C++
  // tradition.

  // Todo: We need to push another scope onto the scope stack. Keep in mind that
  // the method parameters may be in the same exact scope as the routine body
  // (or top-most block of the routine).

  def methodDeclaration (accessSpecifier: AstNode, modifiers: AstNode): AstNode =
    val n = AstNode(AstNode.Kind.METHOD_DECLARATION, lookahead)
    match_(Token.Kind.DEF)
    n.addChild(accessSpecifier)
    n.addChild(modifiers)
    n.addChild(methodName())
    n.addChild(methodParameters())
    n.addChild(methodResult())
    n.addChild(methodBody())
    return n

  // Todo: Should symbols point to AST node, and/or vice versa? This might come
  // in handy later on, but wait until its needed before adding the code.

  def methodName (): AstNode =
    val n = AstNode(AstNode.Kind.NAME, lookahead)
    match_(Token.Kind.IDENTIFIER)
    val s = Symbol(Symbol.Kind.METHOD, n.getToken().lexeme)
    currentScope.define(s)
    return n

  // Todo: Method parameters includes parenthesis, check consistency of other
  // parameter rules.

  def methodParameters (): AstNode =
    val n = AstNode(AstNode.Kind.METHOD_PARAMETERS)
    match_(Token.Kind.L_PARENTHESIS)
    if lookahead.kind == Token.Kind.IDENTIFIER then
      n.addChild(methodParameter())
    while lookahead.kind == Token.Kind.COMMA do
      match_(Token.Kind.COMMA)
      n.addChild(methodParameter())
    match_(Token.Kind.R_PARENTHESIS)
    return n

  // Shoud name() be parameterName()?

  def methodParameter (): AstNode =
    val n = AstNode(AstNode.Kind.METHOD_PARAMETER)
    n.addChild(name())
    match_(Token.Kind.COLON)
    n.addChild(typeRoot())
    return n

  def methodResult (): AstNode =
    val n = AstNode(AstNode.Kind.METHOD_RESULT)
    if lookahead.kind == Token.Kind.MINUS_GREATER then
      match_(Token.Kind.MINUS_GREATER)
      n.addChild(typeRoot())
    return n

  // A combination of 'abstract' keyword and an empty body indicates an abstract
  // method.

  def methodBody (): AstNode =
    val n = AstNode(AstNode.Kind.METHOD_BODY)
    if lookahead.kind == Token.Kind.SEMICOLON then
      match_(Token.Kind.SEMICOLON)
    else
      n.addChild(compoundStatement())
    return n

  // ENUMERATION DECLARATION

  def enumerationDeclaration (accessSpecifier: AstNode, modifiers: AstNode): AstNode =
    val n = AstNode(AstNode.Kind.ENUMERATION_DECLARATION, lookahead)
    match_(Token.Kind.ENUM)
    // Should be name()?
    n.addChild(identifier())
    n.addChild(enumerationBody())
    return n

  def enumerationBody (): AstNode =
    val n = AstNode(AstNode.Kind.ENUMERATION_BODY)
    match_(Token.Kind.L_BRACE)
    while lookahead.kind != Token.Kind.R_BRACE do
      n.addChild(enumerationMember())
    match_(Token.Kind.R_BRACE)
    return n

  def enumerationMember (): AstNode =
    val n = enumerationConstantDeclaration()
    return n

  def enumerationConstantDeclaration (): AstNode =
    val n = AstNode(AstNode.Kind.ENUMERATION_CONSTANT_DECLARATION, lookahead)
    match_(Token.Kind.VAL)
    n.addChild(name())
    match_(Token.Kind.SEMICOLON)
    return n

  // ROUTINE DECLARATION

  // Todo: We need to push another scope onto the scope stack. Keep in mind that
  // the routine parameters may be in the same exact scope as the routine body
  // (or top-most block of the routine).

  def routineDeclaration (accessSpecifier: AstNode, modifiers: AstNode): AstNode =
    val n = AstNode(AstNode.Kind.ROUTINE_DECLARATION, lookahead)
    match_(Token.Kind.DEF)
    n.addChild(accessSpecifier)
    n.addChild(modifiers)
    n.addChild(routineName())
    n.addChild(routineParameters())
    n.addChild(routineReturnType())
    n.addChild(routineBody())
    return n

  // Todo: Should symbols point to AST node, and/or vice versa? This might come
  // in handy later on, but wait until its needed before adding the code.

  def routineName (): AstNode =
    val n = AstNode(AstNode.Kind.NAME, lookahead)
    match_(Token.Kind.IDENTIFIER)
    val s = Symbol(Symbol.Kind.ROUTINE, n.getToken().lexeme)
    currentScope.define(s)
    return n

  def routineParameters (): AstNode =
    val n = AstNode(AstNode.Kind.ROUTINE_PARAMETERS)
    match_(Token.Kind.L_PARENTHESIS)
    if lookahead.kind == Token.Kind.IDENTIFIER then
      n.addChild(routineParameter())
    while lookahead.kind == Token.Kind.COMMA do
      match_(Token.Kind.COMMA)
      n.addChild(routineParameter())
    match_(Token.Kind.R_PARENTHESIS)
    return n

  // Routine parameters are for all intents and purposes local variables

  def routineParameter (): AstNode =
    val n = AstNode(AstNode.Kind.ROUTINE_PARAMETER)
    n.addChild(routineParameterName())
    match_(Token.Kind.COLON)
    n.addChild(typeRoot())
    return n

  def routineParameterName (): AstNode =
    val n = AstNode(AstNode.Kind.ROUTINE_PARAMETER_NAME, lookahead)
    match_(Token.Kind.IDENTIFIER)
    val s = Symbol(Symbol.Kind.VARIABLE, n.getToken().lexeme)
    currentScope.define(s)
    return n

  // We need to decide if we want to use an arrow or a colon for the result
  // type. C++, python, ruby, swift, ocaml, haskell, and rust all use an arrow,
  // while scala, kotlin, typescript, and pascal all use a colon. The proper
  // choice probably depends on whether or not the language in question already
  // uses colons and/or arrows for other things (and the amount that the symbol
  // appears in the program text); and whether it would lead to grammar
  // ambiguities. For now, we will use an arrow.

  // We can either treat this like a type specifier or use it as a passthrough
  // to a type specifier.

  def routineReturnType (): AstNode =
    val n = AstNode(AstNode.Kind.ROUTINE_RETURN_TYPE)
    if lookahead.kind == Token.Kind.MINUS_GREATER then
      match_(Token.Kind.MINUS_GREATER)
      n.addChild(typeRoot())
    return n

  // Do we need to distinguish between a top compound statement and a regular
  // compound statement? The top compound statement does not need to introduce a
  // new scope.

  def routineBody (): AstNode =
    val n = AstNode(AstNode.Kind.ROUTINE_BODY)
      n.addChild(compoundStatement())
    return n

  // VARIABLE DECLARATION

  def variableDeclaration (accessSpecifier: AstNode, modifiers: AstNode): AstNode =
    val n = AstNode(AstNode.Kind.VARIABLE_DECLARATION, lookahead)
    if lookahead.kind == Token.Kind.VAL then
      // Keyword 'val' is equivalent to 'final var'
      match_(Token.Kind.VAL)
      // This final modifier won't have a token since it is an implied modifier
      // that doesn't actually appear in the source code.
      // Update: We might not want to add an implicit modifier like this.
      // Instead, when it comes time to set attributes, we can set one base on
      // the token (val or var).
      modifiers.addChild(AstNode(AstNode.Kind.FINAL_MODIFIER))
    else
      match_(Token.Kind.VAR)
    n.addChild(accessSpecifier)
    n.addChild(modifiers)
    n.addChild(variableName())
    n.addChild(typeSpecifier())
    n.addChild(initializer())
    match_(Token.Kind.SEMICOLON)
    return n

  // Todo: Should symbols point to AST node, and/or vice versa? This might come
  // in handy later on, but wait until its needed before adding the code.

  def variableName (): AstNode =
    val n = AstNode(AstNode.Kind.NAME, lookahead)
    match_(Token.Kind.IDENTIFIER)
    val s = Symbol(Symbol.Kind.VARIABLE, n.getToken().lexeme)
    currentScope.define(s)
    return n

  def typeSpecifier (): AstNode =
    val n = AstNode(AstNode.Kind.TYPE_SPECIFIER)
    if lookahead.kind == Token.Kind.COLON then
      match_(Token.Kind.COLON)
      // Need to fix up array handling. Is this still true?
      n.addChild(typeRoot())
    return n

  // For now, we require an initializer for all variable declarations. This may
  // not be efficient, so the requirement may eventually be relaxed for certain
  // types or conditions.

  def initializer (): AstNode =
    val n = AstNode(AstNode.Kind.INITIALIZER)
    if lookahead.kind == Token.Kind.EQUAL then
      match_(Token.Kind.EQUAL)
      n.addChild(expression(root=true))
    return n

  // NAME

  // We need to distinguish between identifiers used when defining program
  // elements and identifiers used when referencing program elements. Is this
  // done already (comment copied in from python prototype)?

  def name (): AstNode =
    val n = AstNode(AstNode.Kind.NAME)
    n.setToken(lookahead)
    match_(Token.Kind.IDENTIFIER)
    return n

  // EMPTY

  // This node is useful as a placeholder so that we have known, fixed child
  // node counts for certain cases of nodes that have heterogeneous children.

  def empty (): AstNode =
    val n = AstNode(AstNode.Kind.EMPTY)
    return n

  // STATEMENTS

  val booleanLiteralFirstSet = Set(Token.Kind.FALSE, Token.Kind.TRUE)
  val characterLiteralFirstSet = Set(Token.Kind.CHARACTER_LITERAL)
  val floatingPointLiteralFirstSet = Set(Token.Kind.FLOAT32_LITERAL, Token.Kind.FLOAT64_LITERAL)
  val integerLiteralFirstSet = Set(Token.Kind.INT32_LITERAL, Token.Kind.INT64_LITERAL)
  val nullLiteralFirstSet = Set(Token.Kind.NULL)
  val unsignedIntegerLiteralFirstSet =  Set(Token.Kind.UINT32_LITERAL, Token.Kind.UINT64_LITERAL)
  val stringLiteralFirstSet = Set(Token.Kind.STRING_LITERAL)

  val literalFirstSet =
    booleanLiteralFirstSet ++
    characterLiteralFirstSet ++
    floatingPointLiteralFirstSet ++
    integerLiteralFirstSet ++
    nullLiteralFirstSet ++
    unsignedIntegerLiteralFirstSet ++
    stringLiteralFirstSet

  val expressionStatementFirstSet =
    Set(Token.Kind.IDENTIFIER, Token.Kind.THIS) ++
    literalFirstSet

  // Todo: Need to add any other required modifiers in here

  val declarationStatementFirstSet = Set(
    Token.Kind.STATIC,
    Token.Kind.VAL,
    Token.Kind.VAR
  )

  // Notice that we don't include 'if' in the first set for expression
  // statements. This is because we want send the parser towards the statement
  // version of 'if'.

  def statement (): AstNode =
    var n: AstNode = null
    val kind = lookahead.kind
    if kind == Token.Kind.BREAK then
      n = breakStatement()
    else if kind == Token.Kind.L_BRACE then
      n = compoundStatement()
    else if kind == Token.Kind.CONTINUE then
      n = continueStatement()
    else if declarationStatementFirstSet.contains(kind) then
      n = declarationStatement()
    else if expressionStatementFirstSet.contains(kind) then
      n = expressionStatement()
    else if kind == Token.Kind.DO then
      n = doStatement()
    else if kind == Token.Kind.SEMICOLON then
      n = emptyStatement()
    else if kind == Token.Kind.FOR then
      n = forStatement()
    else if kind == Token.Kind.FOREACH then
      n = foreachStatement()
    else if kind == Token.Kind.IF then
      n = ifStatement()
    else if kind == Token.Kind.RETURN then
      n = returnStatement()
    else if kind == Token.Kind.UNTIL then
      n = untilStatement()
    else if kind == Token.Kind.WHILE then
      n = whileStatement()
    else
      print("Error: Invalid statement")
    return n

  def breakStatement (): AstNode =
    val n = AstNode(AstNode.Kind.BREAK_STATEMENT)
    match_(Token.Kind.BREAK)
    match_(Token.Kind.SEMICOLON)
    return n

  // Per C++ standard, compound statements are also equivalently called blocks.

  // The top-most block needs to use the scope of the routine itself
  // so we might need a topBlock production. Alternatively, we can
  // pass in a parameter that says whether or not to create a new
  // scope.

  val standardStatementFirstSet = Set(
    Token.Kind.BREAK,
    Token.Kind.CONTINUE,
    Token.Kind.BREAK,
    Token.Kind.L_BRACE,
    Token.Kind.CONTINUE,
    Token.Kind.DO,
    Token.Kind.SEMICOLON,
    Token.Kind.FOR,
    Token.Kind.FOREACH,
    Token.Kind.IF,
    Token.Kind.RETURN,
    Token.Kind.UNTIL,
    Token.Kind.WHILE
  )

  val statementFirstSet =
    standardStatementFirstSet ++
    declarationStatementFirstSet ++
    expressionStatementFirstSet

  def compoundStatement (): AstNode =
    val n = AstNode(AstNode.Kind.COMPOUND_STATEMENT)
    match_(Token.Kind.L_BRACE)
    while statementFirstSet.contains(lookahead.kind) do
      println(s"Sleeping for ${SLEEP_TIME} seconds in compoundStatement...")
      Thread.sleep(SLEEP_TIME)
      n.addChild(statement())
    match_(Token.Kind.R_BRACE)
    return n

  def continueStatement (): AstNode =
    val n = AstNode(AstNode.Kind.CONTINUE_STATEMENT)
    match_(Token.Kind.CONTINUE)
    match_(Token.Kind.SEMICOLON)
    return n

  // For now we only support variable declaration statements (i.e. local
  // variables). I do not think cobalt needs local classes since they have
  // significant limitations in C++ and only niche use cases. I would like to
  // have nested routines, but since C++ doesn't have them, I need to research
  // the feasibility of that idea. We might be able to compile nested routines
  // into C++ lambda functions.

  // It seems that we can just use the existing variableDeclaration production
  // but if we need to distinguish between local and global variables, then we
  // might need a separate production. Alternatively, we could use a flag to
  // signify one or the other. Or we can defer the question to later phases.

  // Should this just be a passthrough or do we want dedicated AST nodes at the
  // root of all statement sub-trees?

  def declarationStatement (): AstNode =
    var n: AstNode = null
    var mods = modifiers()
    val kind = lookahead.kind
    if kind == Token.Kind.VAL || kind == Token.Kind.VAR then
      n = variableDeclaration(null, mods)
    return n

  // The do statement is flexible and can either be a "do while" or a "do until"
  // statement, depending on what follows the 'do' keyword.

  def doStatement (): AstNode =
    val n = AstNode(AstNode.Kind.DO_STATEMENT, lookahead)
    match_(Token.Kind.DO)
    if lookahead.kind == Token.Kind.UNTIL then
      n.addChild(untilStatement())
    else if lookahead.kind == Token.Kind.WHILE then
      n.addChild(whileStatement())
    return n

  // Note: Microsoft calls this a "null statement", but the offical C++ standard
  // (latest ISO/IEC 14882:2023) has always used the term "empty statement". I
  // like the official term more.

  // Update: The situation might be a little more complex, see ISO/IEC IS 14882
  // Draft C++ standard, N3092.

  // Note: Empty statements may be a type of expression statement under C++
  // rules. I am not sure how much sense that makes because an expression
  // statement should presumably evaluate to some value, but a null statement
  // does not. (On the other hand, a procedure call is considered an expression
  // statement and also does not evaluate to a value.)

  // An empty statement could theoretically produce no AST node at all, since it
  // is a "noop". However, this may be useful to do because it will provide a
  // more faithful translation to C++. It will be optimized out by C++ anyways.

  def emptyStatement (): AstNode =
    val n = AstNode(AstNode.Kind.EMPTY_STATEMENT, lookahead)
    match_(Token.Kind.SEMICOLON)
    return n

  // The expression statement is primarily just a passthrough, but we want a
  // dedicated AST node at the root of all statement sub-trees.

  def expressionStatement(): AstNode =
    val n = AstNode(AstNode.Kind.EXPRESSION_STATEMENT)
    n.addChild(expression(root=true))
    match_(Token.Kind.SEMICOLON)
    return n

  def forStatement (): AstNode =
    val n = AstNode(AstNode.Kind.FOR_STATEMENT, lookahead)
    match_(Token.Kind.FOR)
    match_(Token.Kind.L_PARENTHESIS)
    // Technically, these expressions can be empty. Perhaps this is why null
    // statements are really expressions. We can research that and tidy up the
    // grammar later.
    n.addChild(forInit())
    n.addChild(forCondition())
    n.addChild(forUpdate())
    match_(Token.Kind.R_PARENTHESIS)
    if lookahead.kind == Token.Kind.L_BRACE then
      n.addChild(compoundStatement())
    else
      n.addChild(statement())
    return n

  // The for initialization part consists of either a special variable
  // declaration or a list of expressions; or it may be empty.

  def forInit (): AstNode =
    val n = AstNode(AstNode.Kind.FOR_INIT, lookahead)
    if lookahead.kind == Token.Kind.VAR then
      // Are any modifiers allowed on this declaration? Probably not.
      n.addChild(forInitDeclaration())
    else if
      // This really needs to be an expression first set
      lookahead.kind == Token.Kind.IDENTIFIER ||
      lookahead.kind == Token.Kind.THIS
    then
      n.addChild(forInitExpressionList())
    else
      // Empty case
      match_(Token.Kind.SEMICOLON)
    return n

  def forInitDeclaration (): AstNode =
    val n = AstNode(AstNode.Kind.FOR_INIT_DECLARATION, lookahead)
    match_(Token.Kind.VAR)
    n.addChild(variableName())
    n.addChild(typeSpecifier())
    n.addChild(initializer())
    match_(Token.Kind.SEMICOLON)
    return n

  def forInitExpressionList (): AstNode =
    val n = AstNode(AstNode.Kind.FOR_INIT_EXPRESSION_LIST)
    n.addChild(expression(root=true))
    while lookahead.kind == Token.Kind.COMMA do
      match_(Token.Kind.COMMA)
      n.addChild(expression(root=true))
    match_(Token.Kind.SEMICOLON)
    return n

  // Todo: Do we need a condition node or is a plain expression good enough?

  def forCondition (): AstNode =
    val n = AstNode(AstNode.Kind.FOR_CONDITION)
    if
      // This really needs to be an expression first set
      lookahead.kind == Token.Kind.IDENTIFIER ||
      lookahead.kind == Token.Kind.THIS
    then
      n.addChild(expression(root=true))
      match_(Token.Kind.SEMICOLON)
    else
      // Empty case
      match_(Token.Kind.SEMICOLON)
    return n

  def forUpdate (): AstNode =
    val n = AstNode(AstNode.Kind.FOR_UPDATE)
    if
      // This really needs to be an expression first set
      lookahead.kind == Token.Kind.IDENTIFIER ||
      lookahead.kind == Token.Kind.THIS
    then
      n.addChild(expression(root=true))
    while lookahead.kind == Token.Kind.COMMA do
      match_(Token.Kind.COMMA)
      n.addChild(expression(root=true))
    return n

  // After trying to handle basic and enhanced for loops with a single keyword,
  // it became clear that it is much cleaner and easier to use two separate
  // ones. Basic for loops use 'for', whereas enhanced for loops use 'foreach'.
  
  // Most languages seem to either combine the two kinds of for loops or forego
  // basic for loops altogether on the basis that they are too low-level. In
  // this case, they use the (now free) 'for' keyword to handle enhanced for
  // loops exclusively. However, basic for loops are too useful for low-level
  // languages like C++ and Cobalt to give up. Thus, we choose to keep both
  // kinds of loops and give each their own dedicated keyword.

  def foreachStatement (): AstNode =
    val n = AstNode(AstNode.Kind.FOREACH_STATEMENT, lookahead)
    return n

  def ifStatement (): AstNode =
    val n = AstNode(AstNode.Kind.IF_STATEMENT, lookahead)
    match_(Token.Kind.IF)
    n.addChild(ifCondition())
    n.addChild(ifBody())
    if lookahead.kind == Token.Kind.ELSE then
      n.addChild(elseClause())
    return n

  def ifCondition (): AstNode =
    match_(Token.Kind.L_PARENTHESIS)
    val n = expression(root=true)
    match_(Token.Kind.R_PARENTHESIS)
    return n

  def ifBody (): AstNode =
    var n: AstNode = null
    if lookahead.kind == Token.Kind.L_BRACE then
      n = compoundStatement()
    else
      // Insert fabricated compound statement
      n = AstNode(AstNode.Kind.COMPOUND_STATEMENT)
      n.addChild(statement())
    return n

  def elseClause (): AstNode =
    val n = AstNode(AstNode.Kind.ELSE_CLAUSE, lookahead)
    match_(Token.Kind.ELSE)
    n.addChild(elseBody())
    return n

  def elseBody (): AstNode =
    var n: AstNode = null
    if lookahead.kind == Token.Kind.L_BRACE then
      n = compoundStatement()
    else
      // Insert fabricated compound statement
      n = AstNode(AstNode.Kind.COMPOUND_STATEMENT)
      n.addChild(statement())
    return n

  def returnStatement (): AstNode =
    val n = AstNode(AstNode.Kind.RETURN_STATEMENT, lookahead)
    match_(Token.Kind.RETURN)
    // Should we explicitly check FIRST, or is it ok to just check FOLLOW?
    if lookahead.kind != Token.Kind.SEMICOLON then
      n.addChild(expression(root=true))
    match_(Token.Kind.SEMICOLON)
    return n

  // C++ doesn't have 'until' statements, but I would like to have them. I often
  // need an "until (done) doSomething();" loop. Yes, that requirement can be
  // met by a 'while' statement such as "while (!done) doSomething();" but I
  // prefer to have the option to use either one. If the 'until' statement
  // proves controversial or unpopular (or is deemed to not be orthogonal
  // enough) then it can be removed later.

  def untilStatement (): AstNode =
    val n = AstNode(AstNode.Kind.UNTIL_STATEMENT, lookahead)
    match_(Token.Kind.UNTIL)
    n.addChild(untilCondition())
    n.addChild(untilBody())
    return n

  // In C++26 the condition can be an expression or a declaration. For now, we
  // will only support expressions, and use the rule as a passthrough.

  // We can handle transformation to a 'while' statement here by inserting an
  // AST node that complements the expression. However, the parser should not
  // concern itself with the details of the target language. The lack of an
  // 'until' statement is a concern of the target language, so we will leave it
  // up to a separate transformation or generation phase to make that change.

  def untilCondition (): AstNode =
    match_(Token.Kind.L_PARENTHESIS)
    val n = expression(root=true)
    match_(Token.Kind.R_PARENTHESIS)
    return n

  def untilBody (): AstNode =
    var n: AstNode = null
    if lookahead.kind == Token.Kind.L_BRACE then
      n = statement()
    else
      // Insert fabricated compound statement
      n = AstNode(AstNode.Kind.COMPOUND_STATEMENT)
      n.addChild(statement())
    return n

  def whileStatement (): AstNode =
    val n = AstNode(AstNode.Kind.WHILE_STATEMENT, lookahead)
    match_(Token.Kind.WHILE)
    n.addChild(whileCondition())
    n.addChild(whileBody())
    return n

  // In C++26 the condition can be an expression or a declaration. For now, we
  // will only support expressions, and use the rule as a passthrough.

  def whileCondition (): AstNode =
    match_(Token.Kind.L_PARENTHESIS)
    val n = expression(root=true)
    match_(Token.Kind.R_PARENTHESIS)
    return n

  def whileBody (): AstNode =
    var n: AstNode = null
    if lookahead.kind == Token.Kind.L_BRACE then
      n = statement()
    else
      // Insert fabricated compound statement
      n = AstNode(AstNode.Kind.COMPOUND_STATEMENT)
      n.addChild(statement())
    return n

  // EXPRESSIONS

  // The root of every expression sub-tree has an explicit 'expression' AST
  // node, where the final computed type and other synthesized attributes can be
  // stored to aid in such things as type-checking.

  def expression (root: Boolean = false): AstNode =
    if root then
      val n = AstNode(AstNode.Kind.EXPRESSION)
      n.addChild(assignmentExpression())
      return n
    else
      return assignmentExpression()

  // We may wish to add a 'walrus' operator (:=), which can be used inside a
  // conditional statement to indicate that the developer truly intends to have
  // and assignment rather than an equality check.

  def assignmentExpression (): AstNode =
    var n = logicalOrExpression()
    while
      lookahead.kind == Token.Kind.EQUAL                 ||
      lookahead.kind == Token.Kind.ASTERISK_EQUAL        ||
      lookahead.kind == Token.Kind.SLASH_EQUAL           ||
      lookahead.kind == Token.Kind.PERCENT_EQUAL         ||
      lookahead.kind == Token.Kind.PLUS_EQUAL            ||
      lookahead.kind == Token.Kind.MINUS_EQUAL           ||
      lookahead.kind == Token.Kind.LESS_LESS_EQUAL       ||
      lookahead.kind == Token.Kind.GREATER_GREATER_EQUAL ||
      lookahead.kind == Token.Kind.AMPERSAND_EQUAL       ||
      lookahead.kind == Token.Kind.CARET_EQUAL           ||
      lookahead.kind == Token.Kind.BAR_EQUAL
    do
      var p = n
      n = AstNode(AstNode.Kind.BINARY_EXPRESSION, lookahead)
      n.addChild(p)
      match_(lookahead.kind)
      p = logicalOrExpression()
      n.addChild(p)
    return n

  def logicalOrExpression (): AstNode =
    var n = logicalAndExpression()
    while lookahead.kind == Token.Kind.OR do
      var p = n
      n = AstNode(AstNode.Kind.BINARY_EXPRESSION, lookahead)
      n.addChild(p)
      match_(Token.Kind.OR)
      n.addChild(logicalAndExpression())
    return n

  def logicalAndExpression (): AstNode =
    var n = inclusiveOrExpression()
    while lookahead.kind == Token.Kind.AND do
      var p = n
      n = AstNode(AstNode.Kind.BINARY_EXPRESSION, lookahead)
      n.addChild(p)
      match_(Token.Kind.AND)
      n.addChild(inclusiveOrExpression())
    return n
  
  def inclusiveOrExpression (): AstNode =
    var n = exclusiveOrExpression()
    while lookahead.kind == Token.Kind.BAR do
      var p = n
      n = AstNode(AstNode.Kind.BINARY_EXPRESSION, lookahead)
      n.addChild(p)
      match_(Token.Kind.BAR)
      n.addChild(exclusiveOrExpression())
    return n

  def exclusiveOrExpression (): AstNode =
    var n = andExpression()
    while lookahead.kind == Token.Kind.CARET do
      var p = n
      n = AstNode(AstNode.Kind.BINARY_EXPRESSION, lookahead)
      n.addChild(p)
      match_(Token.Kind.CARET)
      n.addChild(andExpression())
    return n
  
  def andExpression (): AstNode =
    var n = equalityExpression()
    while lookahead.kind == Token.Kind.AMPERSAND do
      var p = n
      n = AstNode(AstNode.Kind.BINARY_EXPRESSION, lookahead)
      n.addChild(p)
      match_(Token.Kind.AMPERSAND)
      n.addChild(equalityExpression())
    return n

  def equalityExpression (): AstNode =
    var n = relationalExpression()
    while
      lookahead.kind == Token.Kind.EQUAL_EQUAL ||
      lookahead.kind == Token.Kind.EXCLAMATION_EQUAL
    do
      var p = n
      n = AstNode(AstNode.Kind.BINARY_EXPRESSION, lookahead)
      n.addChild(p)
      match_(lookahead.kind)
      n.addChild(relationalExpression())
    return n

  def relationalExpression (): AstNode =
    var n = shiftExpression()
    while
      lookahead.kind == Token.Kind.GREATER       ||
      lookahead.kind == Token.Kind.LESS          ||
      lookahead.kind == Token.Kind.GREATER_EQUAL ||
      lookahead.kind == Token.Kind.LESS_EQUAL
    do
      var p = n
      n = AstNode(AstNode.Kind.BINARY_EXPRESSION, lookahead)
      n.addChild(p)
      match_(lookahead.kind)
      n.addChild(shiftExpression())
    return n

  def shiftExpression (): AstNode =
    var n = additiveExpression()
    while
      lookahead.kind == Token.Kind.GREATER_GREATER ||
      lookahead.kind == Token.Kind.LESS_LESS
    do
      var p = n
      n = AstNode(AstNode.Kind.BINARY_EXPRESSION, lookahead)
      n.addChild(p)
      match_(lookahead.kind)
      n.addChild(additiveExpression())
    return n

  def additiveExpression (): AstNode =
    var n = multiplicativeExpression()
    while
      lookahead.kind == Token.Kind.PLUS ||
      lookahead.kind == Token.Kind.MINUS
    do
      var p = n
      n = AstNode(AstNode.Kind.BINARY_EXPRESSION, lookahead)
      n.addChild(p)
      match_(lookahead.kind)
      n.addChild(multiplicativeExpression())
    return n

  def multiplicativeExpression (): AstNode =
    var n = unaryExpression()
    while
      lookahead.kind == Token.Kind.ASTERISK ||
      lookahead.kind == Token.Kind.SLASH    ||
      lookahead.kind == Token.Kind.PERCENT
    do
      var p = n
      n = AstNode(AstNode.Kind.BINARY_EXPRESSION, lookahead)
      n.addChild(p)
      match_(lookahead.kind)
      n.addChild(unaryExpression())
    return n
  
  // Why recursion here instead of iteration? Does it matter?

  def unaryExpression (): AstNode =
    var n: AstNode = null
    if
      lookahead.kind == Token.Kind.ASTERISK    ||
      lookahead.kind == Token.Kind.MINUS       ||
      lookahead.kind == Token.Kind.PLUS        ||
      lookahead.kind == Token.Kind.EXCLAMATION ||
      lookahead.kind == Token.Kind.TILDE
    then
      n = AstNode(AstNode.Kind.UNARY_EXPRESSION, lookahead)
      match_(lookahead.kind)
      n.addChild(unaryExpression())
    else
      n = postfixExpression()
    return n

  // The postfix expression grammar below permits semantically invalid results.
  // For example, primary expressions can include integer literals, so the
  // parser will successfully parse '5++', even though that isn't semantically
  // sound because integer literals are not l-values. This issue is addressed
  // during a semantic analysis pass (which is how it seems to be dealt with in
  // C++ and some other languages). Although we might be able to adjust the
  // grammar to avoid this problem, the for now we will just stick with the
  // traditional design.

  def postfixExpression (): AstNode =
    var node = primaryExpression()
    while
      lookahead.kind == Token.Kind.MINUS_GREATER ||
      lookahead.kind == Token.Kind.PERIOD        ||
      lookahead.kind == Token.Kind.L_PARENTHESIS ||
      lookahead.kind == Token.Kind.L_BRACKET
    do
      lookahead.kind match
        case Token.Kind.MINUS_GREATER =>
          node = dereferencingMemberAccess(node)
        case Token.Kind.PERIOD =>
          node = memberAccess(node)
        case Token.Kind.L_PARENTHESIS =>
          node = routineCall(node)
        case Token.Kind.L_BRACKET =>
          node = arraySubscript(node)
        case _ =>
          println("Error: No viable alternative in postfixExpression")
    return node

  def dereferencingMemberAccess (nameExpr: AstNode): AstNode =
    val n = AstNode(AstNode.Kind.DEREFERENCING_MEMBER_ACCESS, lookahead)
    n.addChild(nameExpr)
    match_(Token.Kind.MINUS_GREATER)
    n.addChild(name())
    return n

  def memberAccess (nameExpr: AstNode): AstNode =
    val n = AstNode(AstNode.Kind.MEMBER_ACCESS, lookahead)
    n.addChild(nameExpr)
    match_(Token.Kind.PERIOD)
    n.addChild(name())
    return n

  // Subroutines (or 'routines' for short) may be classified as 'functions',
  // which return a result; or 'procedures', which do not. Furthermore, routines
  // that are members of a class are known as 'methods'. However, we do not
  // distinguish between all these types of routines using different keywords.

  def routineCall (nameExpr: AstNode): AstNode =
    val n = AstNode(AstNode.Kind.ROUTINE_CALL, lookahead)
    n.addChild(nameExpr)
    n.addChild(arguments())
    return n

  // Todo: Maybe change to routineArguments and add routineArgument

  def arguments (): AstNode =
    val n = AstNode(AstNode.Kind.ARGUMENTS)
    match_(Token.Kind.L_PARENTHESIS)
    if lookahead.kind != Token.Kind.R_PARENTHESIS then
      n.addChild(expression())
      while lookahead.kind == Token.Kind.COMMA do
        match_(Token.Kind.COMMA)
        n.addChild(expression())
    match_(Token.Kind.R_PARENTHESIS)
    return n

  def arraySubscript (nameExpr: AstNode): AstNode =
    val n = AstNode(AstNode.Kind.SUBSCRIPT)
    n.addChild(nameExpr)
    match_(Token.Kind.L_BRACKET)
    n.addChild(expression())
    match_(Token.Kind.R_BRACKET)
    return n

  def primaryExpression (): AstNode =
    var n: AstNode = null
    if literalFirstSet.contains(lookahead.kind) then
      n = literal()
    else if lookahead.kind == Token.Kind.THIS then
      n = this_()
    else if lookahead.kind == Token.Kind.IDENTIFIER then
      n = name()
    else if lookahead.kind == Token.Kind.IF then
      n = ifExpression()
    else if lookahead.kind == Token.Kind.L_PARENTHESIS then
      n = parenthesizedExpression()
    else
      println("ERROR - INVALID PRIMARY EXPRESSION")
    return n

  def literal (): AstNode =
    var n: AstNode = null
    if booleanLiteralFirstSet.contains(lookahead.kind) then
      n = booleanLiteral()
    else if characterLiteralFirstSet.contains(lookahead.kind) then
      n = characterLiteral()
    else if floatingPointLiteralFirstSet.contains(lookahead.kind) then
      n = floatingPointLiteral()
    else if integerLiteralFirstSet.contains(lookahead.kind) then
      n = integerLiteral()
    else if nullLiteralFirstSet.contains(lookahead.kind) then
      n = nullLiteral()
    else if stringLiteralFirstSet.contains(lookahead.kind) then
      n = stringLiteral()
    else if unsignedIntegerLiteralFirstSet.contains(lookahead.kind) then
      n = unsignedIntegerLiteral()
    return n

  def booleanLiteral (): AstNode =
    val n = AstNode(AstNode.Kind.BOOLEAN_LITERAL, lookahead)
    consume()
    return n

  def characterLiteral (): AstNode =
    val n = AstNode(AstNode.Kind.CHARACTER_LITERAL, lookahead)
    consume()
    return n

  def floatingPointLiteral (): AstNode =
    val n = AstNode(AstNode.Kind.FLOATING_POINT_LITERAL, lookahead)
    consume()
    return n

  def integerLiteral (): AstNode =
    val n = AstNode(AstNode.Kind.INTEGER_LITERAL, lookahead)
    consume()
    return n

  def nullLiteral (): AstNode =
    val n = AstNode(AstNode.Kind.NULL_LITERAL, lookahead)
    consume()
    return n

  def stringLiteral (): AstNode =
    val n = AstNode(AstNode.Kind.STRING_LITERAL, lookahead)
    consume()
    return n

  def unsignedIntegerLiteral (): AstNode =
    val n =AstNode(AstNode.Kind.UNSIGNED_INTEGER_LITERAL, lookahead)
    consume()
    return n

  // Note: In C++, 'this' is a pointer, but in cppfront, it is not. Its unclear
  // if we can achieve the same thing in cobalt. For now, just assume it is a
  // pointer.

  def this_ (): AstNode =
    val n = AstNode(AstNode.Kind.THIS, lookahead)
    match_(Token.Kind.THIS)
    return n

  // If expressions can have complicated semantics. For example, do we allow
  // statements or only expressions? What about procedure calls, since they
  // return no value? What about blocks -- can they yield results? Thus, we
  // may only be able to implement this on a limited basis or perhaps not at
  // all. Further investigation is required. I feel like we *should* be able to
  // implement this with the same semantics as the ternary operator.

  // It appears that the C ternary operator inputs must evaluate to expressions
  // if the result is used in an expression. For example, the inputs cannot be
  // void functions.

  def ifExpression (): AstNode =
    val n = AstNode(AstNode.Kind.IF_EXPRESSION, lookahead)
    match_(Token.Kind.IF)
    match_(Token.Kind.L_PARENTHESIS)
    n.addChild(expression())
    match_(Token.Kind.R_PARENTHESIS)
    n.addChild(expression())
    match_(Token.Kind.ELSE)
    n.addChild(expression())
    return n

  def parenthesizedExpression (): AstNode =
    match_(Token.Kind.L_PARENTHESIS)
    var n = expression()
    match_(Token.Kind.R_PARENTHESIS)
    return n

  def identifier (): AstNode =
    val n = AstNode(AstNode.Kind.IDENTIFIER)
    n.setToken(lookahead)
    match_(Token.Kind.IDENTIFIER)
    return n

  // TYPES

  // Type processing is interesting because Cobalt uses a form of the
  // C-declaration style, so parsing types requires following the "spiral rule".
  // To make this easier, we make use of stack and queue types provided by the
  // language rather than complicating AST node class definition with parent
  // links. We can re-think this in the future if we wish.

  // Do we need a separate typeRoot node, or can we just use type_?

  def typeRoot (): AstNode =
    val n = AstNode(AstNode.Kind.TYPE_ROOT)
    n.addChild(type_())
    return n

  def type_ (): AstNode =
    directType()
    // Need to remove items from parsing stack and construct final type. With
    // just arrays and pointers it should be easy. Once other types are added,
    // it will require slightly more processing.
    // Stack must have at least one element, e.g. primitive type
    var n = stack.pop()
    while !stack.isEmpty do
      var p = n
      n = stack.pop()
      n.addChild(p)
    // Now trace through the type expression and print it out
    // while n.getChildCount() != 0 do
    //   println(n)
    //   if n.getKind() == AstNode.Kind.ARRAY_TYPE then
    //     n = n.getChild(1)
    //   else if n.getKind() == AstNode.Kind.POINTER_TYPE then
    //     n = n.getChild(0)
    // println(n)
    return n

  // Algorithm below is to handle the C++ spiral rule for type specifiers. The
  // type is broken into three fragments: left, consisting of pointers; center,
  // which is either a primitive type, routine pointer, nominal type, or some
  // compounded type enclosed in parenthesis; and right, consisting of arrays.
  // First, pointers are placed onto a stack. Next, the center is processed,
  // which may entail a recursive call to directType(). Then, arrays are placed
  // into a queue. Once all fragments have been created, their contents are
  // systematically moved onto the parsing stack in the order of: right, left,
  // center.
  
  // Note: Currently, we don't keep track of how many items are in the parsing
  // stack because this is the only thing it is being used for. However, due to
  // routines and templates, we might need to keep track of the count. This can
  // be done by using an "accumulator" and returning its value from
  // directType(). We could alternatively create a "combined" stack and return
  // it instead of using the parsing stack.

  // Note: The left fragment also uses a stack. Do not confuse this with the
  // parsing stack.

  def directType (): Unit =
    // Build left type fragment
    val leftFragment = Stack[AstNode]()
    while lookahead.kind == Token.Kind.ASTERISK do
      leftFragment.push(pointerType())
    // Build center type fragment
    var centerFragment: AstNode = null
    if lookahead.kind == Token.Kind.CARET then
      centerFragment = routinePointerType()
    else if
      lookahead.kind == Token.Kind.BOOL    ||
      lookahead.kind == Token.Kind.INT     ||
      lookahead.kind == Token.Kind.INT8    ||
      lookahead.kind == Token.Kind.INT16   ||
      lookahead.kind == Token.Kind.INT32   ||
      lookahead.kind == Token.Kind.INT64   ||
      lookahead.kind == Token.Kind.UINT    ||
      lookahead.kind == Token.Kind.UINT8   ||
      lookahead.kind == Token.Kind.UINT16  ||
      lookahead.kind == Token.Kind.UINT32  ||
      lookahead.kind == Token.Kind.UINT64  ||
      lookahead.kind == Token.Kind.FLOAT   ||
      lookahead.kind == Token.Kind.FLOAT32 ||
      lookahead.kind == Token.Kind.FLOAT64 ||
      lookahead.kind == Token.Kind.VOID
    then
      centerFragment = primitiveType()
    else if lookahead.kind == Token.Kind.IDENTIFIER then
      // Need to look up name in symbol table to tell what kind it is (e.g.
      // class, template). If it is defined as a class, then a left bracket
      // following indicates an array of that class type. If it is not defined
      // at all, then assume it is a class and treat it as such. If it is
      // defined as a class template, then a left bracket following denotes
      // class template parameters.
      currentScope.define(Symbol(Symbol.Kind.CLASS_TEMPLATE, "Token"))
      val symbol = currentScope.resolve(lookahead.lexeme)
      if symbol == null then
        // Nominal types include classes and enums. They do NOT include
        // primitive types or template types.
        centerFragment = nominalType()
      else
        if symbol.getKind() == Symbol.Kind.CLASS_TEMPLATE then
          centerFragment = templateType()
        else
          centerFragment = nominalType()
    else if lookahead.kind == Token.Kind.L_PARENTHESIS then
      match_(Token.Kind.L_PARENTHESIS)
      directType()
      centerFragment = stack.pop()
      match_(Token.Kind.R_PARENTHESIS)
    // Build right type fragment
    val rightFragment = Queue[AstNode]()
    while lookahead.kind == Token.Kind.L_BRACKET do
      rightFragment.enqueue(arrayType())
    // Move type fragments to parsing stack in "spiral rule" order
    while !rightFragment.isEmpty do
      stack.push(rightFragment.dequeue())
    while !leftFragment.isEmpty do
      stack.push(leftFragment.pop())
    stack.push(centerFragment)

  // The array size expression may be optional if an initializer is provided.
  // This will be checked during semantic analysis rather than during parsing.

  def arrayType (): AstNode =
    val n = AstNode(AstNode.Kind.ARRAY_TYPE, lookahead)
    match_(Token.Kind.L_BRACKET)
    if lookahead.kind != Token.Kind.R_BRACKET then
      n.addChild(expression(root=true))
    match_(Token.Kind.R_BRACKET)
    return n

  def routinePointerType (): AstNode =
    val n = AstNode(AstNode.Kind.ROUTINE_POINTER_TYPE, lookahead)
    match_(Token.Kind.CARET)
    // To do: Process parameters
    return n

  // Perhaps treat class types, enum types, and template types as kinds of
  // nominal types. Or get rid of nominal type altogether.

  def nominalType (): AstNode =
    val n = AstNode(AstNode.Kind.NOMINAL_TYPE, lookahead)
    match_(Token.Kind.IDENTIFIER)
    println("FOUND NOMINAL TYPE")
    // Need to eventually allow for type parameters. (This would allow
    // us to know that this was a class type, if that matters.)
    return n

  def pointerType (): AstNode =
    val n = AstNode(AstNode.Kind.POINTER_TYPE, lookahead)
    match_(Token.Kind.ASTERISK)
    return n

  // Here we use a somewhat generic PRIMITIVE_TYPE for the node. We can
  // distinguish with more granularity by looking at the token. If we need
  // separate AST node types for each primitive, then we can change it later.

  def primitiveType (): AstNode =
    val n = AstNode(AstNode.Kind.PRIMITIVE_TYPE, lookahead)
    match_(lookahead.kind)
    return n

  // Is a template type a kind of nominal type?

  def templateType (): AstNode =
    val n = AstNode(AstNode.Kind.TEMPLATE_TYPE, lookahead)
    match_(Token.Kind.IDENTIFIER)
    n.addChild(templateArguments())
    return n

  def templateArguments (): AstNode =
    val n = AstNode(AstNode.Kind.TEMPLATE_ARGUMENTS, lookahead)
    match_(Token.Kind.L_BRACKET)
    // There must be at least one template argument
    n.addChild(templateArgument())
    while lookahead.kind == Token.Kind.COMMA do
      match_(Token.Kind.COMMA)
      n.addChild(templateArgument())
    match_(Token.Kind.R_BRACKET)
    return n

  // For now, just support primitive and nominal types for template arguments.
  // Eventually, this needs to be expanded to pointers, references, literals,
  // templates, etc. with various caveats.

  // We might need to just use direct type here, or a special variant of direct
  // type. If we use direct type, but only some types are supported, then we
  // need to handle that with semantic analysis.

  // Todo: Do we need void here?

  def templateArgument (): AstNode =
    var n: AstNode = null
    if
      lookahead.kind == Token.Kind.BOOL    ||
      lookahead.kind == Token.Kind.INT     ||
      lookahead.kind == Token.Kind.INT8    ||
      lookahead.kind == Token.Kind.INT16   ||
      lookahead.kind == Token.Kind.INT32   ||
      lookahead.kind == Token.Kind.INT64   ||
      lookahead.kind == Token.Kind.UINT    ||
      lookahead.kind == Token.Kind.UINT8   ||
      lookahead.kind == Token.Kind.UINT16  ||
      lookahead.kind == Token.Kind.UINT32  ||
      lookahead.kind == Token.Kind.UINT64  ||
      lookahead.kind == Token.Kind.FLOAT   ||
      lookahead.kind == Token.Kind.FLOAT32 ||
      lookahead.kind == Token.Kind.FLOAT64
    then
      n = primitiveType()
    else if lookahead.kind == Token.Kind.IDENTIFIER then
      n = nominalType()
    return n



}
