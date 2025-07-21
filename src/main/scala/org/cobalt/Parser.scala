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

  val SLEEP_TIME = 200

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

  def definePrimitiveTypes () =
    builtinScope.define(Symbol(Symbol.Kind.PRIMITIVE_TYPE, "int"))
    builtinScope.define(Symbol(Symbol.Kind.PRIMITIVE_TYPE, "int8"))
    builtinScope.define(Symbol(Symbol.Kind.PRIMITIVE_TYPE, "int16"))
    builtinScope.define(Symbol(Symbol.Kind.PRIMITIVE_TYPE, "int32"))
    builtinScope.define(Symbol(Symbol.Kind.PRIMITIVE_TYPE, "int64"))
    builtinScope.define(Symbol(Symbol.Kind.PRIMITIVE_TYPE, "float"))
    builtinScope.define(Symbol(Symbol.Kind.PRIMITIVE_TYPE, "float32"))
    builtinScope.define(Symbol(Symbol.Kind.PRIMITIVE_TYPE, "float64"))

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

  def translationUnit (): AstNode =
    val n = AstNode(AstNode.Kind.TRANSLATION_UNIT)
    while lookahead.kind != Token.Kind.EOF do
      // Infinite loop, need to consume
      Thread.sleep(SLEEP_TIME)
      n.addChild(declaration())
    return n

  // DECLARATIONS

  // Template must come first before any modifiers.

  def declaration (): AstNode =
    var n: AstNode = null
    if lookahead.kind == Token.Kind.TEMPLATE then
      n = templateDeclaration()
    else
      val p = modifiers()
      n = lookahead.kind match
        case Token.Kind.CLASS => classDeclaration(p)
        case Token.Kind.ENUM  => enumerationDeclaration(p)
        case Token.Kind.DEF   => routineDeclaration(p)
        case Token.Kind.VAL   => variableDeclaration(p)
        case Token.Kind.VAR   => variableDeclaration(p)
        case _ =>
          println(s"Found something else! ${lookahead.kind}")
          null
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

  // Todo: Check best practice on whether or not we should gratuitously store
  // keyword tokens in their corresponding AST nodes. This might make sense for
  // error reporting, so that even the semantic analysis passes can trace
  // problems back to their originating column and line numbers.
  // Update: Yes, it seems we should be storing tokens in AST nodes (Parr, 81).

  def modifiers (): AstNode =
    val n = AstNode(AstNode.Kind.MODIFIERS)
    while lookahead.kind == Token.Kind.FINAL   ||
          lookahead.kind == Token.Kind.PRIVATE ||
          lookahead.kind == Token.Kind.PUBLIC  ||
          lookahead.kind == Token.Kind.STATIC
    do
      val modifier = lookahead.kind match
        case Token.Kind.FINAL   => finalModifier()
        case Token.Kind.PRIVATE => privateModifier()
        case Token.Kind.PUBLIC  => publicModifier()
        case Token.Kind.STATIC  => staticModifier()
        case _ =>
          print("error: This can only happen if there is a parser error.")
          null
      n.addChild(modifier)
    return n

  def finalModifier (): AstNode =
    // We need to define the node at the top so that we can set its token to the
    // current lookahead before it advances and we lose the chance to do so.
    val n = AstNode(AstNode.Kind.FINAL_MODIFIER, lookahead)
    match_(Token.Kind.FINAL)
    return n

  def overrideModifier (): AstNode =
    val n = AstNode(AstNode.Kind.OVERRIDE_MODIFIER, lookahead)
    match_(Token.Kind.OVERRIDE)
    return n

  def privateModifier (): AstNode =
    val n = AstNode(AstNode.Kind.PRIVATE_MODIFIER, lookahead)
    match_(Token.Kind.PRIVATE)
    return n

  def publicModifier (): AstNode =
    val n = AstNode(AstNode.Kind.PUBLIC_MODIFIER, lookahead)
    match_(Token.Kind.PUBLIC)
    return n

  def staticModifier (): AstNode =
    val n = AstNode(AstNode.Kind.STATIC_MODIFIER, lookahead)
    match_(Token.Kind.STATIC)
    return n

  // TEMPLATE DECLARATION

  def templateDeclaration (): AstNode =
    val n = AstNode(AstNode.Kind.TEMPLATE_DECLARATION, lookahead)
    match_(Token.Kind.TEMPLATE)
    n.addChild(templateParameters())
    val p = modifiers()
    val q = lookahead.kind match
      case Token.Kind.CLASS => classDeclaration(p)
      case Token.Kind.DEF   => routineDeclaration(p)
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

  def classDeclaration (modifiers: AstNode): AstNode =
    val n = AstNode(AstNode.Kind.CLASS_DECLARATION, lookahead)
    match_(Token.Kind.CLASS)
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

  def classMember (): AstNode =
    val p = modifiers()
    val n = lookahead.kind match
      case Token.Kind.DEF => methodDeclaration(p)
      // case Token.Kind.VAL => fieldDeclaration(p)
      // case Token.Kind.VAR => fieldDeclaration(p)
      case _ => 
          print("error: This can only happen if there is a parser error.")
          null
    return n

  def methodDeclaration (modifiers: AstNode): AstNode =
    val n = AstNode(AstNode.Kind.METHOD_DECLARATION, lookahead)
    match_(Token.Kind.DEF)
    n.addChild(modifiers)
    n.addChild(name())
    n.addChild(routineParameters())
    n.addChild(result())
    n.addChild(methodBody())
    return n

  // Here we might not be able to set a token on the method body. This raises
  // the question -- should any body nodes be associated with punctuation like
  // curly braces?

  // Can a semicolon properly represent an abstract method? Or would that only
  // signify a "noop" method?

  def methodBody (): AstNode =
    val n = AstNode(AstNode.Kind.METHOD_BODY)
    if lookahead.kind == Token.Kind.SEMICOLON then
      match_(Token.Kind.SEMICOLON)
    else
      n.addChild(block())
    return n

  def enumerationDeclaration (modifiers: AstNode): AstNode =
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

  def routineDeclaration (modifiers: AstNode): AstNode =
    val n = AstNode(AstNode.Kind.ROUTINE_DECLARATION)
    match_(Token.Kind.DEF)
    n.addChild(modifiers)
    n.addChild(routineName())
    n.addChild(routineParameters())
    n.addChild(result())
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

  // Todo: Routine parameters includes parenthesis, check consistency of other
  // parameter rules.

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

  // Shoud name() be parameterName()?

  def routineParameter (): AstNode =
    val n = AstNode(AstNode.Kind.ROUTINE_PARAMETER)
    n.addChild(name())
    match_(Token.Kind.COLON)
    n.addChild(typeRoot())
    return n

  // We need to decide if we want to use an arrow or a colon for the result
  // type. C++, python, ruby, swift, ocaml, haskell, and rust all use an arrow,
  // while scala, kotlin, typescript, and pascal all use a colon. The proper
  // choice probably depends on whether or not the language in question already
  // uses colons and/or arrows for other things (and the amount that the symbol
  // appears in the program text); and whether it would lead to grammar
  // ambiguities. For now, we will use an arrow.

  def result (): AstNode =
    val n = AstNode(AstNode.Kind.RESULT)
    if lookahead.kind == Token.Kind.MINUS_GREATER then
      match_(Token.Kind.MINUS_GREATER)
      n.addChild(typeRoot())
    return n

  // If no routine body is specified, then this is simply a forward declaration.
  // It is not clear at this time whether forward declarations of routines are
  // ever required in cobalt, but we can keep the option open for now.

  def routineBody (): AstNode =
    val n = AstNode(AstNode.Kind.ROUTINE_BODY)
    if lookahead.kind == Token.Kind.SEMICOLON then
      match_(Token.Kind.SEMICOLON)
    else
      n.addChild(block())
    return n

  // The top-most block needs to use the scope of the routine itself
  // so we might need a topBlock production. Alternatively, we can
  // pass in a parameter that says whether or not to create a new
  // scope.

  def block (): AstNode =
    val n = AstNode(AstNode.Kind.BLOCK)
    match_(Token.Kind.L_BRACE)
    while lookahead.kind != Token.Kind.R_BRACE do
      Thread.sleep(SLEEP_TIME)
      // Do blocks only contain statements? If so, then we don't need
      // blockElement. Otherwise, if we full distinguish between
      // declarations and statements, then we need blockElement.
      n.addChild(statement())
    match_(Token.Kind.R_BRACE)
    return n

  // VARIABLE DECLARATION

  def variableDeclaration (modifiers: AstNode): AstNode =
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
      n.addChild(expression())
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

  def statement (): AstNode =
    val kind = lookahead.kind
    var n: AstNode = null
    if kind == Token.Kind.IDENTIFIER ||
       kind == Token.Kind.THIS       ||
       literalFirstSet.contains(kind)
    then
      n = expressionStatement()
    else if kind == Token.Kind.BREAK then
      n = breakStatement()
    else if kind == Token.Kind.CONTINUE then
      n = continueStatement()
    else if kind == Token.Kind.DO then
      n = doStatement()
    // else if kind == Token.Kind.FOR then
    //   n = forStatement()
    // else if kind == Token.Kind.IF then
    //   n = ifStatement()
    else if kind == Token.Kind.SEMICOLON then
      n = nullStatement()
    else if kind == Token.Kind.RETURN then
      n = returnStatement()
    else if kind == Token.Kind.UNTIL then
      n = untilStatement()
    else if kind == Token.Kind.WHILE then
      n = whileStatement()
    // else if kind == Token.Kind.VAL || kind == Token.Kind.VAR then
    //   n = declarationStatement()
    else
      print("Error: Invalid statement")
    return n

  def breakStatement (): AstNode =
    val n = AstNode(AstNode.Kind.BREAK_STATEMENT)
    match_(Token.Kind.BREAK)
    match_(Token.Kind.SEMICOLON)
    return n

  def continueStatement (): AstNode =
    val n = AstNode(AstNode.Kind.CONTINUE_STATEMENT)
    match_(Token.Kind.CONTINUE)
    match_(Token.Kind.SEMICOLON)
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

  // We would like to eventually add "foreach" support. This is a bit tricky
  // because our LL(1) grammar cannot look that far ahead to tell whether it is
  // a for statement or a foreach statement. LL(k) doesn't help because the
  // first expression could be arbitrarily long. Although it is extremely
  // common for it to be something very simple like "i: int = 0", we cannot
  // rule out the possibility of it being a much more complex expression. We
  // do not want to introduce backtracking just for this purpose. Therefore, we
  // will probably split the rule into two parts later on in order to add the
  // required support. Alternatively, we could add a "foreach" keyword, but I
  // really don't want to do that.

  def forStatement (): AstNode =
    val n = AstNode(AstNode.Kind.FOR_STATEMENT, lookahead)
    match_(Token.Kind.FOR)
    match_(Token.Kind.L_PARENTHESIS)
    // Technically, these expressions can be empty. Perhaps this is why null
    // statements are really expressions. We can research that and tidy up the
    // grammar later.
    n.addChild(expression())
    match_(Token.Kind.SEMICOLON)
    n.addChild(expression())
    match_(Token.Kind.SEMICOLON)
    n.addChild(expression())
    match_(Token.Kind.R_PARENTHESIS)
    if lookahead.kind == Token.Kind.L_BRACE then
      n.addChild(block())
    else
      n.addChild(statement())
    return n


  // Note: Null statements may be a type of expression statement under C++
  // rules. I am not sure how much sense that makes because an expression
  // statement should presumably evaluate to some value, but a null statement
  // does not. (On the other hand, a procedure call is considered an expression
  // statement and also does not evaluate to a value.)

  // A null statement could theoretically produce no AST node at all, since it
  // is a "noop". However, this may be useful to do because it will provide a
  // more faithful translation to C++. It will be optimized out by C++ anyways.

  def nullStatement (): AstNode =
    val n = AstNode(AstNode.Kind.NULL_STATEMENT, lookahead)
    match_(Token.Kind.SEMICOLON)
    return n

  def returnStatement (): AstNode =
    print("GOT RETURN STATEMENT")
    val n = AstNode(AstNode.Kind.RETURN_STATEMENT, lookahead)
    // Todo: Allow optional expression to be provided to return statement
    // n.addChild(expression())
    match_(Token.Kind.SEMICOLON)
    return n

  def untilStatement (): AstNode =
    val n = AstNode(AstNode.Kind.UNTIL_STATEMENT, lookahead)
    match_(Token.Kind.UNTIL)
    match_(Token.Kind.L_PARENTHESIS)
    // Not sure if this is always an expression or something else that is
    // usually an expression.
    n.addChild(expression())
    match_(Token.Kind.R_PARENTHESIS)
    if lookahead.kind == Token.Kind.L_BRACE then
      n.addChild(block())
    else
      n.addChild(statement())
    return n

  def whileStatement (): AstNode =
    val n = AstNode(AstNode.Kind.WHILE_STATEMENT, lookahead)
    match_(Token.Kind.WHILE)
    match_(Token.Kind.L_PARENTHESIS)
    // Not sure if this is always an expression or something else that is
    // usually an expression.
    n.addChild(expression())
    match_(Token.Kind.R_PARENTHESIS)
    if lookahead.kind == Token.Kind.L_BRACE then
      n.addChild(block())
    else
      n.addChild(statement())
    return n

  def expressionStatement(): AstNode =
    return null

  // EXPRESSIONS

  // Do we need an expression root AST node?

  def expressionRoot (): AstNode =
    println("EXPRESSION_ROOT")
    val n = AstNode(AstNode.Kind.EXPRESSION_ROOT)
    n.addChild(expression())
    return n

  def expression (): AstNode =
    val n = assignmentExpression()
    return n

  // Note: It is inefficient to define first sets inside of each method because
  // these methods may be called over and over again, resulting in repeated
  // creation of first sets, which all contain constant information. However, it
  // is convenient to define them like so. We can optimize this later.

  def assignmentExpression (): AstNode =
    var n = logicalOrExpression()
    val firstSet = Set(
      Token.Kind.EQUAL,
      Token.Kind.ASTERISK_EQUAL,
      Token.Kind.SLASH_EQUAL,
      Token.Kind.PERCENT_EQUAL,
      Token.Kind.PLUS_EQUAL,
      Token.Kind.MINUS_EQUAL,
      Token.Kind.LESS_LESS_EQUAL,
      Token.Kind.GREATER_GREATER_EQUAL,
      Token.Kind.AMPERSAND_EQUAL,
      Token.Kind.CARET_EQUAL,
      Token.Kind.BAR_EQUAL
    )
    while firstSet.contains(lookahead.kind) do
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
    val firstSet = Set(
      Token.Kind.EQUAL_EQUAL,
      Token.Kind.EXCLAMATION_EQUAL
    )
    while firstSet.contains(lookahead.kind) do
      var p = n
      n = AstNode(AstNode.Kind.BINARY_EXPRESSION, lookahead)
      n.addChild(p)
      match_(lookahead.kind)
      n.addChild(relationalExpression())
    return n

  def relationalExpression (): AstNode =
    var n = shiftExpression()
    val firstSet = Set(
      Token.Kind.GREATER,
      Token.Kind.LESS,
      Token.Kind.GREATER_EQUAL,
      Token.Kind.LESS_EQUAL
    )
    while firstSet.contains(lookahead.kind) do
      var p = n
      n = AstNode(AstNode.Kind.BINARY_EXPRESSION, lookahead)
      n.addChild(p)
      match_(lookahead.kind)
      n.addChild(shiftExpression())
    return n

  def shiftExpression (): AstNode =
    var n = additiveExpression()
    val firstSet = Set(
      Token.Kind.GREATER_GREATER,
      Token.Kind.LESS_LESS
    )
    while firstSet.contains(lookahead.kind) do
      var p = n
      n = AstNode(AstNode.Kind.BINARY_EXPRESSION, lookahead)
      n.addChild(p)
      match_(lookahead.kind)
      n.addChild(additiveExpression())
    return n

  def additiveExpression (): AstNode =
    var n = multiplicativeExpression()
    val firstSet = Set(
      Token.Kind.PLUS,
      Token.Kind.MINUS
    )
    while firstSet.contains(lookahead.kind) do
      var p = n
      n = AstNode(AstNode.Kind.BINARY_EXPRESSION, lookahead)
      n.addChild(p)
      match_(lookahead.kind)
      n.addChild(multiplicativeExpression())
    return n

  def multiplicativeExpression (): AstNode =
    var n = unaryExpression()
    val firstSet = Set(
      Token.Kind.ASTERISK,
      Token.Kind.SLASH,
      Token.Kind.PERCENT
    )
    while firstSet.contains(lookahead.kind) do
      var p = n
      n = AstNode(AstNode.Kind.BINARY_EXPRESSION, lookahead)
      n.addChild(p)
      match_(lookahead.kind)
      n.addChild(unaryExpression())
    return n
  
  // Why recursion here instead of iteration? Does it matter?

  def unaryExpression (): AstNode =
    var n: AstNode = null
    // TODO: Need to add tilde
    val firstSet = Set(
      Token.Kind.ASTERISK,
      Token.Kind.MINUS,
      Token.Kind.PLUS,
      Token.Kind.EXCLAMATION
    )
    if firstSet.contains(lookahead.kind) then
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
    val firstSet = Set(
      Token.Kind.MINUS_GREATER,
      Token.Kind.PERIOD,
      Token.Kind.L_PARENTHESIS,
      Token.Kind.L_BRACKET
    )
    while firstSet.contains(lookahead.kind) do
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

  def ifExpression (): AstNode =
    val n = AstNode(AstNode.Kind.IF_EXPRESSION, lookahead)
    match_(Token.Kind.IF)
    if lookahead.kind == Token.Kind.L_PARENTHESIS then
      match_(Token.Kind.L_PARENTHESIS)
      n.addChild(expression())
      match_(Token.Kind.R_PARENTHESIS)
    else
      n.addChild(expression())
      match_(Token.Kind.THEN)
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
    println("BEGIN STACK")
    // Stack must have at least one element, e.g. primitive type
    var n = stack.pop()
    while !stack.isEmpty do
      var p = n
      n = stack.pop()
      n.addChild(p)
    println("END STACK")
    // // Now trace through the type expression and print it out
    while n.getChildCount() != 0 do
      println(n)
      if n.getKind() == AstNode.Kind.ARRAY_TYPE then
        n = n.getChild(1)
      else if n.getKind() == AstNode.Kind.POINTER_TYPE then
        n = n.getChild(0)
    println(n)
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
      // Nominal type. Need to look up name in symbol table to tell
      // what kind it is (e.g. struct, class). For now assume class.
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
      n.addChild(expressionRoot())
    match_(Token.Kind.R_BRACKET)
    return n

  def routinePointerType (): AstNode =
    val n = AstNode(AstNode.Kind.ROUTINE_POINTER_TYPE, lookahead)
    match_(Token.Kind.CARET)
    // To do: Process parameters
    return n

  def nominalType (): AstNode =
    val n = AstNode(AstNode.Kind.NOMINAL_TYPE, lookahead)
    // I don't think we want to add a name here, we just want to set
    // the token instead, but we can revisit this later.
    // n.add_child(self.name())
    match_(Token.Kind.IDENTIFIER)
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

}
