package org.cobalt

class Lexer {

  // Define end of file (EOF)
  val EOF = (-1).toChar

  var input = ""
  var position = 0
  var current = EOF
  var line = 1
  var column = 1

  val keywordLookup = Map (
    "abstract" -> Token.Kind.ABSTRACT,
    "and" -> Token.Kind.AND,
    "break" -> Token.Kind.BREAK,
    "case" -> Token.Kind.CASE,
    "cast" -> Token.Kind.CAST,
    "catch" -> Token.Kind.CATCH,
    "class" -> Token.Kind.CLASS,
    "const" -> Token.Kind.CONST,
    "constexpr" -> Token.Kind.CONSTEXPR,
    "continue" -> Token.Kind.CONTINUE,
    "def" -> Token.Kind.DEF,
    "default" -> Token.Kind.DEFAULT,
    "delete" -> Token.Kind.DELETE,
    "divine" -> Token.Kind.DIVINE,
    "do" -> Token.Kind.DO,
    "else" -> Token.Kind.ELSE,
    "enum" -> Token.Kind.ENUM,
    "extends" -> Token.Kind.EXTENDS,
    "false" -> Token.Kind.FALSE,
    "final" -> Token.Kind.FINAL,
    "for" -> Token.Kind.FOR,
    "foreach" -> Token.Kind.FOREACH,
    "fn" -> Token.Kind.FN,
    "fun" -> Token.Kind.FUN,
    "if" -> Token.Kind.IF,
    "in" -> Token.Kind.IN,
    "nil" -> Token.Kind.NIL,
    "null" -> Token.Kind.NULL,
    "or" -> Token.Kind.OR,
    "override" -> Token.Kind.OVERRIDE,
    "package" -> Token.Kind.PACKAGE,
    "private" -> Token.Kind.PRIVATE,
    "public" -> Token.Kind.PUBLIC,
    "return" -> Token.Kind.RETURN,
    "static" -> Token.Kind.STATIC,
    "struct" -> Token.Kind.STRUCT,
    "template" -> Token.Kind.TEMPLATE,
    "transmute" -> Token.Kind.TRANSMUTE,
    "true" -> Token.Kind.TRUE,
    "try" -> Token.Kind.TRY,
    "typealias" -> Token.Kind.TYPEALIAS,
    "union" -> Token.Kind.UNION,
    "until" -> Token.Kind.UNTIL,
    "using" -> Token.Kind.USING,
    "val" -> Token.Kind.VAL,
    "var" -> Token.Kind.VAR,
    "virtual" -> Token.Kind.VIRTUAL,
    "volatile" -> Token.Kind.VOLATILE,
    "while" -> Token.Kind.WHILE,

    "int" -> Token.Kind.INT,
    "int8" -> Token.Kind.INT8,
    "int16" -> Token.Kind.INT16,
    "int32" -> Token.Kind.INT32,
    "int64" -> Token.Kind.INT64,

    "uint" -> Token.Kind.UINT,
    "uint8" -> Token.Kind.UINT8,
    "uint16" -> Token.Kind.UINT16,
    "uint32" -> Token.Kind.UINT32,
    "uint64" -> Token.Kind.UINT64,

    "float" -> Token.Kind.FLOAT,
    "float32" -> Token.Kind.FLOAT32,
    "float64" -> Token.Kind.FLOAT64,

    "void" -> Token.Kind.VOID
  )

  def setInput (input: String) =
    this.input = input
    if (input.length > 0)
      current = input(position)

  def error (message: String) =
    val coords = s"(${line},${column})"
    println(s"${coords}: error: ${message}")

  def consume () =
    position += 1
    current = if position < input.length then input(position) else EOF
    column += 1

  def backup () =
    position -= 1
    current = input(position)
    column -= 1

  def process (): List[Token] =
    // We might eventually require a client to perform the
    // construction of this list. For now just have the lexer do it.
    var tokens = List[Token]()
    var token = getToken()
    tokens = token :: tokens
    while token.kind != Token.Kind.EOF do
      token = getToken()
      tokens = token :: tokens
    tokens = tokens.reverse
    return tokens

  // Todo: For multi-character lexemes, we need to ensure that the column is
  // calculated as the start of the lexeme, not the end.

  def getToken (): Token =

    var kind: Token.Kind = null
    var lexeme = ""

    while current != EOF do

      if current == '=' then
        consume()
        if current == '=' then
          consume()
          kind = Token.Kind.EQUAL_X2
          lexeme = "=="
        else
          kind = Token.Kind.EQUAL
          lexeme = "="
        return Token(kind, lexeme, position, line, column)

      else if current == '|' then
        consume()
        if current == '|' then
          consume()
          kind = Token.Kind.BAR_BAR
          lexeme = "||"
        else if current == '=' then
          consume()
          kind = Token.Kind.BAR_EQUAL
          lexeme = "|="
        else
          kind = Token.Kind.BAR
          lexeme = "|"
        return Token(kind, lexeme, position, line, column)

      else if current == '^' then
        consume()
        if current == '=' then
          consume()
          kind = Token.Kind.CARET_EQUAL
          lexeme = "^="
        else
          kind = Token.Kind.CARET
          lexeme = "^"
        return Token(kind, lexeme, position, line, column)

      else if current == '&' then
        consume()
        if current == '&' then
          consume()
          kind = Token.Kind.AMPERSAND_AMPERSAND
          lexeme = "&&"
        else if current == '=' then
          consume()
          kind = Token.Kind.AMPERSAND_EQUAL
          lexeme = "&="
        else
          kind = Token.Kind.AMPERSAND
          lexeme = "&"
        return Token(kind, lexeme, position, line, column)

      else if current == '>' then
        consume()
        if current == '>' then
          consume()
          if current == '=' then
            consume()
            kind = Token.Kind.GREATER_GREATER_EQUAL
            lexeme = ">>="
          else
            kind = Token.Kind.GREATER_GREATER
            lexeme = ">>"
        else if current == '=' then
          consume()
          kind = Token.Kind.GREATER_EQUAL
          lexeme = ">="
        else
          kind = Token.Kind.GREATER
          lexeme = ">"
        return Token(kind, lexeme, position, line, column)

      else if current == '<' then
        consume()
        if current == '<' then
          consume()
          if current == '=' then
            consume()
            kind = Token.Kind.LESS_LESS_EQUAL
            lexeme = "<<="
          else
            kind = Token.Kind.LESS_LESS
            lexeme = "<<"
        else if current == '=' then
          consume()
          kind = Token.Kind.LESS_EQUAL
          lexeme = "<="
        else
          kind = Token.Kind.LESS
          lexeme = "<"
        return Token(kind, lexeme, position, line, column)

      else if current == '+' then
        consume()
        if current == '=' then
          consume()
          kind = Token.Kind.PLUS_EQUAL
          lexeme = "+="
        else
          kind = Token.Kind.PLUS
          lexeme = "+"
        return Token(kind, lexeme, position, line, column)

      else if current == '-' then
        consume()
        if current == '>' then
          consume()
          kind = Token.Kind.MINUS_GREATER
          lexeme = "->"
        else if current == '=' then
          consume()
          kind = Token.Kind.MINUS_EQUAL
          lexeme = "-="
        else
          kind = Token.Kind.MINUS
          lexeme = "-"
        return Token(kind, lexeme, position, line, column)

      else if current == '*' then
        consume()
        if current == '=' then
          consume()
          kind = Token.Kind.ASTERISK_EQUAL
          lexeme = "*="
        else
          kind = Token.Kind.ASTERISK
          lexeme = "*"
        return Token(kind, lexeme, position, line, column)

      // To do: Need to account for comments
      else if current == '/' then
        consume()
        if current == '=' then
          consume()
          kind = Token.Kind.SLASH_EQUAL
          lexeme = "/="
          return Token(kind, lexeme, position, line, column)
        else if current == '/' then
          // Line comment
          consume()
          while current != '\n' && current != '\r' && current != EOF do
            consume()
        else
          kind = Token.Kind.SLASH
          lexeme = "/"
          return Token(kind, lexeme, position, line, column)

      else if current == '%' then
        consume()
        if current == '=' then
          consume()
          kind = Token.Kind.PERCENT_EQUAL
          lexeme = "%="
        else
          kind = Token.Kind.PERCENT
          lexeme = "%"
        return Token(kind, lexeme, position, line, column)

      else if current == '!' then
        consume()
        if current == '=' then
          consume()
          kind = Token.Kind.EXCLAMATION_EQUAL
          lexeme = "!="
        else
          kind = Token.Kind.EXCLAMATION
          lexeme = "!"
        return Token(kind, lexeme, position, line, column)
      
      else if current == '~' then
        consume()
        if current == '=' then
          consume()
          kind = Token.Kind.TILDE_EQUAL
          lexeme = "~="
        else
          kind = Token.Kind.TILDE
          lexeme = "~"
        return Token(kind, lexeme, position, line, column)

      else if current == '"' then
        // String
        val begin = position
        consume()
        while current != '"' && current != EOF do
          // Might need to put some logic in here to increment line
          // number and reset position if a newline is encountered
          consume()
        if current == '"' then
          consume()
          val end = position
          val value = input.slice(begin, end)
          return Token(Token.Kind.STRING_LITERAL, value, position, line, column)
        else if current == EOF then
          // To do: probably should pretend terminator is there and return token
          print("error: missing string terminator")

      else if current == '\'' then
        // Character
        val begin = position
        consume()
        while current != '\'' && current != EOF do
          // Might need to put some logic in here to increment line
          // number and reset position if a newline is encountered
          consume()
        if current == '\'' then
          consume()
          val end = position
          val value = input.slice(begin, end)
          return Token(Token.Kind.CHARACTER_LITERAL, value, position, line, column)
        else if current == EOF then
          // To do: probably should pretend terminator is there and return token
          print("error: missing character terminator")

      else if current == ':' then
        consume()
        return Token(Token.Kind.COLON, ":", position, line, column)

      else if current == ';' then
        consume()
        return Token(Token.Kind.SEMICOLON, ";", position, line, column)

      else if current == '.' then
        consume()
        if current == '.' then
          consume()
          return Token(Token.Kind.PERIOD_PERIOD, "..", position, line, column)
        else if current.isDigit then
          return number()
        else
          return Token(Token.Kind.PERIOD, ".", position, line, column)

      else if current == ',' then
        consume()
        return Token(Token.Kind.COMMA, ",", position, line, column)

      else if current == '{' then
        consume()
        return Token(Token.Kind.L_BRACE, "{", position, line, column)

      else if current == '}' then
        consume()
        return Token(Token.Kind.R_BRACE, "}", position, line, column)

      else if current == '[' then
        consume()
        return Token(Token.Kind.L_BRACKET, "[", position, line, column)

      else if current == ']' then
        consume()
        return Token(Token.Kind.R_BRACKET, "]", position, line, column)

      else if current == '(' then
        consume()
        return Token(Token.Kind.L_PARENTHESIS, ")", position, line, column)

      else if current == ')' then
        consume()
        return Token(Token.Kind.R_PARENTHESIS, ")", position, line, column)

      else if current == '0' then
        consume()
        if current == 'b' then
          backup()
          return binaryInteger()
        else if current == 'o' then
          backup()
          return octalInteger()
        else if current == 'x' then
          backup()
          return hexadecimalNumber()
        else
          backup()
          return number()

      else if current == ' ' || current == '\t' then
        // Skip spaces and tabs
        while current == ' ' || current == '\t' do
          consume()

      else if current == '\n' then
        // Skip line feed (LF) characters
        while current == '\n' do
          consume()
          line += 1
          column = 0

      else if current == '\r' then
        // skip carriage return + line feed (CR+LF) pairs
        while current == '\r' do
          consume()
          if current == '\n' then
            consume()
            line += 1
            column = 0
          else
            // Should return error token here maybe
            // Found carriage return by itself, which is invalid (except on mac?)
            print("error: invalid line ending")

      else if current.isLetter || current == '_' then
        val begin = position
        consume()
        while (position < input.length) && (current.isLetter || current.isDigit || current == '_') do
          consume()
        // End index of slice is excluded from result
        val end = position
        lexeme = input.slice(begin, end)
        kind = if keywordLookup.contains(lexeme) then keywordLookup(lexeme) else Token.Kind.IDENTIFIER
        return Token(kind, lexeme, position, line, column - lexeme.length() + 1)

      else if current.isDigit then
        return number()
      
      else
        print("ERROR!")

    end while

    // Placeholder to avoid error
    return Token(Token.Kind.EOF, "<EOF>", position, line, column)

  end getToken

  def isBinaryDigit (ch: Char) =
    ch == '0' || ch == '1'

  def isOctalDigit (ch: Char) =
    ch >= '0' && ch <= '7'

  def isDecimalDigit (ch: Char) =
    ch >= '0' && ch <= '9'

  def isHexadecimalDigit (ch: Char) =
    (ch >= '0' && ch <= '9') || (ch >= 'A' && ch <= 'Z') || (ch >= 'z' && ch <= 'z')

  def binaryInteger (): Token =
    // Note: We arrive at this function after lookahead or
    // backtracking, so we really should never fail to match the '0b'
    // portion, unless there is a bug in this program.
    val begin = position
    var state = State.BIN_START
    var token: Token = null
    while token == null do
      state match
        case State.BIN_START =>
          if current == '0' then
            consume()
            state = State.BIN_100
          else
            state = State.BIN_ERROR
        case State.BIN_100 =>
          if current == 'b' then
            consume()
            state = State.BIN_200
          else
            state = State.BIN_ERROR
        case State.BIN_200 =>
          consume()
          if isBinaryDigit(current) then
            consume()
            state = State.BIN_400
          else if current == '_' then
            consume()
            state = State.BIN_300
          else
            // Pretend we got a digit or underscore for error recovery purposes
            error(s"invalid number: found '${current}', expected binary digit or underscore")
            consume()
            state = State.BIN_400
        case State.BIN_300 =>
          if isBinaryDigit(current) then
            consume()
            state = State.BIN_400
          else
            // Pretend we got a digit for error recovery purposes
            error(s"invalid number: found '${current}', expected binary digit")
            consume()
            state = State.BIN_400
        case State.BIN_400 =>
          if isBinaryDigit(current) then
            consume()
          else if current == '_' then
            consume()
            state = State.BIN_500
          else if current == 'L' then
            consume()
            state = State.BIN_600
          else if current == 'u' then
            consume()
            state = State.BIN_700
          else
            // Accept
            val end = position
            val lexeme = input.slice(begin, end)
            token = Token(Token.Kind.BINARY_INT32_LITERAL, lexeme, position, line, column)
        case State.BIN_500 =>
          if isBinaryDigit(current) then
            consume()
            state = State.BIN_400
          else if current == 'L' then
            consume()
            state = State.BIN_600
          else if current == 'u' then
            consume()
            state = State.BIN_700
          else
            // Pretend we got a digit for error recovery purposes
            error(s"invalid number: found '${current}', expected binary digit")
            consume()
            state = State.BIN_400
        case State.BIN_600 =>
          if current == 'u' then
            consume()
            state = State.BIN_800
          else
            // Accept
            val end = position
            val lexeme = input.slice(begin, end)
            token = Token(Token.Kind.BINARY_INT64_LITERAL, lexeme, position, line, column)
        case State.BIN_700 =>
          if current == 'L' then
            consume()
            state = State.BIN_800
          else
            // Accept
            val end = position
            val lexeme = input.slice(begin, end)
            token = Token(Token.Kind.BINARY_UINT32_LITERAL, lexeme, position, line, column)
        case State.BIN_800 =>
          // Accept
          val end = position
          val lexeme = input.slice(begin, end)
          token = Token(Token.Kind.BINARY_UINT64_LITERAL, lexeme, position, line, column)
        case _ =>
          // Invalid state. Can only be reached through a lexer bug.
          print("error: Invalid state.")
    end while
    return token

  def octalInteger (): Token =
    // Note: We arrive at this function after lookahead or
    // backtracking, so we really should never fail to match the '0o'
    // portion, unless there is a bug in this program.
    val begin = position
    var state = State.OCT_START
    var token: Token = null
    while token == null do
      state match
        case State.OCT_START =>
          if current == '0' then
            consume()
            state = State.OCT_100
          else
            state = State.OCT_ERROR
        case State.OCT_100 =>
          if current == 'o' then
            consume()
            state = State.OCT_200
          else
            state = State.OCT_ERROR
        case State.OCT_200 =>
          if isOctalDigit(current) then
            consume()
            state = State.OCT_400
          else if current == '_' then
            consume()
            state = State.OCT_300
          else
            // Pretend we got a digit or underscore for error recovery purposes
            error(s"invalid number: found '${current}', expected octal digit or underscore")
            consume()
            state = State.OCT_400
        case State.OCT_300 =>
          if isOctalDigit(current) then
            consume()
            state = State.OCT_400
        case State.OCT_400 =>
          if isOctalDigit(current) then
            consume()
          else if current == '_' then
            consume()
            state = State.OCT_500
          else if current == 'L' then
            consume()
            state = State.OCT_600
          else if current == 'u' then
            consume()
            state = State.OCT_700
          else
            // Accept
            val end = position
            val lexeme = input.slice(begin, end)
            token = Token(Token.Kind.OCTAL_INT32_LITERAL, lexeme, position, line, column)
        case State.OCT_500 =>
          if isOctalDigit(current) then
            consume()
            state = State.OCT_400
          else if current == 'L' then
            consume()
            state = State.OCT_600
          else if current == 'u' then
            consume()
            state = State.OCT_700
          else
            // Pretend we got a digit for error recovery purposes
            error(s"invalid number: found '${current}', expected octal digit")
            consume()
            state = State.OCT_400
        case State.OCT_600 =>
          if current == 'u' then
            consume()
            state = State.OCT_800
          else
            // Accept
            val end = position
            val lexeme = input.slice(begin, end)
            token = Token(Token.Kind.OCTAL_INT64_LITERAL, lexeme, position, line, column)
        case State.OCT_700 =>
          if current == 'L' then
            consume()
            state = State.OCT_800
          else
            // Accept
            val end = position
            val lexeme = input.slice(begin, end)
            token = Token(Token.Kind.OCTAL_UINT32_LITERAL, lexeme, position, line, column)
        case State.OCT_800 =>
          // Accept
          val end = position
          val lexeme = input.slice(begin, end)
          token = Token(Token.Kind.OCTAL_UINT64_LITERAL, lexeme, position, line, column)
        case _ =>
          // Invalid state. Can only be reached through a lexer bug.
          print("error: Invalid state.")
    end while
    return token

  def hexadecimalNumber (): Token =
    // This scans for a hexadecimal integer or floating point number.
    val begin = position
    var state = State.HEX_START
    var token: Token = null
    while token == null do
      state match
        case State.HEX_START =>
          if current == '0' then
            consume()
            state = State.HEX_10
          else
            state = State.HEX_ERROR
        case State.HEX_10 =>
          if current == 'x' then
            consume()
            state = State.HEX_20
          else
            state = State.HEX_ERROR
        case State.HEX_20 =>
          if isHexadecimalDigit(current) then
            consume()
            state = State.HEX_100
          else if current == '_' then
            consume()
            state = State.HEX_30
          else if current == '.' then
            consume()
            state = State.HEX_300
          else
            // Pretend we got a digit, dot, or underscore for error recovery purposes
            error(s"invalid number: found '${current}', expected hexadecimal digit, dot, or underscore")
            consume()
            state = State.HEX_100
        case State.HEX_30 =>
          if isHexadecimalDigit(current) then
            consume()
            state = State.HEX_100
          else
            // Pretend we got a digit for error recovery purposes
            error(s"invalid number: found '${current}', expected hexadecimal digit")
            consume()
            state = State.HEX_100
        case State.HEX_100 =>
          if isHexadecimalDigit(current) then
            consume()
          else if current == '_' then
            consume()
            state = State.HEX_200
          else if current == 'L' then
            consume()
            state = State.HEX_210
          else if current == 'u' then
            consume()
            state = State.HEX_220
          else if current == '.' then
            consume()
            state = State.HEX_300
          else if current == 'p' then
            consume()
            state = State.HEX_600
          else
            // Accept
            val end = position
            val lexeme = input.slice(begin, end)
            token = Token(Token.Kind.HEXADECIMAL_INT32_LITERAL, lexeme, position, line, column)
        case State.HEX_200 =>
          if isHexadecimalDigit(current) then
            consume()
            state = State.HEX_100
          else if current == 'L' then
            consume()
            state = State.HEX_210
          else if current == 'u' then
            consume()
            state = State.HEX_220
          else if current == 'p' then
            consume()
            state = State.HEX_600
          else
            // Pretend we got a digit for error recovery purposes
            error(s"invalid number: found '${current}', expected 'L', 'u', 'p', or hexadecimal digit")
            consume()
            state = State.HEX_100
        case State.HEX_210 =>
          if current == 'u' then
            consume()
            state = State.HEX_230
          else
            // Accept
            val end = position
            val lexeme = input.slice(begin, end)
            token = Token(Token.Kind.HEXADECIMAL_INT64_LITERAL, lexeme, position, line, column)
        case State.HEX_220 =>
          if current == 'L' then
            consume()
            state = State.HEX_230
          else
            // Accept
            val end = position
            val lexeme = input.slice(begin, end)
            token = Token(Token.Kind.HEXADECIMAL_UINT32_LITERAL, lexeme, position, line, column)
        case State.HEX_230 =>
          // Accept
          val end = position
          val lexeme = input.slice(begin, end)
          token = Token(Token.Kind.HEXADECIMAL_UINT64_LITERAL, lexeme, position, line, column)
        case State.HEX_300 =>
          if isHexadecimalDigit(current) then
            consume()
            state = State.HEX_400
          else
            // Pretend we got a digit for error recovery purposes
            error(s"invalid number: found '${current}', expected hexadecimal digit")
            consume()
            // Do we need to change states here?
        case State.HEX_400 =>
          if isHexadecimalDigit(current) then
            consume()
          else if current == '_' then
            consume()
            state = State.HEX_500
          else if current == 'p' then
            consume()
            state = State.HEX_600
          else
            val end = position
            val lexeme = input.slice(begin, end)
            token = Token(Token.Kind.HEXADECIMAL_FLOAT64_LITERAL, lexeme, position, line, column)
        case State.HEX_500 =>
          if isHexadecimalDigit(current) then
            consume()
            state = State.HEX_400
          else if current == 'p' then
            consume()
            state = State.HEX_600
          else
            // Pretend we got a digit for error recovery purposes
            error(s"invalid number: found '${current}', expected 'p' or hexadecimal digit")
            consume()
            state = State.HEX_400
        case State.HEX_600 =>
          if isDecimalDigit(current) then
            consume()
            state = State.HEX_800
          else if current == '+' || current == '-' then
            consume()
            state = State.HEX_700
          else
            // Pretend we got a digit for error recovery purposes
            error(s"invalid number: found '${current}', expected '+', '-', or decimal digit")
            consume()
            state = State.HEX_800
        case State.HEX_700 =>
          if isDecimalDigit(current) then
            consume()
            state = State.HEX_800
          else
            // Pretend we got a digit for error recovery purposes
            error(s"invalid number: found '${current}', expected decimal digit")
            consume()
            state = State.HEX_800
        case State.HEX_800 =>
          if isDecimalDigit(current) then
            consume()
          else if current == 'd' then
            consume()
            state = State.HEX_810
          else if current == 'f' then
            consume()
            state = State.HEX_820
          else
            val end = position
            val lexeme = input.slice(begin, end)
            token = Token(Token.Kind.HEXADECIMAL_FLOAT64_LITERAL, lexeme, position, line, column)
        case State.HEX_810 =>
          val end = position
          val lexeme = input.slice(begin, end)
          token = Token(Token.Kind.HEXADECIMAL_FLOAT64_LITERAL, lexeme, position, line, column)
        case State.HEX_820 =>
          val end = position
          val lexeme = input.slice(begin, end)
          token = Token(Token.Kind.HEXADECIMAL_FLOAT32_LITERAL, lexeme, position, line, column)
        case _ =>
          // Invalid state. Can only be reached through a lexer bug.
          print("error: Invalid state.")
    end while
    return token

  def number (): Token =
    // This scans for an integer or floating point number.
    val begin = position
    var state = State.NUM_START
    var token: Token = null
    while token == null do
      state match
        case State.NUM_START =>
          // We are guaranteed to get a digit here unless the lexer
          // has a bug in it.
          if isDecimalDigit(current) then
            consume()
            state = State.NUM_100
          else if current == '.' then
            consume()
            state = State.NUM_300
          else
            state = State.NUM_ERROR
        case State.NUM_100 =>
          if isDecimalDigit(current) then
            consume()
          else if current == '_' then
            consume()
            state = State.NUM_200
          else if current == 'L' then
            consume()
            state = State.NUM_210
          else if current == 'u' then
            consume()
            state = State.NUM_220
          else if current == '.' then
            consume()
            state = State.NUM_300
          else if current == 'e' then
            consume()
            state = State.NUM_600
          else if current == 'd' then
            consume()
            state = State.NUM_810
          else if current == 'f' then
            consume()
            state = State.NUM_820
          else
            // Accept
            val end = position
            val lexeme = input.slice(begin, end)
            token = Token(Token.Kind.INT32_LITERAL, lexeme, position, line, column)
        case State.NUM_200 =>
          if isDecimalDigit(current) then
            consume()
            state = State.NUM_100
          else if current == 'e' then
            consume()
            state = State.NUM_600
          else if current == 'd' then
            consume()
            state = State.NUM_810
          else if current == 'f' then
            consume()
            state = State.NUM_820
          else if current == 'L' then
            consume()
            state = State.NUM_210
          else if current == 'u' then
            consume()
            state = State.NUM_220
          else
            // Pretend we got a digit for error recovery purposes
            error(s"invalid number: found '${current}', expected 'd', 'f', 'e', 'L', 'u', or decimal digit")
            consume()
            state = State.NUM_100
        case State.NUM_210 =>
          if current == 'u' then
            consume()
            state = State.NUM_230
          else
            val end = position
            val lexeme = input.slice(begin, end)
            token = Token(Token.Kind.INT64_LITERAL, lexeme, position, line, column)
        case State.NUM_220 =>
          if current == 'L' then
            consume()
            state = State.NUM_230
          else
            // Accept
            val end = position
            val lexeme = input.slice(begin, end)
            token = Token(Token.Kind.UINT32_LITERAL, lexeme, position, line, column)
        case State.NUM_230 =>
          // Accept
          val end = position
          val lexeme = input.slice(begin, end)
          token = Token(Token.Kind.UINT64_LITERAL, lexeme, position, line, column)
        case State.NUM_300 =>
          if isDecimalDigit(current) then
            consume()
            state = State.NUM_400
          else
            // Pretend we got a digit for error recovery purposes
            error(s"invalid number: found '${current}', expected decimal digit")
            consume()
        case State.NUM_400 =>
          if isDecimalDigit(current) then
            consume()
          else if current == '_' then
            consume()
            state = State.NUM_500
          else if current == 'e' then
            consume()
            state = State.NUM_600
          else if current == 'd' then
            consume()
            state = State.NUM_810
          else if current == 'f' then
            consume()
            state = State.NUM_820
          else
            // Accept
            val end = position
            val lexeme = input.slice(begin, end)
            token = Token(Token.Kind.FLOAT64_LITERAL, lexeme, position, line, column)
        case State.NUM_500 =>
          if isDecimalDigit(current) then
            consume()
            state = State.NUM_400
          else if current == 'e' then
            consume()
            state = State.NUM_600
          else if current == 'd' then
            consume()
            state = State.NUM_810
          else if current == 'f' then
            consume()
            state = State.NUM_820
          else
            // Pretend we got a digit for error recovery purposes
            error(s"invalid number: found '${current}', expected decimal digit")
            consume()
            state = State.NUM_400
        case State.NUM_600 =>
          if isDecimalDigit(current) then
            consume()
            state = State.NUM_800
          else if current == '+' || current == '-' then
            consume()
            state = State.NUM_700
          else
            // Pretend we got a digit for error recovery purposes
            error(s"invalid number: found '${current}', expected '+', '-', or decimal digit")
            consume()
            state = State.NUM_800
        case State.NUM_700 =>
          if isDecimalDigit(current) then
            consume()
            state = State.NUM_800
          else
            // Pretend we got a digit for error recovery purposes
            error(s"invalid number: found '${current}', expected decimal digit")
            consume()
            state = State.NUM_800
        case State.NUM_800 =>
          if isDecimalDigit(current) then
            consume()
          else if current == 'd' then
            consume()
            state = State.NUM_810
          else if current == 'f' then
            consume()
            state = State.NUM_820
          else
            // Accept
            val end = position
            val lexeme = input.slice(begin, end)
            token = Token(Token.Kind.FLOAT64_LITERAL, lexeme, position, line, column)
        case State.NUM_810 =>
          // Accept
          val end = position
          val lexeme = input.slice(begin, end)
          token = Token(Token.Kind.FLOAT64_LITERAL, lexeme, position, line, column)
        case State.NUM_820 =>
          // Accept
          val end = position
          val lexeme = input.slice(begin, end)
          token = Token(Token.Kind.FLOAT32_LITERAL, lexeme, position, line, column)
        case _ =>
          // Invalid state. Can only be reached through a lexer bug.
          print("error: Invalid state.")
    end while
    return token
}
