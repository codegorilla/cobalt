package org.cobalt

class Lexer {

  // Only supports ASCII for now
  val EOF: Char = 1000

  var input = ""
  var position = 0
  var current = EOF
  var start = 0
  var end = 0
  var line = 1
  var column = 1

  val keywordLookup = Map (
    "and" -> Token.Kind.AND,
    "break" -> Token.Kind.BREAK
  )

  def setInput (input: String) =
    this.input = input
    if (input.length > 0)
      current = input(position)

  def error (message: String) =
    val coords = s"(${line},${column})"
    println(s"${coords}: error: ${message}")

  def consume () = {
    position += 1
    current = if position < input.length then input(position) else EOF
    column += 1
  }

  def test () =
    val t = Token(Token.Kind.EQUAL_EQUAL, "for", 0, 1, 1)
    print(t.kind)

  def getToken (): Token =

    var kind: Token.Kind = null
    var lexeme = ""

    while current != EOF do

      if current == '=' then
        consume()
        if current == '=' then
          consume()
          kind = Token.Kind.EQUAL_EQUAL
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
          kind = Token.Kind.CARAT_EQUAL
          lexeme = "^="
        else
          kind = Token.Kind.CARAT
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

      if current == '+' then
        consume()
        if current == '=' then
          consume()
          kind = Token.Kind.PLUS_EQUAL
          lexeme = "+="
        else
          kind = Token.Kind.PLUS
          lexeme = "+"
        return Token(kind, lexeme, position, line, column)

      if current == '-' then
        consume()
        if current == '=' then
          consume()
          kind = Token.Kind.MINUS_EQUAL
          lexeme = "-="
        else
          kind = Token.Kind.MINUS
          lexeme = "-"
        return Token(kind, lexeme, position, line, column)

      if current == '*' then
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
      if current == '/' then
        consume()
        if current == '=' then
          consume()
          kind = Token.Kind.SLASH_EQUAL
          lexeme = "/="
        else
          kind = Token.Kind.SLASH
          lexeme = "/"
        return Token(kind, lexeme, position, line, column)

      if current == '%' then
        consume()
        if current == '=' then
          consume()
          kind = Token.Kind.PERCENT_EQUAL
          lexeme = "%="
        else
          kind = Token.Kind.PERCENT
          lexeme = "%"
        return Token(kind, lexeme, position, line, column)

      if current == '!' then
        consume()
        if current == '=' then
          consume()
          kind = Token.Kind.EXCLAMATION_EQUAL
          lexeme = "!="
        else
          kind = Token.Kind.EXCLAMATION
          lexeme = "!"
        return Token(kind, lexeme, position, line, column)
      
      if current == '~' then
        consume()
        if current == '=' then
          consume()
          kind = Token.Kind.TILDE_EQUAL
          lexeme = "~="
        else
          kind = Token.Kind.TILDE
          lexeme = "~"
        return Token(kind, lexeme, position, line, column)





      else if current.isLetter || current == '_' then
        // Not sure if this can be a val - does it get re-created each time?
        // Might need to change to var
        val begin = position
        consume()
        // Todo: The end should occur before the consume?
        while current.isLetter || current.isDigit || current == '_' do
          consume()
        val end = position
        val id = input.slice(begin, end)
        if keywordLookup.contains(id) then
          return Token(keywordLookup(id), id, position, line, column)
        else
          return Token(Token.Kind.IDENTIFIER, id, position, line, column)

      else if current.isDigit then
        return number()
      
      else
        print("ERROR!")

    end while

    // Placeholder to avoid error
    return Token(Token.Kind.EQUAL, "=", position, line, column)

  // Todo: Fix
  def number (): Token =
    return Token(Token.Kind.EQUAL, "=", position, line, column)
}

