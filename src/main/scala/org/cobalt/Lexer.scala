package org.cobalt

import scala.compiletime.ops.double

class Lexer {

  // Define end of file (EOF)
  val EOF = (-1).toChar

  var input = ""
  var position = 0
  var current = EOF
  var line = 1
  var column = 1

  val keywordLookup = Map (
    "and" -> Token.Kind.AND,
    "break" -> Token.Kind.BREAK,
    "case" -> Token.Kind.CASE,
    "catch" -> Token.Kind.CATCH,
    "class" -> Token.Kind.CLASS,
    "const" -> Token.Kind.CONST,
    "continue" -> Token.Kind.CONTINUE,
    "def" -> Token.Kind.DEF,
    "default" -> Token.Kind.DEFAULT,
    "delete" -> Token.Kind.DELETE,
    "do" -> Token.Kind.DO,
    "else" -> Token.Kind.ELSE,
    "end" -> Token.Kind.END,
    "enum" -> Token.Kind.ENUM,
    "extends" -> Token.Kind.EXTENDS,
    "false" -> Token.Kind.FALSE,
    "for" -> Token.Kind.FOR,
    "foreach" -> Token.Kind.FOREACH,
    "fn" -> Token.Kind.FN,
    "fun" -> Token.Kind.FUN,
    "if" -> Token.Kind.IF,
    "in" -> Token.Kind.IN,
    "loop" -> Token.Kind.LOOP,
    "nil" -> Token.Kind.NIL,
    "null" -> Token.Kind.NULL,
    "or" -> Token.Kind.OR,
    "package" -> Token.Kind.PACKAGE,
    "return" -> Token.Kind.RETURN,
    "struct" -> Token.Kind.STRUCT,
    "then" -> Token.Kind.THEN,
    "true" -> Token.Kind.TRUE,
    "typealias" -> Token.Kind.TYPEALIAS,
    "union" -> Token.Kind.UNION,
    "val" -> Token.Kind.VAL,
    "var" -> Token.Kind.VAR,
    "while" -> Token.Kind.WHILE,

    "int" -> Token.Kind.INT
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
        if current == '=' then
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

      // Do strings and characters here

      else if current == ':' then
        consume()
        return Token(Token.Kind.COLON, ":", position, line, column)

      else if current == ';' then
        consume()
        return Token(Token.Kind.SEMICOLON, ";", position, line, column)



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
        // Not sure if this can be a val - does it get re-created each time?
        // Might need to change to var
        val begin = position
        consume()
        while (position < input.length) && (current.isLetter || current.isDigit || current == '_') do
          consume()
        // End index of slice is excluded from result
        val end = position
        lexeme = input.slice(begin, end)
        kind = if keywordLookup.contains(lexeme) then keywordLookup(lexeme) else Token.Kind.IDENTIFIER
        return Token(kind, lexeme, position, line, column)

      else if current.isDigit then
        return number()
      
      else
        print("ERROR!")

    end while

    // Placeholder to avoid error
    return Token(Token.Kind.EOF, "<EOF>", position, line, column)

  // Todo: Fix
  def number (): Token =
    return Token(Token.Kind.EQUAL, "=", position, line, column)
}

