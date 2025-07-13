package org.cobalt

case class Token (val kind: Token.Kind, val lexeme: String, val position: Int, val line: Int, val column: Int)

object Token {
    enum Kind {
        // Keywords
        case AND
        case BREAK
        case CASE
        case CATCH
        case CLASS
        case CONST
        case CONTINUE
        case DEF
        case DEFAULT
        case DELETE
        case DO
        case ELSE
        case END
        case ENUM
        case EXTENDS
        case FALSE
        case FINAL
        case FOR
        case FOREACH
        case FN
        case FUN
        case IF
        case IN
        case LOOP
        case NIL
        case NULL
        case OR
        case OVERRIDE
        case PACKAGE
        case PRIVATE
        case PUBLIC
        case RETURN
        case STATIC
        case STRUCT
        case THEN
        case THIS
        case TRUE
        case TYPEALIAS
        case UNION
        case USING
        case VAL
        case VAR
        case WHILE

        // Basic types
        case BOOL
        case DOUBLE
        case FLOAT
        case FLOAT32
        case FLOAT64
        case INT
        case INT8
        case INT16
        case INT32
        case INT64
        case LONG
        case NULL_T
        case SHORT
        case UINT
        case UINT8
        case UINT16
        case UINT32
        case UINT64
        case VOID

        // Identifiers
        case IDENTIFIER

        // Integer literals
        case BINARY_INT32_LITERAL
        case BINARY_INT64_LITERAL
        case BINARY_UINT32_LITERAL
        case BINARY_UINT64_LITERAL
        case HEXADECIMAL_INT32_LITERAL
        case HEXADECIMAL_INT64_LITERAL
        case HEXADECIMAL_UINT32_LITERAL
        case HEXADECIMAL_UINT64_LITERAL
        case INT32_LITERAL
        case INT64_LITERAL
        case OCTAL_INT32_LITERAL
        case OCTAL_INT64_LITERAL
        case OCTAL_UINT32_LITERAL
        case OCTAL_UINT64_LITERAL
        case UINT32_LITERAL
        case UINT64_LITERAL

        // Floating-point literals
        case FLOAT32_LITERAL
        case FLOAT64_LITERAL
        case HEXADECIMAL_FLOAT32_LITERAL
        case HEXADECIMAL_FLOAT64_LITERAL

        // Operators and Punctuation
        case AMPERSAND
        case AMPERSAND_AMPERSAND
        case AMPERSAND_EQUAL
        case ASTERISK
        case ASTERISK_EQUAL
        case BAR
        case BAR_BAR
        case BAR_EQUAL
        case CARET
        case CARET_EQUAL
        case CHARACTER_LITERAL
        case COLON
        case COMMA
        case EQUAL
        case EQUAL_X2
        case EQUAL_EQUAL
        case EXCLAMATION
        case EXCLAMATION_EQUAL
        case GREATER
        case GREATER_EQUAL
        case GREATER_GREATER
        case GREATER_GREATER_EQUAL
        case L_BRACE
        case L_BRACKET
        case L_PARENTHESIS
        case LESS
        case LESS_EQUAL
        case LESS_LESS
        case LESS_LESS_EQUAL
        case MINUS
        case MINUS_EQUAL
        case MINUS_GREATER
        case PERCENT
        case PERCENT_EQUAL
        case PERIOD
        case PERIOD_PERIOD
        case PLUS
        case PLUS_EQUAL
        case R_BRACE
        case R_BRACKET
        case R_PARENTHESIS
        case SEMICOLON
        case SLASH
        case SLASH_EQUAL
        case STRING_LITERAL
        case TILDE
        case TILDE_EQUAL

        // End of file
        case EOF
    }

}
