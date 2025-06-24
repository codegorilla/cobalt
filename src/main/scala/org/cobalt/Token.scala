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
        case PACKAGE
        case RETURN
        case STRUCT
        case THEN
        case TRUE
        case TYPEALIAS
        case UNION
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

        // Numbers
        case BINARY_INT32_LITERAL
        case INT32_LITERAL
        case INT64_LITERAL

        // Operators and Punctuation
        case AMPERSAND
        case AMPERSAND_AMPERSAND
        case AMPERSAND_EQUAL
        case ASTERISK
        case ASTERISK_EQUAL
        case BAR
        case BAR_BAR
        case BAR_EQUAL
        case CARAT
        case CARAT_EQUAL
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
