package org.cobalt

case class Token (val kind: Token.Kind, val lexeme: String, val position: Int, val line: Int, val column: Int)

object Token {
    enum Kind {
        // Keywords
        case ABSTRACT
        case AND
        case BREAK
        case CASE
        case CAST
        case CATCH
        case CLASS
        case CONST
        case CONTINUE
        case DEF
        case DEFAULT
        case DELETE
        case DIVINE
        case DO
        case ELSE
        case ENUM
        case EXTENDS
        case FALSE
        case FINAL
        case FOR
        case FOREACH
        case FN
        case FUN
        case GOTO
        case IF
        case IN
        case NAMESPACE
        case NEW
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
        case SWITCH
        case TEMPLATE
        case THIS
        case TRANSMUTE
        case TRUE
        case TRY
        case TYPEALIAS
        case UNION
        case UNTIL
        case USING
        case VAL
        case VAR
        case VIRTUAL
        case VOLATILE
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

        // Other literals
        case CHARACTER_LITERAL
        case STRING_LITERAL

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
        case TILDE
        case TILDE_EQUAL

        // End of file
        case EOF
    }

}
