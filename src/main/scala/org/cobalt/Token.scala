package org.cobalt

class Token (val kind: Token.Kind, val lexeme: String, val position: Int, val line: Int, val column: Int) {}

object Token {
    enum Kind {
        case AND
        case BREAK
        case CASE
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
        case EQUAL
        case EQUAL_EQUAL
        case EXCLAMATION
        case EXCLAMATION_EQUAL
        case GREATER
        case GREATER_EQUAL
        case GREATER_GREATER
        case GREATER_GREATER_EQUAL
        case IDENTIFIER
        case LESS
        case LESS_EQUAL
        case LESS_LESS
        case LESS_LESS_EQUAL
        case MINUS
        case MINUS_EQUAL
        case MINUS_GREATER
        case PERCENT
        case PERCENT_EQUAL
        case PLUS
        case PLUS_EQUAL
        case SLASH
        case SLASH_EQUAL
        case STRING_LITERAL
        case TILDE
        case TILDE_EQUAL
        case VAR
    }

}
