#pragma once

// Single source of truth for token kinds used by the lexer & parser.
enum class TokenType
{
    // Punctuators
    LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
    COMMA, SEMICOLON, PERIOD,

    // Operators
    PLUS, MINUS, STAR, SLASH, MOD,
    ASSIGN,      // =
    EQUAL,       // ==
    NOT,         // !
    NOT_EQUAL,   // !=
    LESS, LESS_EQUAL,
    GREATER, GREATER_EQUAL,
    LOGICAL_AND, // &&
    LOGICAL_OR,  // ||

    // Literals & identifiers
    IDENTIFIER, NUMBER,

    // Keywords / types
    INT, BOOL, VOID,
    IF, ELSE, WHILE, TRUE, FALSE, RETURN,

    // Diagnostics / stream end
    ERROR, END_OF_FILE
};
