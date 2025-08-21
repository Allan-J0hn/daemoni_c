// include/lexer/Token.h
#pragma once

#include "TokenType.h"
#include <ostream>
#include <string>

using namespace std;

// A single lexical token with source location for diagnostics.
struct Token {
    TokenType type{};
    string    lexeme{};
    int       line{0};
    int       column{0};

    Token() = default;
    Token(TokenType t, string lx, int ln, int col)
        : type(t), lexeme(move(lx)), line(ln), column(col) {}
};

inline const char* to_string(TokenType t) {
    switch (t) {
        // Punctuators
        case TokenType::LEFT_PAREN:      return "LEFT_PAREN";
        case TokenType::RIGHT_PAREN:     return "RIGHT_PAREN";
        case TokenType::LEFT_BRACE:      return "LEFT_BRACE";
        case TokenType::RIGHT_BRACE:     return "RIGHT_BRACE";
        case TokenType::COMMA:           return "COMMA";
        case TokenType::SEMICOLON:       return "SEMICOLON";
        case TokenType::PERIOD:          return "PERIOD";

        // Operators
        case TokenType::PLUS:            return "PLUS";
        case TokenType::MINUS:           return "MINUS";
        case TokenType::STAR:            return "STAR";
        case TokenType::SLASH:           return "SLASH";
        case TokenType::MOD:             return "MOD";
        case TokenType::ASSIGN:          return "ASSIGN";
        case TokenType::EQUAL:           return "EQUAL";
        case TokenType::NOT:             return "NOT";
        case TokenType::NOT_EQUAL:       return "NOT_EQUAL";
        case TokenType::LESS:            return "LESS";
        case TokenType::LESS_EQUAL:      return "LESS_EQUAL";
        case TokenType::GREATER:         return "GREATER";
        case TokenType::GREATER_EQUAL:   return "GREATER_EQUAL";
        case TokenType::LOGICAL_AND:     return "LOGICAL_AND";
        case TokenType::LOGICAL_OR:      return "LOGICAL_OR";

        // Literals & identifiers
        case TokenType::IDENTIFIER:      return "IDENTIFIER";
        case TokenType::NUMBER:          return "NUMBER";

        // Keywords / types
        case TokenType::INT:             return "INT";
        case TokenType::BOOL:            return "BOOL";
        case TokenType::VOID:            return "VOID";
        case TokenType::IF:              return "IF";
        case TokenType::ELSE:            return "ELSE";
        case TokenType::WHILE:           return "WHILE";
        case TokenType::TRUE:            return "TRUE";
        case TokenType::FALSE:           return "FALSE";
        case TokenType::RETURN:          return "RETURN";

        // Diagnostics / stream end
        case TokenType::ERROR:           return "ERROR";
        case TokenType::END_OF_FILE:     return "END_OF_FILE";
    }
    return "<UNKNOWN_TOKEN_TYPE>";
}

// Pretty-printers for logging.
inline ostream& operator<<(ostream& os, TokenType t) {
    return os << to_string(t);
}

inline ostream& operator<<(ostream& os, const Token& tok) {
    os << "Token(" << to_string(tok.type)
       << ", lexeme='" << tok.lexeme
       << "', line=" << tok.line
       << ", column=" << tok.column << ")";
    return os;
}
