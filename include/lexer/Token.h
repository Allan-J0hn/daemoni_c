#pragma once
#include <string>

using namespace std;

enum class TokenType
{
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    PLUS,
    MINUS,
    STAR,
    SLASH,
    PERIOD,
    EQUAL,
    ASSIGN,
    COMMA,
    SEMICOLON,
    GREATER,
    LESS,
    LOGICAL_AND,
    LOGICAL_OR,
    NOT,
    NOT_EQUAL,
    LESS_EQUAL,
    GREATER_EQUAL,
    MOD,
    IDENTIFIER,
    NUMBER,
    INT,
    BOOL,
    VOID,
    IF,
    ELSE,
    WHILE,
    TRUE,
    FALSE,
    RETURN,
    ERROR,
    END_OF_FILE
};

struct Token
{
    TokenType type;
    string lexeme;
    int line;
    int column;
};