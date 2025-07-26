#ifndef LEXER_H
#define LEXER_H

#include "Token.h"
#include <string>
#include <vector>

using namespace std;

class Lexer {
public:
    Lexer(const string& source);
    vector<Token> tokenize();

private:
    const string& src;
    size_t start = 0;
    size_t current = 0;
    int line = 1;
    int column = 1;

    bool isAtEnd() const;
    char advance();
    char peek() const;
    void addToken(TokenType type, vector<Token>& tokens);
};

#endif
