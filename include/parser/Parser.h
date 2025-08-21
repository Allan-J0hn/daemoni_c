// include/parser/Parser.h
#pragma once
#include "../lexer/Token.h"
#include "../ast/ASTNode.h"
#include <vector>
#include <string>

using namespace std;

class Parser {
public:
    Parser(const vector<Token>& tokens);
    ASTNode* parse();  // Entry point: returns a FunctionDecl*

private:
    const vector<Token>& tokens;
    size_t current = 0;

    // Token stream control
    Token advance();
    Token peek() const;
    bool isAtEnd() const;
    void expect(TokenType type, const string& message);

    // Top-level constructs
    ASTNode* parseFunction();   // Parses FunctionDecl with parameter list
    ASTNode* parseStatement();  // Parses a Stmt*

    // Expression parsing (precedence-based)
    Expr* parseExpression();        // ||
    Expr* parseLogicalOr();         // ||
    Expr* parseLogicalAnd();        // &&
    Expr* parseEquality();          // ==, !=
    Expr* parseRelational();        // <, <=, >, >=
    Expr* parseAdditive();          // +, -
    Expr* parseMultiplicative();    // *, /, %
    Expr* parseUnary();             // !, -
    Expr* parsePrimary();           // literals, identifiers, grouped ( )
};
