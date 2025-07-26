#pragma once
#include "../ast/ASTNode.h"
#include <vector>
#include <string>
#include <unordered_map>

using namespace std;

class SemanticAnalyzer
{
public:
    void analyze(ASTNode *root);

private:
    // Stack of scopes, each scope maps variable name TokenType
    vector<unordered_map<string, TokenType>> symbolStack;

    // Scope management
    void pushScope();
    void popScope();
    bool declare(const string &name, TokenType type);
    TokenType lookup(const string &name);

    // Core analysis routines
    void analyzeFunction(FunctionDecl *func);
    void analyzeStatement(Stmt *stmt);
    void analyzeExpr(Expr *expr);
};
