#include "../../include/sema/SemanticAnalyzer.h"
#include <stdexcept>

using namespace std;

void SemanticAnalyzer::analyze(ASTNode *root)
{
    auto *func = dynamic_cast<FunctionDecl *>(root);
    if (!func)
    {
        throw runtime_error("Top-level node must be a function");
    }
    analyzeFunction(func);
}

// ------------------ Scope Helpers ------------------

void SemanticAnalyzer::pushScope()
{
    symbolStack.emplace_back();
}

void SemanticAnalyzer::popScope()
{
    symbolStack.pop_back();
}

bool SemanticAnalyzer::declare(const string &name, TokenType type)
{
    auto &scope = symbolStack.back();
    if (scope.count(name))
        return false;
    scope[name] = type;
    return true;
}

TokenType SemanticAnalyzer::lookup(const string &name)
{
    for (auto it = symbolStack.rbegin(); it != symbolStack.rend(); ++it)
    {
        auto found = it->find(name);
        if (found != it->end())
            return found->second;
    }
    throw runtime_error("Undeclared variable: " + name);
}

// ------------------ Analysis ------------------

void SemanticAnalyzer::analyzeFunction(FunctionDecl *func)
{
    symbolStack.clear();
    pushScope();

    for (const auto &[type, name] : func->params)
    {
        if (!declare(name.lexeme, type.type))
        {
            throw runtime_error("Duplicate parameter: " + name.lexeme);
        }
    }

    for (const auto &stmt : func->body)
    {
        analyzeStatement(stmt.get());
    }

    popScope();
}

void SemanticAnalyzer::analyzeStatement(Stmt *stmt)
{
    if (auto *var = dynamic_cast<VarDeclStmt *>(stmt))
    {
        if (!declare(var->name.lexeme, var->type.type))
        {
            throw runtime_error("Redeclaration of variable: " + var->name.lexeme);
        }
        if (var->initializer)
            analyzeExpr(var->initializer.get());
    }
    else if (auto *assign = dynamic_cast<AssignStmt *>(stmt))
    {
        lookup(assign->name.lexeme);
        analyzeExpr(assign->value.get());
    }
    else if (auto *ret = dynamic_cast<ReturnStmt *>(stmt))
    {
        if (ret->expr)
            analyzeExpr(ret->expr.get());
    }
    else if (auto *iff = dynamic_cast<IfStmt *>(stmt))
    {
        analyzeExpr(iff->condition.get());

        pushScope();
        for (const auto &s : iff->thenBranch)
            analyzeStatement(s.get());
        popScope();

        pushScope();
        for (const auto &s : iff->elseBranch)
            analyzeStatement(s.get());
        popScope();
    }
    else if (auto *loop = dynamic_cast<WhileStmt *>(stmt))
    {
        analyzeExpr(loop->condition.get());

        pushScope();
        for (const auto &s : loop->body)
            analyzeStatement(s.get());
        popScope();
    }
    else
    {
        throw runtime_error("Unknown statement type");
    }
}

void SemanticAnalyzer::analyzeExpr(Expr *expr)
{
    if (auto *var = dynamic_cast<VariableExpr *>(expr))
    {
        lookup(var->id.lexeme);
    }
    else if (auto *bin = dynamic_cast<BinaryOp *>(expr))
    {
        analyzeExpr(bin->lhs.get());
        analyzeExpr(bin->rhs.get());
    }
    else if (auto *un = dynamic_cast<UnaryOp *>(expr))
    {
        analyzeExpr(un->expr.get());
    }
    else if (dynamic_cast<Literal *>(expr))
    {
        // Literals are always valid
    }
    else
    {
        throw runtime_error("Unknown expression type");
    }
}
