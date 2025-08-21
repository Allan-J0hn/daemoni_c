// include/ast/ASTNode.h
#pragma once
#include "../lexer/Token.h"
#include <vector>
#include <memory>
#include <string>

using namespace std;

class ASTNode
{
public:
    virtual ~ASTNode() = default;
    Token token; // for diagnostics
};

// -------------------- Expressions --------------------

class Expr : public ASTNode
{
};

class VariableExpr : public Expr
{
public:
    Token id;
    VariableExpr(Token id) : id(id) {}
};

enum class BinaryOpKind
{
    Add, Sub, Mul, Div, Mod,
    Eq, Ne, Lt, Le, Gt, Ge,
    And, Or
};

enum class UnaryOpKind
{
    Negate,
    Not
};

class BinaryOp : public Expr
{
public:
    BinaryOpKind op;
    unique_ptr<Expr> lhs, rhs;

    BinaryOp(BinaryOpKind op, unique_ptr<Expr> lhs, unique_ptr<Expr> rhs)
        : op(op), lhs(move(lhs)), rhs(move(rhs)) {}
};

class UnaryOp : public Expr
{
public:
    UnaryOpKind op;
    unique_ptr<Expr> expr;

    UnaryOp(UnaryOpKind op, unique_ptr<Expr> expr)
        : op(op), expr(move(expr)) {}
};

class Literal : public Expr
{
public:
    enum class Kind { Int, Bool } kind;
    int intValue;
    bool boolValue;

    Literal(int val) : kind(Kind::Int), intValue(val), boolValue(false) {}
    Literal(bool val) : kind(Kind::Bool), intValue(0), boolValue(val) {}
};

// -------------------- Statements --------------------

class Stmt : public ASTNode
{
};

class VarDeclStmt : public Stmt
{
public:
    Token type;
    Token name;
    unique_ptr<Expr> initializer;

    VarDeclStmt(Token type, Token name, unique_ptr<Expr> initializer)
        : type(type), name(name), initializer(move(initializer)) {}
};

class AssignStmt : public Stmt
{
public:
    Token name;
    unique_ptr<Expr> value;

    AssignStmt(Token name, unique_ptr<Expr> value)
        : name(name), value(move(value)) {}
};

class ReturnStmt : public Stmt
{
public:
    unique_ptr<Expr> expr;

    ReturnStmt(unique_ptr<Expr> expr) : expr(move(expr)) {}
};

class IfStmt : public Stmt
{
public:
    unique_ptr<Expr> condition;
    vector<unique_ptr<Stmt>> thenBranch;
    vector<unique_ptr<Stmt>> elseBranch;

    IfStmt(unique_ptr<Expr> condition,
           vector<unique_ptr<Stmt>> thenBranch,
           vector<unique_ptr<Stmt>> elseBranch)
        : condition(move(condition)),
          thenBranch(move(thenBranch)),
          elseBranch(move(elseBranch)) {}
};

class WhileStmt : public Stmt
{
public:
    unique_ptr<Expr> condition;
    vector<unique_ptr<Stmt>> body;

    WhileStmt(unique_ptr<Expr> condition, vector<unique_ptr<Stmt>> body)
        : condition(move(condition)), body(move(body)) {}
};

class FunctionDecl : public ASTNode
{
public:
    Token returnType;
    Token name;
    vector<pair<Token, Token>> params;
    vector<unique_ptr<Stmt>> body;

    FunctionDecl(Token returnType, Token name,
                 vector<pair<Token, Token>> params,
                 vector<unique_ptr<Stmt>> body)
        : returnType(returnType), name(name),
          params(move(params)), body(move(body)) {}
};
