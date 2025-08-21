// include/codegen/Codegen.h
#pragma once
#include "../ast/ASTNode.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Instructions.h"
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

using namespace std;
using namespace llvm;

class Codegen
{
public:
    Codegen();
    void generate(ASTNode *root);
    void printIR();

private:
    unique_ptr<LLVMContext> context;
    unique_ptr<Module> module;
    unique_ptr<IRBuilder<>> builder;

    // Scoped symbol table of stack allocas (variables/params)
    vector<unordered_map<string, Value *>> symbolStack;

    // ---- scope mgmt ----
    void pushScope();
    void popScope();
    void insertVar(const string &name, Value *ptr);
    Value *lookupVar(const string &name);

    // ---- top-level lowering ----
    void generateFunction(FunctionDecl *func);
    void generateStatement(Stmt *stmt);
    Value *generateExpr(Expr *expr);

    // ---- type normalization helpers ----
    Value *asBool(Value *v);

    Value *asI32(Value *v);

    // ---- short-circuit logical lowering ----
    Value *emitLogicalAnd(Expr *lhs, Expr *rhs);

    Value *emitLogicalOr(Expr *lhs, Expr *rhs);
};
