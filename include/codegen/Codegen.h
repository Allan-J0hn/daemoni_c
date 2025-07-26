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

    vector<unordered_map<string, Value *>> symbolStack;

    void pushScope();
    void popScope();
    void insertVar(const string &name, Value *ptr);
    Value *lookupVar(const string &name);

    void generateFunction(FunctionDecl *func);
    void generateStatement(Stmt *stmt);
    Value *generateExpr(Expr *expr);
};