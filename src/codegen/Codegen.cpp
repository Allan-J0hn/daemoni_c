#include "../../include/codegen/Codegen.h"
#include "llvm/IR/Verifier.h"
#include <stdexcept>

using namespace std;
using namespace llvm;

Codegen::Codegen()
{
    context = make_unique<LLVMContext>();
    module = make_unique<Module>("daemoni_c_module", *context);
    builder = make_unique<IRBuilder<>>(*context);
}

void Codegen::printIR()
{
    module->print(outs(), nullptr);
}

void Codegen::generate(ASTNode *root)
{
    auto *func = dynamic_cast<FunctionDecl *>(root);
    if (!func)
        throw runtime_error("Root must be a function");
    generateFunction(func);
}

void Codegen::pushScope()
{
    symbolStack.emplace_back();
}

void Codegen::popScope()
{
    symbolStack.pop_back();
}

void Codegen::insertVar(const string &name, Value *ptr)
{
    symbolStack.back()[name] = ptr;
}

Value *Codegen::lookupVar(const string &name)
{
    for (auto it = symbolStack.rbegin(); it != symbolStack.rend(); ++it)
    {
        auto found = it->find(name);
        if (found != it->end())
            return found->second;
    }
    throw runtime_error("Undefined variable: " + name);
}

void Codegen::generateFunction(FunctionDecl *func)
{
    vector<Type *> argTypes;
    for (const auto &[typeTok, _] : func->params)
    {
        if (typeTok.type == TokenType::INT)
            argTypes.push_back(Type::getInt32Ty(*context));
        else if (typeTok.type == TokenType::BOOL)
            argTypes.push_back(Type::getInt1Ty(*context));
        else
            throw runtime_error("Unsupported parameter type at line " + to_string(typeTok.line));
    }

    Type *retType;
    if (func->returnType.type == TokenType::INT)
    {
        retType = Type::getInt32Ty(*context);
    }
    else if (func->returnType.type == TokenType::BOOL)
    {
        retType = Type::getInt1Ty(*context);
    }
    else if (func->returnType.type == TokenType::VOID)
    {
        retType = Type::getVoidTy(*context);
    }
    else
    {
        throw runtime_error("Unsupported return type at line " + to_string(func->returnType.line));
    }

    FunctionType *fnType = FunctionType::get(retType, argTypes, false);
    Function *llvmFunc = Function::Create(fnType, Function::ExternalLinkage, func->name.lexeme, module.get());

    BasicBlock *entry = BasicBlock::Create(*context, "entry", llvmFunc);
    builder->SetInsertPoint(entry);

    pushScope();

    unsigned idx = 0;
    for (auto &arg : llvmFunc->args())
    {
        arg.setName(func->params[idx].second.lexeme);
        AllocaInst *alloca = builder->CreateAlloca(arg.getType(), nullptr, arg.getName());
        builder->CreateStore(&arg, alloca);
        insertVar(arg.getName().str(), alloca);
        idx++;
    }

    for (const auto &stmt : func->body)
    {
        generateStatement(stmt.get());
    }

    popScope();

    // Ensure a terminator exists for non-void functions
    if (!builder->GetInsertBlock()->getTerminator())
    {
        if (retType->isVoidTy())
        {
            builder->CreateRetVoid();
        }
        else
        {
            builder->CreateRet(ConstantInt::get(retType, 0));
        }
    }

    // Verify the function
    verifyFunction(*llvmFunc, &errs());
}

void Codegen::generateStatement(Stmt *stmt)
{
    if (auto *ret = dynamic_cast<ReturnStmt *>(stmt))
    {
        if (ret->expr)
        {
            Value *val = generateExpr(ret->expr.get());
            builder->CreateRet(val);
        }
        else
        {
            builder->CreateRetVoid();
        }
    }
    else if (auto *decl = dynamic_cast<VarDeclStmt *>(stmt))
    {
        Type *llvmType;
        if (decl->type.type == TokenType::INT)
        {
            llvmType = Type::getInt32Ty(*context);
        }
        else if (decl->type.type == TokenType::BOOL)
        {
            llvmType = Type::getInt1Ty(*context);
        }
        else
        {
            throw runtime_error("Unsupported variable type at line " + to_string(decl->type.line));
        }

        Value *init = decl->initializer
                          ? generateExpr(decl->initializer.get())
                          : ConstantInt::get(llvmType, 0);

        AllocaInst *alloca = builder->CreateAlloca(llvmType, nullptr, decl->name.lexeme);
        builder->CreateStore(init, alloca);
        insertVar(decl->name.lexeme, alloca);
    }
    else if (auto *assign = dynamic_cast<AssignStmt *>(stmt))
    {
        Value *rhs = generateExpr(assign->value.get());
        Value *ptr = lookupVar(assign->name.lexeme);
        builder->CreateStore(rhs, ptr);
    }
    else if (auto *iff = dynamic_cast<IfStmt *>(stmt))
    {
        Value *cond = generateExpr(iff->condition.get());

        Function *parent = builder->GetInsertBlock()->getParent();
        BasicBlock *thenBB = BasicBlock::Create(*context, "then", parent);
        BasicBlock *elseBB = BasicBlock::Create(*context, "else");
        BasicBlock *mergeBB = BasicBlock::Create(*context, "merge");

        builder->CreateCondBr(cond, thenBB, elseBB);

        builder->SetInsertPoint(thenBB);
        pushScope();
        for (const auto &s : iff->thenBranch)
        {
            generateStatement(s.get());
        }
        if (!builder->GetInsertBlock()->getTerminator())
        {
            builder->CreateBr(mergeBB);
        }
        popScope();

        elseBB->insertInto(parent);
        builder->SetInsertPoint(elseBB);
        pushScope();
        for (const auto &s : iff->elseBranch)
        {
            generateStatement(s.get());
        }
        if (!builder->GetInsertBlock()->getTerminator())
        {
            builder->CreateBr(mergeBB);
        }
        popScope();

        mergeBB->insertInto(parent);
        builder->SetInsertPoint(mergeBB);
    }
    else if (auto *loop = dynamic_cast<WhileStmt *>(stmt))
    {
        Function *parent = builder->GetInsertBlock()->getParent();

        BasicBlock *condBB = BasicBlock::Create(*context, "cond", parent);
        BasicBlock *bodyBB = BasicBlock::Create(*context, "loop");
        BasicBlock *afterBB = BasicBlock::Create(*context, "after");

        builder->CreateBr(condBB);
        builder->SetInsertPoint(condBB);
        Value *cond = generateExpr(loop->condition.get());
        builder->CreateCondBr(cond, bodyBB, afterBB);

        bodyBB->insertInto(parent);
        builder->SetInsertPoint(bodyBB);
        pushScope();
        for (const auto &s : loop->body)
        {
            generateStatement(s.get());
        }
        if (!builder->GetInsertBlock()->getTerminator())
        {
            builder->CreateBr(condBB);
        }
        popScope();

        afterBB->insertInto(parent);
        builder->SetInsertPoint(afterBB);
    }
    else
    {
        throw runtime_error("Unknown statement type at line " + to_string(stmt->token.line));
    }
}

Value *Codegen::generateExpr(Expr *expr)
{
    if (auto *lit = dynamic_cast<Literal *>(expr))
    {
        if (lit->kind == Literal::Kind::Int)
        {
            return ConstantInt::get(Type::getInt32Ty(*context), lit->intValue);
        }
        else
        {
            return ConstantInt::get(Type::getInt1Ty(*context), lit->boolValue);
        }
    }
    else if (auto *var = dynamic_cast<VariableExpr *>(expr))
    {
        Value *ptr = lookupVar(var->id.lexeme);
        // Ensure ptr is an AllocaInst (from symbol table)
        auto *alloca = dyn_cast<AllocaInst>(ptr);
        if (!alloca)
        {
            throw runtime_error("Variable " + var->id.lexeme + " is not an AllocaInst at line " + to_string(var->id.line));
        }
        // Get pointee type from AllocaInst
        Type *pointeeType = alloca->getAllocatedType();
        return builder->CreateLoad(pointeeType, ptr, var->id.lexeme);
    }
    else if (auto *bin = dynamic_cast<BinaryOp *>(expr))
    {
        Value *lhs = generateExpr(bin->lhs.get());
        Value *rhs = generateExpr(bin->rhs.get());

        switch (bin->op)
        {
        case BinaryOpKind::Add:
            return builder->CreateAdd(lhs, rhs);
        case BinaryOpKind::Sub:
            return builder->CreateSub(lhs, rhs);
        case BinaryOpKind::Mul:
            return builder->CreateMul(lhs, rhs);
        case BinaryOpKind::Div:
            return builder->CreateSDiv(lhs, rhs);
        case BinaryOpKind::Mod:
            return builder->CreateSRem(lhs, rhs);
        case BinaryOpKind::Eq:
            return builder->CreateICmpEQ(lhs, rhs);
        case BinaryOpKind::Ne:
            return builder->CreateICmpNE(lhs, rhs);
        case BinaryOpKind::Lt:
            return builder->CreateICmpSLT(lhs, rhs);
        case BinaryOpKind::Le:
            return builder->CreateICmpSLE(lhs, rhs);
        case BinaryOpKind::Gt:
            return builder->CreateICmpSGT(lhs, rhs);
        case BinaryOpKind::Ge:
            return builder->CreateICmpSGE(lhs, rhs);
        case BinaryOpKind::And:
            return builder->CreateAnd(lhs, rhs);
        case BinaryOpKind::Or:
            return builder->CreateOr(lhs, rhs);
        default:
            throw runtime_error("Unknown binary operator at line " + to_string(expr->token.line));
        }
    }
    else if (auto *un = dynamic_cast<UnaryOp *>(expr))
    {
        Value *val = generateExpr(un->expr.get());
        switch (un->op)
        {
        case UnaryOpKind::Not:
            return builder->CreateNot(val);
        case UnaryOpKind::Negate:
            return builder->CreateNeg(val);
        default:
            throw runtime_error("Unknown unary operator at line " + to_string(expr->token.line));
        }
    }

    throw runtime_error("Unknown expression type at line " + to_string(expr->token.line));
}