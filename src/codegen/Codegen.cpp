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

// === helpers ===
Value *Codegen::asBool(Value *v)
{
    Type *ty = v->getType();
    if (ty->isIntegerTy(1))
        return v;

    if (ty->isIntegerTy(32))
        return builder->CreateICmpNE(v, ConstantInt::get(ty, 0), "tobool");

    throw runtime_error("asBool: unsupported value type");
}

Value *Codegen::asI32(Value *v)
{
    Type *ty = v->getType();
    if (ty->isIntegerTy(32))
        return v; // already i32

    if (ty->isIntegerTy(1))
        return builder->CreateZExt(v, Type::getInt32Ty(*context), "booltoint");

    throw runtime_error("asI32: unsupported value type");
}

Value *Codegen::emitLogicalAnd(Expr *lhsE, Expr *rhsE)
{
    // Short-circuit: if lhs is false, don't eval rhs
    Function *F = builder->GetInsertBlock()->getParent();

    BasicBlock *lhsBB = builder->GetInsertBlock();
    BasicBlock *rhsBB = BasicBlock::Create(*context, "and.rhs", F);
    BasicBlock *mergeBB = BasicBlock::Create(*context, "and.merge");

    Value *lhs = asBool(generateExpr(lhsE));
    builder->CreateCondBr(lhs, rhsBB, mergeBB);

    // RHS block
    builder->SetInsertPoint(rhsBB);
    Value *rhs = asBool(generateExpr(rhsE));
    builder->CreateBr(mergeBB);

    // Merge
    mergeBB->insertInto(F);
    builder->SetInsertPoint(mergeBB);
    PHINode *phi = builder->CreatePHI(Type::getInt1Ty(*context), 2, "and.tmp");
    phi->addIncoming(ConstantInt::getFalse(*context), lhsBB);
    phi->addIncoming(rhs, rhsBB);
    return phi;
}

Value *Codegen::emitLogicalOr(Expr *lhsE, Expr *rhsE)
{
    // Short-circuit: if lhs is true, don't eval rhs
    Function *F = builder->GetInsertBlock()->getParent();

    BasicBlock *lhsBB = builder->GetInsertBlock();
    BasicBlock *rhsBB = BasicBlock::Create(*context, "or.rhs", F);
    BasicBlock *mergeBB = BasicBlock::Create(*context, "or.merge");

    Value *lhs = asBool(generateExpr(lhsE));
    builder->CreateCondBr(lhs, mergeBB, rhsBB);

    // RHS block
    builder->SetInsertPoint(rhsBB);
    Value *rhs = asBool(generateExpr(rhsE));
    builder->CreateBr(mergeBB);

    // Merge
    mergeBB->insertInto(F);
    builder->SetInsertPoint(mergeBB);
    PHINode *phi = builder->CreatePHI(Type::getInt1Ty(*context), 2, "or.tmp");
    phi->addIncoming(lhs, lhsBB);
    phi->addIncoming(rhs, rhsBB);
    return phi;
}

// === end helpers ===

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
        retType = Type::getInt32Ty(*context);
    else if (func->returnType.type == TokenType::BOOL)
        retType = Type::getInt1Ty(*context);
    else if (func->returnType.type == TokenType::VOID)
        retType = Type::getVoidTy(*context);
    else
        throw runtime_error("Unsupported return type at line " + to_string(func->returnType.line));

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
            builder->CreateRetVoid();
        else
            builder->CreateRet(ConstantInt::get(retType, 0));
    }

    verifyFunction(*llvmFunc, &errs());
}

void Codegen::generateStatement(Stmt *stmt)
{
    if (auto *ret = dynamic_cast<ReturnStmt *>(stmt))
    {
        Function *F = builder->GetInsertBlock()->getParent();
        Type *retTy = F->getReturnType();

        if (ret->expr)
        {
            Value *val = generateExpr(ret->expr.get());
            // Coerce return value to function return type
            if (retTy->isVoidTy())
                throw runtime_error("Void function cannot return a value");

            if (retTy->isIntegerTy(1))
                val = asBool(val);
            else if (retTy->isIntegerTy(32))
                val = asI32(val);
            else
                throw runtime_error("Unsupported return type");

            builder->CreateRet(val);
        }
        else
        {
            if (!retTy->isVoidTy())
                builder->CreateRet(ConstantInt::get(retTy, 0));
            else
                builder->CreateRetVoid();
        }
    }
    else if (auto *decl = dynamic_cast<VarDeclStmt *>(stmt))
    {
        Type *llvmType;
        if (decl->type.type == TokenType::INT)
            llvmType = Type::getInt32Ty(*context);
        else if (decl->type.type == TokenType::BOOL)
            llvmType = Type::getInt1Ty(*context);
        else
            throw runtime_error("Unsupported variable type at line " + to_string(decl->type.line));

        Value *init = nullptr;
        if (decl->initializer)
        {
            init = generateExpr(decl->initializer.get());
            // Implicit cast initializer to declared type
            if (llvmType->isIntegerTy(1))
                init = asBool(init);
            else
                init = asI32(init);
        }
        else
        {
            init = ConstantInt::get(llvmType, 0);
        }

        AllocaInst *alloca = builder->CreateAlloca(llvmType, nullptr, decl->name.lexeme);
        builder->CreateStore(init, alloca);
        insertVar(decl->name.lexeme, alloca);
    }
    else if (auto *assign = dynamic_cast<AssignStmt *>(stmt))
    {
        Value *rhs = generateExpr(assign->value.get());
        Value *ptr = lookupVar(assign->name.lexeme);
        auto *alloca = dyn_cast<AllocaInst>(ptr);
        if (!alloca)
            throw runtime_error("Assignment target is not a variable");

        Type *dstTy = alloca->getAllocatedType();
        // Implicit cast RHS to LHS var type
        if (dstTy->isIntegerTy(1))
            rhs = asBool(rhs);
        else if (dstTy->isIntegerTy(32))
            rhs = asI32(rhs);
        else
            throw runtime_error("Unsupported assignment type");

        builder->CreateStore(rhs, ptr);
    }
    else if (auto *iff = dynamic_cast<IfStmt *>(stmt))
    {
        Value *condV = asBool(generateExpr(iff->condition.get()));

        Function *parent = builder->GetInsertBlock()->getParent();
        BasicBlock *thenBB = BasicBlock::Create(*context, "then", parent);
        BasicBlock *elseBB = BasicBlock::Create(*context, "else");
        BasicBlock *mergeBB = BasicBlock::Create(*context, "merge");

        builder->CreateCondBr(condV, thenBB, elseBB);

        builder->SetInsertPoint(thenBB);
        pushScope();
        for (const auto &s : iff->thenBranch)
            generateStatement(s.get());
        if (!builder->GetInsertBlock()->getTerminator())
            builder->CreateBr(mergeBB);
        popScope();

        elseBB->insertInto(parent);
        builder->SetInsertPoint(elseBB);
        pushScope();
        for (const auto &s : iff->elseBranch)
            generateStatement(s.get());
        if (!builder->GetInsertBlock()->getTerminator())
            builder->CreateBr(mergeBB);
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
        Value *condV = asBool(generateExpr(loop->condition.get()));
        builder->CreateCondBr(condV, bodyBB, afterBB);

        bodyBB->insertInto(parent);
        builder->SetInsertPoint(bodyBB);
        pushScope();
        for (const auto &s : loop->body)
            generateStatement(s.get());
        if (!builder->GetInsertBlock()->getTerminator())
            builder->CreateBr(condBB);
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
            return ConstantInt::get(Type::getInt32Ty(*context), lit->intValue);
        else
            return ConstantInt::get(Type::getInt1Ty(*context), lit->boolValue);
    }
    else if (auto *var = dynamic_cast<VariableExpr *>(expr))
    {
        Value *ptr = lookupVar(var->id.lexeme);
        auto *alloca = dyn_cast<AllocaInst>(ptr);
        if (!alloca)
            throw runtime_error("Variable " + var->id.lexeme + " is not an AllocaInst at line " + to_string(var->id.line));
        Type *pointeeType = alloca->getAllocatedType();
        return builder->CreateLoad(pointeeType, ptr, var->id.lexeme);
    }
    else if (auto *bin = dynamic_cast<BinaryOp *>(expr))
    {
        // Short-circuit logical ops
        if (bin->op == BinaryOpKind::And)
            return emitLogicalAnd(bin->lhs.get(), bin->rhs.get());
        if (bin->op == BinaryOpKind::Or)
            return emitLogicalOr(bin->lhs.get(), bin->rhs.get());

        Value *lhs = generateExpr(bin->lhs.get());
        Value *rhs = generateExpr(bin->rhs.get());

        switch (bin->op)
        {
        // Arithmetic: operate in i32
        case BinaryOpKind::Add:  return builder->CreateAdd(asI32(lhs), asI32(rhs));
        case BinaryOpKind::Sub:  return builder->CreateSub(asI32(lhs), asI32(rhs));
        case BinaryOpKind::Mul:  return builder->CreateMul(asI32(lhs), asI32(rhs));
        case BinaryOpKind::Div:  return builder->CreateSDiv(asI32(lhs), asI32(rhs));
        case BinaryOpKind::Mod:  return builder->CreateSRem(asI32(lhs), asI32(rhs));

        // Comparisons: result is i1; compare as i32 (promote bools)
        case BinaryOpKind::Eq: return builder->CreateICmpEQ(asI32(lhs), asI32(rhs));
        case BinaryOpKind::Ne: return builder->CreateICmpNE(asI32(lhs), asI32(rhs));
        case BinaryOpKind::Lt: return builder->CreateICmpSLT(asI32(lhs), asI32(rhs));
        case BinaryOpKind::Le: return builder->CreateICmpSLE(asI32(lhs), asI32(rhs));
        case BinaryOpKind::Gt: return builder->CreateICmpSGT(asI32(lhs), asI32(rhs));
        case BinaryOpKind::Ge: return builder->CreateICmpSGE(asI32(lhs), asI32(rhs));

        case BinaryOpKind::And:
        case BinaryOpKind::Or:
            break;
        }
        throw runtime_error("Unknown binary operator");
    }
    else if (auto *un = dynamic_cast<UnaryOp *>(expr))
    {
        Value *val = generateExpr(un->expr.get());
        switch (un->op)
        {
        case UnaryOpKind::Not:    return builder->CreateNot(asBool(val));
        case UnaryOpKind::Negate: return builder->CreateNeg(asI32(val));
        }
        throw runtime_error("Unknown unary operator");
    }

    throw runtime_error("Unknown expression type at line " + to_string(expr->token.line));
}
