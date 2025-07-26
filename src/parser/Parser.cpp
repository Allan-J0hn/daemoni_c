#include "../../include/parser/Parser.h"
#include "../../include/ast/ASTNode.h"
#include <stdexcept>
#include <string>

using namespace std;

Parser::Parser(const vector<Token> &tokens) : tokens(tokens) {}

ASTNode *Parser::parse()
{
    if (isAtEnd())
    {
        throw runtime_error("Empty input");
    }
    ASTNode *func = parseFunction();
    if (!isAtEnd())
    {
        throw runtime_error("Unexpected tokens after function at line " + to_string(peek().line));
    }
    return func;
}

Token Parser::advance()
{
    if (!isAtEnd())
        current++;
    return tokens[current - 1];
}

Token Parser::peek() const
{
    return tokens[current];
}

bool Parser::isAtEnd() const
{
    return current >= tokens.size() || tokens[current].type == TokenType::END_OF_FILE;
}

void Parser::expect(TokenType type, const string &message)
{
    if (peek().type == type)
    {
        advance();
    }
    else
    {
        throw runtime_error(message + " at line " + to_string(peek().line));
    }
}

ASTNode *Parser::parseFunction()
{
    Token returnType = advance(); // INT or BOOL
    Token name = advance();       // IDENTIFIER

    expect(TokenType::LEFT_PAREN, "Expected '(' after function name");

    vector<pair<Token, Token>> params;

    if (peek().type != TokenType::RIGHT_PAREN)
    {
        do
        {
            Token type = advance();      // INT or BOOL
            Token paramName = advance(); // IDENTIFIER
            params.emplace_back(type, paramName);
            if (peek().type == TokenType::COMMA)
            {
                advance(); // consume comma
            }
            else
            {
                break; // no more parameters
            }
        } while (true);
    }

    expect(TokenType::RIGHT_PAREN, "Expected ')' after parameter list");

    expect(TokenType::LEFT_BRACE, "Expected '{' to start function body");

    vector<unique_ptr<Stmt>> body;
    while (!isAtEnd() && peek().type != TokenType::RIGHT_BRACE)
    {
        body.push_back(unique_ptr<Stmt>(static_cast<Stmt *>(parseStatement())));
    }

    expect(TokenType::RIGHT_BRACE, "Expected '}' to end function body");

    return new FunctionDecl(returnType, name, move(params), move(body));
}

ASTNode *Parser::parseStatement()
{
    if (peek().type == TokenType::RETURN)
    {
        advance();
        unique_ptr<Expr> expr = nullptr;
        if (peek().type != TokenType::SEMICOLON)
        {
            expr.reset(parseExpression());
        }
        expect(TokenType::SEMICOLON, "Expected ';' after return");
        return new ReturnStmt(move(expr));
    }
    else if (peek().type == TokenType::INT || peek().type == TokenType::BOOL)
    {
        Token type = advance();
        Token name = advance(); // IDENTIFIER

        unique_ptr<Expr> initializer = nullptr;
        if (peek().type == TokenType::ASSIGN)
        {
            advance();
            initializer.reset(parseExpression());
        }

        expect(TokenType::SEMICOLON, "Expected ';' after declaration");
        return new VarDeclStmt(type, name, move(initializer));
    }
    else if (peek().type == TokenType::IF)
    {
        advance();
        expect(TokenType::LEFT_PAREN, "Expected '(' after if");
        unique_ptr<Expr> condition(parseExpression());
        expect(TokenType::RIGHT_PAREN, "Expected ')' after if condition");
        expect(TokenType::LEFT_BRACE, "Expected '{' after if condition");

        vector<unique_ptr<Stmt>> thenBranch;
        while (!isAtEnd() && peek().type != TokenType::RIGHT_BRACE)
        {
            thenBranch.push_back(unique_ptr<Stmt>(static_cast<Stmt *>(parseStatement())));
        }
        expect(TokenType::RIGHT_BRACE, "Expected '}' after then branch");

        vector<unique_ptr<Stmt>> elseBranch;
        if (peek().type == TokenType::ELSE)
        {
            advance();
            expect(TokenType::LEFT_BRACE, "Expected '{' after else");
            while (!isAtEnd() && peek().type != TokenType::RIGHT_BRACE)
            {
                elseBranch.push_back(unique_ptr<Stmt>(static_cast<Stmt *>(parseStatement())));
            }
            expect(TokenType::RIGHT_BRACE, "Expected '}' after else branch");
        }

        return new IfStmt(move(condition), move(thenBranch), move(elseBranch));
    }
    else if (peek().type == TokenType::WHILE)
    {
        advance();
        expect(TokenType::LEFT_PAREN, "Expected '(' after while");
        unique_ptr<Expr> condition(parseExpression());
        expect(TokenType::RIGHT_PAREN, "Expected ')' after condition");
        expect(TokenType::LEFT_BRACE, "Expected '{' after condition");

        vector<unique_ptr<Stmt>> body;
        while (!isAtEnd() && peek().type != TokenType::RIGHT_BRACE)
        {
            body.push_back(unique_ptr<Stmt>(static_cast<Stmt *>(parseStatement())));
        }
        expect(TokenType::RIGHT_BRACE, "Expected '}' after while body");

        return new WhileStmt(move(condition), move(body));
    }
    else if (peek().type == TokenType::IDENTIFIER)
    {
        Token name = advance();
        expect(TokenType::ASSIGN, "Expected '=' after identifier");
        unique_ptr<Expr> value(parseExpression());
        expect(TokenType::SEMICOLON, "Expected ';' after assignment");
        return new AssignStmt(name, move(value));
    }

    throw runtime_error("Unexpected statement at line " + to_string(peek().line));
}

Expr *Parser::parseExpression()
{
    return parseLogicalOr();
}

Expr *Parser::parseLogicalOr()
{
    Expr *expr = parseLogicalAnd();
    while (peek().type == TokenType::LOGICAL_OR)
    {
        advance(); // consume ||
        Expr *rhs = parseLogicalAnd();
        expr = new BinaryOp(BinaryOpKind::Or, unique_ptr<Expr>(expr), unique_ptr<Expr>(rhs));
    }
    return expr;
}

Expr *Parser::parseLogicalAnd()
{
    Expr *expr = parseEquality();
    while (peek().type == TokenType::LOGICAL_AND)
    {
        advance(); // consume &&
        Expr *rhs = parseEquality();
        expr = new BinaryOp(BinaryOpKind::And, unique_ptr<Expr>(expr), unique_ptr<Expr>(rhs));
    }
    return expr;
}

Expr *Parser::parseEquality()
{
    Expr *expr = parseRelational();
    while (peek().type == TokenType::EQUAL || peek().type == TokenType::NOT_EQUAL)
    {
        Token op = advance(); // consume == or !=
        Expr *rhs = parseRelational();
        BinaryOpKind kind = (op.type == TokenType::EQUAL) ? BinaryOpKind::Eq : BinaryOpKind::Ne;
        expr = new BinaryOp(kind, unique_ptr<Expr>(expr), unique_ptr<Expr>(rhs));
    }
    return expr;
}

Expr *Parser::parseRelational()
{
    Expr *expr = parseAdditive();
    while (
        peek().type == TokenType::LESS || peek().type == TokenType::GREATER ||
        peek().type == TokenType::LESS_EQUAL || peek().type == TokenType::GREATER_EQUAL)
    {
        Token op = advance(); // consume <, >, <=, or >=
        Expr *rhs = parseAdditive();
        BinaryOpKind kind;
        if (op.type == TokenType::LESS)
            kind = BinaryOpKind::Lt;
        else if (op.type == TokenType::GREATER)
            kind = BinaryOpKind::Gt;
        else if (op.type == TokenType::LESS_EQUAL)
            kind = BinaryOpKind::Le;
        else
            kind = BinaryOpKind::Ge;
        expr = new BinaryOp(kind, unique_ptr<Expr>(expr), unique_ptr<Expr>(rhs));
    }
    return expr;
}

Expr *Parser::parseAdditive()
{
    Expr *expr = parseMultiplicative();
    while (peek().type == TokenType::PLUS || peek().type == TokenType::MINUS)
    {
        Token op = advance(); // consume + or -
        Expr *rhs = parseMultiplicative();
        BinaryOpKind kind = (op.type == TokenType::PLUS) ? BinaryOpKind::Add : BinaryOpKind::Sub;
        expr = new BinaryOp(kind, unique_ptr<Expr>(expr), unique_ptr<Expr>(rhs));
    }
    return expr;
}

Expr *Parser::parseMultiplicative()
{
    Expr *expr = parseUnary();
    while (
        peek().type == TokenType::STAR || peek().type == TokenType::SLASH ||
        peek().type == TokenType::MOD)
    {
        Token op = advance(); // consume *, /, or %
        Expr *rhs = parseUnary();
        BinaryOpKind kind;
        if (op.type == TokenType::STAR)
            kind = BinaryOpKind::Mul;
        else if (op.type == TokenType::SLASH)
            kind = BinaryOpKind::Div;
        else
            kind = BinaryOpKind::Mod;
        expr = new BinaryOp(kind, unique_ptr<Expr>(expr), unique_ptr<Expr>(rhs));
    }
    return expr;
}

Expr *Parser::parseUnary()
{
    if (peek().type == TokenType::MINUS || peek().type == TokenType::NOT)
    {
        Token op = advance(); // consume - or !
        Expr *expr = parseUnary();
        UnaryOpKind kind = (op.type == TokenType::MINUS) ? UnaryOpKind::Negate : UnaryOpKind::Not;
        return new UnaryOp(kind, unique_ptr<Expr>(expr));
    }
    return parsePrimary();
}

Expr *Parser::parsePrimary()
{
    Token token = peek();
    advance();

    if (token.type == TokenType::NUMBER)
    {
        try
        {
            int value = stoi(token.lexeme);
            return new Literal(value);
        }
        catch (...)
        {
            throw runtime_error("Invalid integer literal at line " + to_string(token.line));
        }
    }
    else if (token.type == TokenType::TRUE)
    {
        return new Literal(true);
    }
    else if (token.type == TokenType::FALSE)
    {
        return new Literal(false);
    }
    else if (token.type == TokenType::IDENTIFIER)
    {
        return new VariableExpr(token);
    }
    else if (token.type == TokenType::LEFT_PAREN)
    {
        Expr *expr = parseExpression();
        expect(TokenType::RIGHT_PAREN, "Expected ')' after expression");
        return expr;
    }

    throw runtime_error("Unexpected token in expression at line " + to_string(token.line));
}