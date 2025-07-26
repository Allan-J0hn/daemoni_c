#include "lexer/Lexer.h"
#include <cctype>
#include <unordered_map>

using namespace std;

Lexer::Lexer(const string &source) : src(source), start(0), current(0), line(1), column(1) {}

vector<Token> Lexer::tokenize()
{
    vector<Token> tokens;

    while (!isAtEnd())
    {
        start = current;
        char c = advance();

        // Handle single-line comments
        if (c == '/' && peek() == '/')
        {
            while (!isAtEnd() && peek() != '\n')
            {
                advance();
            }
            if (!isAtEnd())
            {
                advance(); // Consume newline
                line++;
                column = 1;
            }
            continue;
        }

        switch (c)
        {
        case '(':
            addToken(TokenType::LEFT_PAREN, tokens);
            break;
        case ')':
            addToken(TokenType::RIGHT_PAREN, tokens);
            break;
        case '{':
            addToken(TokenType::LEFT_BRACE, tokens);
            break;
        case '}':
            addToken(TokenType::RIGHT_BRACE, tokens);
            break;
        case '+':
            addToken(TokenType::PLUS, tokens);
            break;
        case '-':
            addToken(TokenType::MINUS, tokens);
            break;
        case '*':
            addToken(TokenType::STAR, tokens);
            break;
        case '/':
            addToken(TokenType::SLASH, tokens);
            break;
        case '=':
            if (peek() == '=')
            {
                advance();
                addToken(TokenType::EQUAL, tokens);
            }
            else
            {
                addToken(TokenType::ASSIGN, tokens);
            }
            break;
        case ',':
            addToken(TokenType::COMMA, tokens);
            break;
        case ';':
            addToken(TokenType::SEMICOLON, tokens);
            break;
        case '<':
            if (peek() == '=')
            {
                advance();
                addToken(TokenType::LESS_EQUAL, tokens);
            }
            else
            {
                addToken(TokenType::LESS, tokens);
            }
            break;
        case '>':
            if (peek() == '=')
            {
                advance();
                addToken(TokenType::GREATER_EQUAL, tokens);
            }
            else
            {
                addToken(TokenType::GREATER, tokens);
            }
            break;
        case '&':
            if (peek() == '&')
            {
                advance();
                addToken(TokenType::LOGICAL_AND, tokens);
            }
            else
            {
                string errChar(1, c);
                tokens.push_back({TokenType::ERROR, errChar, line, column});
            }
            break;
        case '|':
            if (peek() == '|')
            {
                advance();
                addToken(TokenType::LOGICAL_OR, tokens);
            }
            else
            {
                string errChar(1, c);
                tokens.push_back({TokenType::ERROR, errChar, line, column});
            }
            break;
        case '!':
            if (peek() == '=')
            {
                advance();
                addToken(TokenType::NOT_EQUAL, tokens);
            }
            else
            {
                addToken(TokenType::NOT, tokens);
            }
            break;
        case '%':
            addToken(TokenType::MOD, tokens);
            break;
        case ' ':
        case '\r':
        case '\t':
            column++;
            break;
        case '\n':
            line++;
            column = 1;
            break;
        default:
            if (isdigit(c))
            {
                while (!isAtEnd() && isdigit(peek()))
                    advance();
                addToken(TokenType::NUMBER, tokens);
            }
            else if (isalpha(c) || c == '_')
            {
                while (!isAtEnd() && (isalnum(peek()) || peek() == '_'))
                    advance();

                string text = src.substr(start, current - start);
                static unordered_map<string, TokenType> keywords = {
                    {"if", TokenType::IF},
                    {"else", TokenType::ELSE},
                    {"int", TokenType::INT},
                    {"bool", TokenType::BOOL},
                    {"while", TokenType::WHILE},
                    {"true", TokenType::TRUE},
                    {"false", TokenType::FALSE},
                    {"return", TokenType::RETURN}};

                auto it = keywords.find(text);
                TokenType type = (it != keywords.end()) ? it->second : TokenType::IDENTIFIER;
                tokens.push_back({type, text, line, column});
                column += current - start;
            }
            else
            {
                string errChar(1, c);
                tokens.push_back({TokenType::ERROR, errChar, line, column});
            }
            break;
        }
    }

    tokens.push_back({TokenType::END_OF_FILE, "", line, column});
    return tokens;
}

bool Lexer::isAtEnd() const
{
    return current >= src.length();
}

char Lexer::advance()
{
    current++;
    return src[current - 1];
}

char Lexer::peek() const
{
    return isAtEnd() ? '\0' : src[current];
}

void Lexer::addToken(TokenType type, vector<Token> &tokens)
{
    string text = src.substr(start, current - start);
    tokens.push_back({type, text, line, column});
    column += text.length();
}