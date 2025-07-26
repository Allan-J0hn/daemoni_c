#include "../../include/lexer/Lexer.h"
#include "../../include/parser/Parser.h"
#include "../../include/sema/SemanticAnalyzer.h"
#include "../../include/codegen/Codegen.h"
#include <fstream>
#include <iostream>
#include <vector>

using namespace std;

int main(int argc, char *argv[])
{
    if (argc != 2)
    {
        cerr << "Usage: " << argv[0] << " <input_file>" << endl;
        return 1;
    }

    // Read input file
    ifstream file(argv[1]);
    if (!file)
    {
        cerr << "Error: Could not open file: " << argv[1] << endl;
        return 1;
    }

    string source((istreambuf_iterator<char>(file)), istreambuf_iterator<char>());
    file.close();

    // Tokenize
    Lexer lexer(source);
    vector<Token> tokens = lexer.tokenize();

    for (const auto &token : tokens)
    {
        cout << "TokenType: " << static_cast<int>(token.type)
             << " Lexeme: '" << token.lexeme
             << "' Line: " << token.line
             << " Column: " << token.column << endl;
    }

    try
    {
        // Parse
        Parser parser(tokens);
        ASTNode *ast = parser.parse();

        // Semantic analysis
        SemanticAnalyzer analyzer;
        analyzer.analyze(ast);
        cout << "Parsing and semantic analysis successful!" << endl;

        // Code generation
        Codegen codegen;
        codegen.generate(ast);
        codegen.printIR();

        delete ast; // cleanup root

        cout << "Code generation successful!" << endl;
    }
    catch (const exception &e)
    {
        cerr << "Error: " << e.what() << endl;
        return 1;
    }

    return 0;
}
