# Daemoni-C Compiler

Daemoni-C is a toy compiler for a C-like language, generating LLVM IR from parsed ASTs with semantic checks for scope, type consistency, and variable use.

## Features
- Lexer for tokenizing C-like source code (supports identifiers, numbers, keywords, and basic operators).
- Parser for AST generation, semantic analysis, and LLVM IR codegen.
- Built with CMake for cross-platform compatibility.

## Prerequisites
- **CMake**
- **C++ Compiler**
- **LLVM**: Version 16 or higher.
- **Git**:

## Building the Project
1. Clone the repository:
   ```bash
   git clone https://github.com/Allan-J0hn/daemoni-c.git
   cd daemoni-c
