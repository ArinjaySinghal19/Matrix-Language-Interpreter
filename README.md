# Matrix Language Interpreter

## Overview

This project implements an interpreter for a matrix-based language that supports operations on various data types including integers, floats, booleans, strings, vectors, and matrices. The language provides comprehensive functionality for matrix and vector operations like addition, multiplication, determinant calculation, transposition, and more.

## Features

- **Data Types**:
  - Primitive: Integer, Float, Boolean, String
  - Composite: Integer Vector, Float Vector, Integer Matrix, Float Matrix

- **Operations**:
  - Arithmetic operations (addition, subtraction, multiplication, division, etc.)
  - Logical operations (AND, OR, NOT)
  - Matrix operations (transpose, determinant, minor)
  - Vector operations (dot product, magnitude, angle)
  - Type conversion (int to float)
  - Comparison operations

- **Control Flow**:
  - If-then-else statements
  - For loops
  - While loops
  - Code blocks

- **I/O**:
  - File input/output operations
  - Standard input

## Project Structure

- **`ast.ml`**: Abstract Syntax Tree definition for the language
- **`lexer.mll`**: Lexical analyzer using OCamllex
- **`parser.mly`**: Parser using Menhir/OCamlyacc
- **`interpreter.ml`**: Interpreter for executing the parsed AST
- **`token.ml`/`token.mli`**: Token definitions for lexing
- **`type_checker.ml`**: Type checking for expressions and statements
- **`main.ml`**: Main entry point to run the interpreter
- **`Makefile`**: Build script for the project
- **`test_cases/`**: Directory containing test programs

## Building and Running

### Prerequisites

- OCaml (recommended version 4.13.0 or higher)
- OCamllex and Menhir/OCamlyacc

### Build Instructions

```bash
make
```

This will compile the source files and generate an executable.

### Running the Interpreter

```bash
./main <filename>
```

Where `<filename>` is the path to a source file containing matrix language code.

To run interactively:

```bash
./main
```

## Language Syntax Examples

### Arithmetic Operations

```
a := 5 + 3;
b := 10 / 2;
c := a * b;
```

### Vector Operations

```
vec1 := 3
[1,2,3];
vec2 := 3
[4,5,6];
result := vec1 vi+ vec2;  // Integer vector addition
dot_product := vec1 vi. vec2;  // Dot product
```

### Matrix Operations

```
A := 2,2
[[1,2],[3,4]];
B := 2,2
[[5,6],[7,8]];
C := A mi+ B;  // Matrix addition
D := A mi* B;  // Matrix multiplication
det := mi|^| A;  // Determinant
trans := mi' A;  // Transpose
```

### Control Flow

```
if (x > 5) then {
    y := x * 2;
} else {
    y := x / 2;
}

for (i := 0; i < 10; i := i + 1) {
    sum := sum + i;
}

while (x > 0) {
    x := x - 1;
}
```

## Test Cases

The `test_cases/` directory contains various example programs demonstrating the language features:

- Matrix operations (determinant, transpose, inverse)
- Gaussian elimination
- Control flow examples
- Vector operations
- Error testing examples

## Contributors

This project was developed as part of the COL226 Programming Languages course.