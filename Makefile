.PHONY: all clean

OCAMLC = ocamlc
OCAMLYACC = ocamlyacc
OCAMLLEX = ocamllex

all: main

# Compile token interface and implementation
token.cmi: token.mli
	$(OCAMLC) -c token.mli

token.cmo: token.ml token.cmi
	$(OCAMLC) -c token.ml

# Compile ast
ast.cmo: ast.ml
	$(OCAMLC) -c ast.ml

# Compile type checker
type_checker.cmo: type_checker.ml ast.cmo
	$(OCAMLC) -c type_checker.ml

# Compile interpreter
interpreter.cmo: interpreter.ml ast.cmo
	$(OCAMLC) -c interpreter.ml

# Generate parser
parser.ml parser.mli: parser.mly
	$(OCAMLYACC) parser.mly

# Compile parser interface
parser.cmi: parser.mli token.cmi ast.cmo
	$(OCAMLC) -c parser.mli

# Compile parser implementation
parser.cmo: parser.ml parser.cmi
	$(OCAMLC) -c parser.ml

# Generate and compile lexer
lexer.ml: lexer.mll
	$(OCAMLLEX) lexer.mll

lexer.cmo: lexer.ml parser.cmi token.cmi
	$(OCAMLC) -c lexer.ml

# Compile main
main.cmo: main.ml ast.cmo parser.cmi lexer.cmo token.cmi type_checker.cmo interpreter.cmo
	$(OCAMLC) -c main.ml

main: token.cmo ast.cmo parser.cmo lexer.cmo type_checker.cmo interpreter.cmo main.cmo
	$(OCAMLC) -o main token.cmo ast.cmo parser.cmo lexer.cmo type_checker.cmo interpreter.cmo main.cmo

clean:
	rm -f *.cmo *.cmi parser.ml parser.mli lexer.ml main