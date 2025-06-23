%{
    open Ast
    open Printf
%}

%token EOF
%token <string> FILE_INPUT OUTPUT VAR STRING
%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token <int * int list> VECTOR_INT
%token <int * float list> VECTOR_FLOAT
%token <int * int * int list list> MATRIX_INT
%token <int * int * float list list> MATRIX_FLOAT
%token STDIN_INPUT BOOL_NEG BOOL_AND BOOL_OR 
%token INT_ADD INT_SUB INT_MUL INT_DIV INT_ABS INT_POW INT_EQ INT_GT INT_LT INT_GE INT_LE INT_NE INT_MOD INT_TO_FLOAT
%token FLOAT_ADD FLOAT_SUB FLOAT_MUL FLOAT_DIV FLOAT_ABS FLOAT_POW FLOAT_EQ FLOAT_GT FLOAT_LT FLOAT_GE FLOAT_LE FLOAT_NE 
%token COMMA
%token INT_VEC_ADD INT_VEC_SUB INT_VEC_MUL INT_VEC_DOT INT_VEC_EQ INT_VEC_ANGLE INT_VEC_DIM INT_VEC_MAG 
%token FLOAT_VEC_ADD FLOAT_VEC_SUB FLOAT_VEC_MUL FLOAT_VEC_DOT FLOAT_VEC_EQ FLOAT_VEC_ANGLE FLOAT_VEC_DIM FLOAT_VEC_MAG 
%token INT_MAT_ADD INT_MAT_SUB INT_MAT_MUL INT_MAT_VEC_MUL INT_MAT_SCAL_MUL INT_MAT_TRANS INT_MAT_DET INT_MAT_EQ INT_MAT_DIM_ROW INT_MAT_DIM_COL INT_MAT_MINOR   
%token FLOAT_MAT_ADD FLOAT_MAT_SUB FLOAT_MAT_MUL FLOAT_MAT_VEC_MUL FLOAT_MAT_SCAL_MUL FLOAT_MAT_TRANS FLOAT_MAT_DET FLOAT_MAT_EQ FLOAT_MAT_DIM_ROW FLOAT_MAT_DIM_COL FLOAT_MAT_MINOR   
%token ASSIGN SEMICOLON LCURLY RCURLY LROUND RROUND IF THEN ELSE FOR WHILE LSQBR RSQBR

/* Resolve precedence conflicts by organizing operators carefully */
/* Boolean operators */
%right ASSIGN
%left BOOL_OR
%left BOOL_AND
%nonassoc BOOL_NEG

/* Comparison operators */
%nonassoc INT_EQ INT_GT INT_LT INT_GE INT_LE INT_NE
%nonassoc FLOAT_EQ FLOAT_GT FLOAT_LT FLOAT_GE FLOAT_LE FLOAT_NE
%nonassoc INT_VEC_EQ FLOAT_VEC_EQ INT_MAT_EQ FLOAT_MAT_EQ

/* Arithmetic operators */
%left INT_ADD INT_SUB FLOAT_ADD FLOAT_SUB INT_VEC_ADD INT_VEC_SUB FLOAT_VEC_ADD FLOAT_VEC_SUB INT_MAT_ADD INT_MAT_SUB FLOAT_MAT_ADD FLOAT_MAT_SUB
%left INT_MUL INT_DIV INT_MOD FLOAT_MUL FLOAT_DIV INT_VEC_MUL INT_VEC_DOT FLOAT_VEC_MUL FLOAT_VEC_DOT INT_MAT_MUL INT_MAT_VEC_MUL INT_MAT_SCAL_MUL FLOAT_MAT_MUL FLOAT_MAT_VEC_MUL FLOAT_MAT_SCAL_MUL

/* Unary and special operators */
%right INT_ABS FLOAT_ABS INT_TO_FLOAT
%right INT_VEC_ANGLE FLOAT_VEC_ANGLE INT_VEC_DIM INT_VEC_MAG FLOAT_VEC_DIM FLOAT_VEC_MAG INT_MAT_DIM_ROW INT_MAT_DIM_COL INT_MAT_DET FLOAT_MAT_DIM_ROW FLOAT_MAT_DIM_COL FLOAT_MAT_DET
%right INT_MAT_TRANS FLOAT_MAT_TRANS INT_MAT_MINOR FLOAT_MAT_MINOR

%left INT_POW FLOAT_POW

/* Indexing has highest precedence */
%left LSQBR


%start program
%type <Ast.program> program
%type <Ast.stmt list> stmt_list
%type <Ast.stmt> stmt
%type <Ast.stmt> if_stmt
%type <Ast.stmt> for_stmt
%type <Ast.stmt> while_stmt
%type <Ast.stmt> block_stmt
%type <Ast.expr> io_expr
%type <Ast.expr> expr
%type <Ast.expr> boolean_expr
%type <Ast.expr> arithmetic_expr
%type <Ast.expr> vector_expr
%type <Ast.expr> matrix_expr
%type <Ast.expr> indexing_expr
%type <Ast.data> data

%%

program:
    | stmt_list EOF { $1 }
    ;

stmt_list:
    | /* empty */ { [] }
    | stmt stmt_list { $1 :: $2 }
    ;

stmt:
    | VAR ASSIGN expr SEMICOLON { Assign($1, $3) }
    | indexing_expr ASSIGN expr SEMICOLON { IndexAssign($1, $3) }
    | OUTPUT SEMICOLON{ Output($1) }
    | if_stmt { $1 }
    | for_stmt { $1 }
    | while_stmt { $1 }
    | block_stmt { $1 }
    ;

if_stmt:
    | IF LROUND expr RROUND THEN LCURLY stmt_list RCURLY ELSE LCURLY stmt_list RCURLY { If($3, $7, $11) }
    ;

for_stmt:
    | FOR LROUND VAR ASSIGN expr SEMICOLON expr SEMICOLON VAR ASSIGN expr RROUND LCURLY stmt_list RCURLY { For(Assign($3, $5), $7, Assign($9, $11), $14) }
    ;

while_stmt:
    | WHILE LROUND expr RROUND LCURLY stmt_list RCURLY { While($3, $6) }
    ;

block_stmt:
    | LCURLY stmt_list RCURLY { Block($2) }
    ;

expr:
    | VAR { Var($1) }
    | data { Data($1) }
    | boolean_expr { $1 }
    | arithmetic_expr { $1 }
    | vector_expr { $1 }
    | matrix_expr { $1 }
    | indexing_expr { $1 }
    | LROUND expr RROUND { $2 }
    | io_expr { $1 }
    ;

boolean_expr:
    | expr BOOL_OR expr { BinaryOp(Bor, $1, $3) }
    | expr BOOL_AND expr { BinaryOp(Band, $1, $3) }
    | BOOL_NEG expr { UnaryOp(BoolNeg, $2) }
    | expr INT_EQ expr { BinaryOp(Ieq, $1, $3) }
    | expr INT_GT expr { BinaryOp(Igt, $1, $3) }
    | expr INT_LT expr { BinaryOp(Ilt, $1, $3) }
    | expr INT_GE expr { BinaryOp(Ige, $1, $3) }
    | expr INT_LE expr { BinaryOp(Ile, $1, $3) }
    | expr INT_NE expr { BinaryOp(Ine, $1, $3) }
    | expr FLOAT_EQ expr { BinaryOp(Feq, $1, $3) }
    | expr FLOAT_GT expr { BinaryOp(Fgt, $1, $3) }
    | expr FLOAT_LT expr { BinaryOp(Flt, $1, $3) }
    | expr FLOAT_GE expr { BinaryOp(Fge, $1, $3) }
    | expr FLOAT_LE expr { BinaryOp(Fle, $1, $3) }
    | expr FLOAT_NE expr { BinaryOp(Fne, $1, $3) }
    ;

arithmetic_expr:
    | expr INT_ADD expr { BinaryOp(Iadd, $1, $3) }
    | expr INT_SUB expr { BinaryOp(Isub, $1, $3) }
    | expr INT_MUL expr { BinaryOp(Imul, $1, $3) }
    | expr INT_DIV expr { BinaryOp(Idiv, $1, $3) }
    | expr INT_MOD expr { BinaryOp(Imod, $1, $3) }
    | INT_ABS expr { UnaryOp(IntAbs, $2) }
    | expr INT_POW expr { BinaryOp(Ipow, $1, $3) }
    | INT_TO_FLOAT expr { UnaryOp(IntToFloat, $2) }
    | expr FLOAT_ADD expr { BinaryOp(Fadd, $1, $3) }
    | expr FLOAT_SUB expr { BinaryOp(Fsub, $1, $3) }
    | expr FLOAT_MUL expr { BinaryOp(Fmul, $1, $3) }
    | expr FLOAT_DIV expr { BinaryOp(Fdiv, $1, $3) }
    | FLOAT_ABS expr { UnaryOp(FloatAbs, $2) }
    | expr FLOAT_POW expr { BinaryOp(Fpow, $1, $3) }
    ;

vector_expr:
    | expr INT_VEC_ADD expr { BinaryOp(IntVecAdd, $1, $3) }
    | expr INT_VEC_SUB expr { BinaryOp(IntVecSub, $1, $3) }
    | expr INT_VEC_MUL expr { BinaryOp(IntVecMul, $1, $3) }
    | expr INT_VEC_DOT expr { BinaryOp(IntVecDot, $1, $3) }
    | expr INT_VEC_EQ expr { BinaryOp(IntVecEq, $1, $3) }
    | expr INT_VEC_ANGLE expr { BinaryOp(IntVecAngle, $1, $3) }
    | INT_VEC_DIM expr { UnaryOp(IntVecDim, $2) }
    | INT_VEC_MAG expr { UnaryOp(IntVecMag, $2) }
    | expr FLOAT_VEC_ADD expr { BinaryOp(FloatVecAdd, $1, $3) }
    | expr FLOAT_VEC_SUB expr { BinaryOp(FloatVecSub, $1, $3) }
    | expr FLOAT_VEC_MUL expr { BinaryOp(FloatVecMul, $1, $3) }
    | expr FLOAT_VEC_DOT expr { BinaryOp(FloatVecDot, $1, $3) }
    | expr FLOAT_VEC_EQ expr { BinaryOp(FloatVecEq, $1, $3) }
    | expr FLOAT_VEC_ANGLE expr { BinaryOp(FloatVecAngle, $1, $3) }
    | FLOAT_VEC_DIM expr { UnaryOp(FloatVecDim, $2) }
    | FLOAT_VEC_MAG expr { UnaryOp(FloatVecMag, $2) }
    ;

matrix_expr:
    | expr INT_MAT_ADD expr { BinaryOp(IntMatAdd, $1, $3) }
    | expr INT_MAT_SUB expr { BinaryOp(IntMatSub, $1, $3) }
    | expr INT_MAT_MUL expr { BinaryOp(IntMatMul, $1, $3) }
    | expr INT_MAT_VEC_MUL expr { BinaryOp(IntMatVecMul, $1, $3) }
    | expr INT_MAT_SCAL_MUL expr { BinaryOp(IntMatScalMul, $1, $3) }
    | expr INT_MAT_EQ expr { BinaryOp(IntMatEq, $1, $3) }
    | INT_MAT_TRANS expr { UnaryOp(IntMatTrans, $2) }
    | INT_MAT_DET expr { UnaryOp(IntMatDet, $2) }
    | INT_MAT_DIM_ROW expr { UnaryOp(IntMatDimRow, $2) }
    | INT_MAT_DIM_COL expr { UnaryOp(IntMatDimCol, $2) }
    | expr FLOAT_MAT_ADD expr { BinaryOp(FloatMatAdd, $1, $3) }
    | expr FLOAT_MAT_SUB expr { BinaryOp(FloatMatSub, $1, $3) }
    | expr FLOAT_MAT_MUL expr { BinaryOp(FloatMatMul, $1, $3) }
    | expr FLOAT_MAT_VEC_MUL expr { BinaryOp(FloatMatVecMul, $1, $3) }
    | expr FLOAT_MAT_SCAL_MUL expr { BinaryOp(FloatMatScalMul, $1, $3) }
    | expr FLOAT_MAT_EQ expr { BinaryOp(FloatMatEq, $1, $3) }
    | FLOAT_MAT_TRANS expr { UnaryOp(FloatMatTrans, $2) }
    | FLOAT_MAT_DET expr { UnaryOp(FloatMatDet, $2) }
    | FLOAT_MAT_DIM_ROW expr { UnaryOp(FloatMatDimRow, $2) }
    | FLOAT_MAT_DIM_COL expr { UnaryOp(FloatMatDimCol, $2) }
    | INT_MAT_MINOR expr expr expr { TernaryOp(IntMatMinor, $2, $3, $4) }
    | FLOAT_MAT_MINOR expr expr expr { TernaryOp(FloatMatMinor, $2, $3, $4) }
    ;

indexing_expr:
    | expr LSQBR expr RSQBR { VecIndex($1, $3) }
    | expr LSQBR expr COMMA expr RSQBR { MatIndex($1, $3, $5) }
    ;

io_expr:
    | STDIN_INPUT { Std_inp }
    | FILE_INPUT { File_inp($1) }
    ;

data:
    | INT { Int($1) }
    | FLOAT { Float($1) }
    | BOOL { Bool($1) }
    | STRING { String($1) }
    | VECTOR_INT { VectorInt(fst $1, snd $1) }
    | VECTOR_FLOAT { VectorFloat(fst $1, snd $1) }
    | MATRIX_INT { let (dim1, dim2, mat) = $1 in MatrixInt(dim1, dim2, mat) }
    | MATRIX_FLOAT { let (dim1, dim2, mat) = $1 in MatrixFloat(dim1, dim2, mat) }
    ;

%%