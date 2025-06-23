open Ast
open Printf
open Lexer
open Parser
open Type_checker

(* Function to convert data type to string *)
(* Function to print data type *)
let string_of_data = function
  | Int i -> sprintf "Int(%d)" i
  | Float f -> sprintf "Float(%f)" f
  | Bool b -> sprintf "Bool(%s)" (if b then "true" else "false")
  | String s -> sprintf "String(%s)" s
  | VectorInt (dim, vals) ->
      sprintf "VectorInt(%d, [%s])" dim
        (String.concat ", " (List.map string_of_int vals))
  | VectorFloat (dim, vals) ->
      sprintf "VectorFloat(%d, [%s])" dim
        (String.concat ", " (List.map string_of_float vals))
  | MatrixInt (rows, cols, mat) ->
      let row_strings =
        List.map
          (fun row ->
            "[" ^ String.concat ", " (List.map string_of_int row) ^ "]")
          mat
      in
      sprintf "MatrixInt(%d, %d, [%s])" rows cols
        (String.concat ", " row_strings)
  | MatrixFloat (rows, cols, mat) ->
      let row_strings =
        List.map
          (fun row ->
            "[" ^ String.concat ", " (List.map string_of_float row) ^ "]")
          mat
      in
      sprintf "MatrixFloat(%d, %d, [%s])" rows cols
        (String.concat ", " row_strings)

let string_of_ternary_op = function
  | IntMatMinor -> "IntMatMinor"
  | FloatMatMinor -> "FloatMatMinor"

(* Function to print binary operators *)
let string_of_binop = function
  | Band -> "Band"
  | Bor -> "Bor"
  | Iadd -> "Iadd"
  | Isub -> "Isub"
  | Imul -> "Imul"
  | Idiv -> "Idiv"
  | Ieq -> "Ieq"
  | Igt -> "Igt"
  | Ilt -> "Ilt"
  | Ige -> "Ige"
  | Ile -> "Ile"
  | Ine -> "Ine"
  | Imod -> "Imod"
  | Ipow -> "Ipow"
  | Fadd -> "Fadd"
  | Fsub -> "Fsub"
  | Fmul -> "Fmul"
  | Fdiv -> "Fdiv"
  | Fpow -> "Fpow"
  | Feq -> "Feq"
  | Fgt -> "Fgt"
  | Flt -> "Flt"
  | Fge -> "Fge"
  | Fle -> "Fle"
  | Fne -> "Fne"
  | IntVecAdd -> "IntVecAdd"
  | IntVecSub -> "IntVecSub"
  | IntVecMul -> "IntVecMul"
  | IntVecDot -> "IntVecDot"
  | IntVecEq -> "IntVecEq"
  | IntVecAngle -> "IntVecAngle"
  | FloatVecAdd -> "FloatVecAdd"
  | FloatVecSub -> "FloatVecSub"
  | FloatVecMul -> "FloatVecMul"
  | FloatVecDot -> "FloatVecDot"
  | FloatVecEq -> "FloatVecEq"
  | FloatVecAngle -> "FloatVecAngle"
  | IntMatAdd -> "IntMatAdd"
  | IntMatSub -> "IntMatSub"
  | IntMatMul -> "IntMatMul"
  | IntMatVecMul -> "IntMatVecMul"
  | IntMatScalMul -> "IntMatScalMul"
  | IntMatEq -> "IntMatEq"
  | FloatMatAdd -> "FloatMatAdd"
  | FloatMatSub -> "FloatMatSub"
  | FloatMatMul -> "FloatMatMul"
  | FloatMatVecMul -> "FloatMatVecMul"
  | FloatMatScalMul -> "FloatMatScalMul"
  | FloatMatEq -> "FloatMatEq"

(* Function to print unary operators *)
let string_of_unop = function
  | BoolNeg -> "BoolNeg"
  | IntAbs -> "IntAbs"
  | FloatAbs -> "FloatAbs"
  | IntToFloat -> "IntToFloat"
  | IntVecDim -> "IntVecDim"
  | IntVecMag -> "IntVecMag"
  | FloatVecDim -> "FloatVecDim"
  | FloatVecMag -> "FloatVecMag"
  | IntMatTrans -> "IntMatTrans"
  | IntMatDet -> "IntMatDet"
  | IntMatDimRow -> "IntMatDimRow"
  | IntMatDimCol -> "IntMatDimCol"
  | FloatMatTrans -> "FloatMatTrans"
  | FloatMatDet -> "FloatMatDet"
  | FloatMatDimRow -> "FloatMatDimRow"
  | FloatMatDimCol -> "FloatMatDimCol"

(* Recursive function to print expressions *)
let rec string_of_expr = function
  | Var v -> sprintf "Var(%s)" v
  | Data d -> string_of_data d
  | TernaryOp (op, e1, e2, e3) ->
      sprintf "TernaryOp(%s, %s, %s, %s)" (string_of_ternary_op op)
        (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
  | BinaryOp (op, e1, e2) ->
      sprintf "BinaryOp(%s, %s, %s)" (string_of_binop op) (string_of_expr e1)
        (string_of_expr e2)
  | UnaryOp (op, e) ->
      sprintf "UnaryOp(%s, %s)" (string_of_unop op) (string_of_expr e)
  | VecIndex (e1, e2) ->
      sprintf "VecIndex(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | MatIndex (e1, e2, e3) ->
      sprintf "MatIndex(%s, %s, %s)" (string_of_expr e1) (string_of_expr e2)
        (string_of_expr e3)
  | Std_inp -> "Std_inp"
  | File_inp s -> sprintf "File_inp(%s)" s
  | Noexpr -> "Noexpr"

(* Function to print statements *)
let rec string_of_stmt indent = function
  | Assign (v, e) -> sprintf "Assign(%s, %s)" v (string_of_expr e)
  | IndexAssign (e1, e2) ->
      sprintf "IndexAssign(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | If (cond, then_stmts, else_stmts) ->
      sprintf "%sIf(%s,\n%s\n%s)" indent (string_of_expr cond)
        (string_of_stmts (indent ^ "  ") then_stmts)
        (string_of_stmts (indent ^ "  ") else_stmts)
  | For (init, cond, incr, body) ->
      sprintf "%sFor(%s, %s, %s,\n%s)" indent
        (string_of_stmt indent init)
        (string_of_expr cond)
        (string_of_stmt indent incr)
        (string_of_stmts (indent ^ "  ") body)
  | While (cond, body) ->
      sprintf "%sWhile(%s,\n%s)" indent (string_of_expr cond)
        (string_of_stmts (indent ^ "  ") body)
  | Block stmts ->
      sprintf "%sBlock(\n%s\n%s)" indent
        (string_of_stmts (indent ^ "  ") stmts)
        indent
  | Output s -> sprintf "Output(%s)" s

(* Function to print a list of statements *)
and string_of_stmts indent stmts =
  String.concat ",\n" (List.map (string_of_stmt indent) stmts)

(* Function to print a program *)
let string_of_program program =
  "Program:\n" ^ string_of_stmts "  " (List.rev program)

(* Function to tokenize input *)
let tokenize input =
  let lexbuf = Lexing.from_string input in
  let rec loop tokens =
    match
      try Lexer.token lexbuf with
      | Failure msg -> failwith ("Lexing error: " ^ msg)
      | _ -> failwith "Unknown lexing error"
    with
    | EOF -> List.rev (EOF :: tokens)
    | token -> loop (token :: tokens)
  in
  loop []

(* Function to print tokens *)
let string_of_token = function
  | FILE_INPUT s -> sprintf "FILE_INPUT(%s)" s
  | STDIN_INPUT -> "STDIN_INPUT"
  | OUTPUT s -> sprintf "OUTPUT(%s)" s
  | VAR s -> sprintf "VAR(%s)" s
  | BOOL b -> sprintf "BOOL(%b)" b
  | INT i -> sprintf "INT(%d)" i
  | FLOAT f -> sprintf "FLOAT(%f)" f
  | STRING s -> sprintf "STRING(%s)" s
  | VECTOR_INT (dim, lst) ->
      sprintf "VECTOR_INT(%d, [%s])" dim
        (String.concat ", " (List.map string_of_int lst))
  | VECTOR_FLOAT (dim, lst) ->
      sprintf "VECTOR_FLOAT(%d, [%s])" dim
        (String.concat ", " (List.map string_of_float lst))
  | MATRIX_INT (r, c, mat) -> sprintf "MATRIX_INT(%d, %d, ...)" r c
  | MATRIX_FLOAT (r, c, mat) -> sprintf "MATRIX_FLOAT(%d, %d, ...)" r c
  | BOOL_NEG -> "BOOL_NEG"
  | BOOL_AND -> "BOOL_AND"
  | BOOL_OR -> "BOOL_OR"
  | INT_ADD -> "INT_ADD"
  | INT_SUB -> "INT_SUB"
  | INT_MUL -> "INT_MUL"
  | INT_DIV -> "INT_DIV"
  | INT_ABS -> "INT_ABS"
  | INT_POW -> "INT_POW"
  | INT_EQ -> "INT_EQ"
  | INT_GT -> "INT_GT"
  | INT_LT -> "INT_LT"
  | INT_GE -> "INT_GE"
  | INT_LE -> "INT_LE"
  | INT_NE -> "INT_NE"
  | INT_MOD -> "INT_MOD"
  | INT_TO_FLOAT -> "INT_TO_FLOAT"
  | FLOAT_ADD -> "FLOAT_ADD"
  | FLOAT_SUB -> "FLOAT_SUB"
  | FLOAT_MUL -> "FLOAT_MUL"
  | FLOAT_DIV -> "FLOAT_DIV"
  | FLOAT_ABS -> "FLOAT_ABS"
  | FLOAT_POW -> "FLOAT_POW"
  | FLOAT_EQ -> "FLOAT_EQ"
  | FLOAT_GT -> "FLOAT_GT"
  | FLOAT_LT -> "FLOAT_LT"
  | FLOAT_GE -> "FLOAT_GE"
  | FLOAT_LE -> "FLOAT_LE"
  | FLOAT_NE -> "FLOAT_NE"
  | LSQBR -> "LSQBR"
  | RSQBR -> "RSQBR"
  | COMMA -> "COMMA"
  | INT_VEC_ADD -> "INT_VEC_ADD"
  | INT_VEC_SUB -> "INT_VEC_SUB"
  | INT_VEC_MUL -> "INT_VEC_MUL"
  | INT_VEC_DOT -> "INT_VEC_DOT"
  | INT_VEC_EQ -> "INT_VEC_EQ"
  | INT_VEC_ANGLE -> "INT_VEC_ANGLE"
  | INT_VEC_DIM -> "INT_VEC_DIM"
  | INT_VEC_MAG -> "INT_VEC_MAG"
  | FLOAT_VEC_ADD -> "FLOAT_VEC_ADD"
  | FLOAT_VEC_SUB -> "FLOAT_VEC_SUB"
  | FLOAT_VEC_MUL -> "FLOAT_VEC_MUL"
  | FLOAT_VEC_DOT -> "FLOAT_VEC_DOT"
  | FLOAT_VEC_EQ -> "FLOAT_VEC_EQ"
  | FLOAT_VEC_ANGLE -> "FLOAT_VEC_ANGLE"
  | FLOAT_VEC_DIM -> "FLOAT_VEC_DIM"
  | FLOAT_VEC_MAG -> "FLOAT_VEC_MAG"
  | INT_MAT_ADD -> "INT_MAT_ADD"
  | INT_MAT_SUB -> "INT_MAT_SUB"
  | INT_MAT_MUL -> "INT_MAT_MUL"
  | INT_MAT_VEC_MUL -> "INT_MAT_VEC_MUL"
  | INT_MAT_SCAL_MUL -> "INT_MAT_SCAL_MUL"
  | INT_MAT_TRANS -> "INT_MAT_TRANS"
  | INT_MAT_DET -> "INT_MAT_DET"
  | INT_MAT_EQ -> "INT_MAT_EQ"
  | INT_MAT_DIM_ROW -> "INT_MAT_DIM_ROW"
  | INT_MAT_DIM_COL -> "INT_MAT_DIM_COL"
  | INT_MAT_MINOR -> "INT_MAT_MINOR"
  | FLOAT_MAT_ADD -> "FLOAT_MAT_ADD"
  | FLOAT_MAT_SUB -> "FLOAT_MAT_SUB"
  | FLOAT_MAT_MUL -> "FLOAT_MAT_MUL"
  | FLOAT_MAT_VEC_MUL -> "FLOAT_MAT_VEC_MUL"
  | FLOAT_MAT_SCAL_MUL -> "FLOAT_MAT_SCAL_MUL"
  | FLOAT_MAT_TRANS -> "FLOAT_MAT_TRANS"
  | FLOAT_MAT_DET -> "FLOAT_MAT_DET"
  | FLOAT_MAT_EQ -> "FLOAT_MAT_EQ"
  | FLOAT_MAT_DIM_ROW -> "FLOAT_MAT_DIM_ROW"
  | FLOAT_MAT_DIM_COL -> "FLOAT_MAT_DIM_COL"
  | FLOAT_MAT_MINOR -> "FLOAT_MAT_MINOR"
  | ASSIGN -> "ASSIGN"
  | SEMICOLON -> "SEMICOLON"
  | LCURLY -> "LCURLY"
  | RCURLY -> "RCURLY"
  | LROUND -> "LROUND"
  | RROUND -> "RROUND"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | FOR -> "FOR"
  | WHILE -> "WHILE"
  | EOF -> "EOF"

(* Function to tokenize input *)
let tokenize input =
  let lexbuf = Lexing.from_string input in
  let rec loop tokens =
    match
      try token lexbuf with
      | Failure msg -> failwith ("Lexing error: " ^ msg)
      | _ -> failwith "Unknown lexing error"
    with
    | EOF -> List.rev (EOF :: tokens)
    | t -> loop (t :: tokens)
  in
  loop []

(* Function to parse input *)
let parse input =
  let lexbuf = Lexing.from_string input in
  try program token lexbuf with
  | Parsing.Parse_error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      let line = pos.Lexing.pos_lnum in
      let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
      failwith (sprintf "Parse error at line %d, character %d" line col)
  | Failure msg -> failwith ("Error: " ^ msg)

(* Main function *)
let () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    try
      let ic = open_in filename in
      try
        let content = really_input_string ic (in_channel_length ic) in
        close_in ic;
        try
          let parsed_program = parse content in
          try
            (match Type_checker.type_check parsed_program with
            | Ok program -> ()
            | Error msg -> failwith ("Type error: " ^ msg));
            Printf.printf "Type checking completed successfully\n";

            (* Write AST to output_ast.txt *)
            let ast_file = "output_ast.txt" in
            let ast_oc = open_out ast_file in
            Printf.fprintf ast_oc "Program AST:\n%s\n"
              (string_of_program parsed_program);
            close_out ast_oc;
            Printf.printf "AST written to %s\n" ast_file;

            (* Run the interpreter *)
            try
              Printf.printf "Running program...\n";
              Interpreter.eval_program parsed_program;
              Printf.printf "Program execution completed successfully\n"
            with
            | Interpreter.Runtime_error msg ->
                Printf.printf "Runtime error: %s\n" msg
            | e -> Printf.printf "Execution error: %s\n" (Printexc.to_string e)
          with Failure msg -> Printf.printf "Type error: %s\n" msg
        with Failure msg ->
          Printf.printf "Type checking/parsing error: %s\n" msg
      with e ->
        close_in_noerr ic;
        Printf.printf "Error reading file: %s\n" (Printexc.to_string e)
    with Sys_error msg -> Printf.printf "Could not open %s: %s\n" filename msg
