{
    open String
    open Parser


let rec sci_not_to_float (str: string) : float = match String.split_on_char 'e' str with
    [num; exp] -> (float_of_string num) *. (10.0 ** (float_of_string exp))
    | _ -> raise (Failure "Invalid scientific notation")

let bool_of_string (str: string) : bool = match str with
    "T" -> true
    | "F" -> false
    | _ -> raise (Failure "Invalid boolean input")




let rec str_to_int_list_helper (str: string list) : int list = match str with
    [] -> []
    | num::rest -> int_of_string num :: str_to_int_list_helper rest

let rec str_to_int_list (str : string) = match String.split_on_char '\n' str with
    [dim; vector] -> if vector="[]" then ((int_of_string dim), [])
                     else
                    let int_list = String.sub vector 1 ((String.length vector) - 2) |> String.split_on_char ',' |> str_to_int_list_helper
                     in 
                     ((int_of_string dim), int_list)
    | _ -> raise (Failure "Invalid vector input")

let rec str_to_float_list_helper (str: string list) : float list = match str with
    [] -> []
    | num::rest -> if String.contains num 'e' then  sci_not_to_float num :: str_to_float_list_helper rest
                   else float_of_string num :: str_to_float_list_helper rest

let rec str_to_float_list (str : string) = match String.split_on_char '\n' str with
    [dim; vector] -> if vector="[]" then ((int_of_string dim), [])
                     else
                    let float_list = String.sub vector 1 ((String.length vector) - 2) |> String.split_on_char ',' |> str_to_float_list_helper
                     in 
                     ((int_of_string dim), float_list)
    | _ -> raise (Failure "Invalid vector input")


let str_to_int_matrix (str : string) = match String.split_on_char '\n' str with
  | dims::matrix::[] -> let dim_list = String.split_on_char ',' dims |> List.map int_of_string in
      
      
                        let matrix_content = String.trim matrix |> fun s -> String.sub s 1 (String.length s - 2) 
                        in
                        
                        let row_strings = 
                        String.split_on_char ']' matrix_content 
                        |> List.filter (fun s -> String.trim s <> "")
                        |> List.map (fun s ->  let cleaned = 
                                                String.trim s |> fun s -> if s.[0] = ',' then String.sub s 2 (String.length s - 2) else String.sub s 1 (String.length s - 1)
                            in
                            String.split_on_char ',' cleaned
                        )
                        in
                        
                        (* Convert row strings to int lists *)
                        let int_matrix = 
                            List.map str_to_int_list_helper row_strings 
                        in
                        
                        (* Return dimensions and matrix *)
                        (List.nth dim_list 0, List.nth dim_list 1, int_matrix)
  
  | _ -> raise (Failure "Invalid matrix input")
    


let str_to_float_matrix (str : string) = match String.split_on_char '\n' str with
    | dims::matrix::[] -> let dim_list = String.split_on_char ',' dims |> List.map int_of_string in
        
        
                            let matrix_content = String.trim matrix |> fun s -> String.sub s 1 (String.length s - 2) 
                            in
                            
                            let row_strings = 
                            String.split_on_char ']' matrix_content 
                            |> List.filter (fun s -> String.trim s <> "")
                            |> List.map (fun s ->  let cleaned = 
                                                    String.trim s |> fun s -> if s.[0] = ',' then String.sub s 2 (String.length s - 2) else String.sub s 1 (String.length s - 1)
                                in
                                String.split_on_char ',' cleaned
                            )
                            in
                            
                            (* Convert row strings to int lists *)
                            let float_matrix = 
                                List.map str_to_float_list_helper row_strings 
                            in
                            
                            (* Return dimensions and matrix *)
                            (List.nth dim_list 0, List.nth dim_list 1, float_matrix)
    
    | _ -> raise (Failure "Invalid matrix input")




let empty_vi_init (str: string) : (int* int list) = match String.split_on_char ' ' str with
    [_; num] -> let n = int_of_string num in (n, List.init n (fun _ -> 0))
    | _ -> raise (Failure "Invalid empty vector input")

let empty_fi_init (str: string) : int * float list = match String.split_on_char ' ' str with
    [_; num] -> let n = int_of_string num in (n, List.init n (fun _ -> 0.0))
    | _ -> raise (Failure "Invalid empty vector input")

let empty_mi_init (str: string) : int*int * int list list = match String.split_on_char ' ' str with
    [_; r; c] -> let rows = int_of_string r in let cols = int_of_string c in (rows, cols, List.init rows (fun _ -> List.init cols (fun _ -> 0)))
    | _ -> raise (Failure "Invalid empty matrix input")

let empty_mf_init (str: string) : int * int * float list list = match String.split_on_char ' ' str with
    [_; r; c] -> let rows = int_of_string r in let cols = int_of_string c in (rows, cols, List.init rows (fun _ -> List.init cols (fun _ -> 0.0)))
    | _ -> raise (Failure "Invalid empty matrix input")

}





let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let float = '-'? (digit+ '.' digit*) | (digit* '.' digit+)
let int = '-'? digit+
let sci_not = (int | float) 'e' int
let string_regex = '"' [^'"']* '"'
let bool = 'T' | 'F'
let ws = [' ' '\t' '\n' '\r']
let comm_start = "~:(~"
let comm_end = "~:)~"
let var =  (letter | digit | '_' | ''')*
let file_inp = "Input(" [^')']+ ")"
let inp = "Input()"
let vi = "[" (int ",")* int "]" | "[" "]"
let vector_int = (int "\n" vi)
let vf = "[" ((float|sci_not) ",")* (float|sci_not) "]" | "[" "]"
let vector_float = (int "\n" vf)
let matrix_int = (int "," int "\n" "[" (vi ",")* vi "]")
let matrix_float = (int "," int "\n" "[" (vf ",")* vf "]")
let data = int | float | bool | string_regex | vector_int | vector_float | matrix_int | matrix_float
let print = "Print(" (data | var) ")"

let empty_vi = ( "empty_vi " digit+ )
let empty_fi = ( "empty_fi " digit+ )
let empty_mi = ( "empty_mi " digit+ " " digit+ )
let empty_mf = ( "empty_mf " digit+ " " digit+ )



rule token = parse
    | eof { EOF }
    | ws { token lexbuf }
    | comm_start { comment lexbuf }
    | file_inp { FILE_INPUT (String.sub (Lexing.lexeme lexbuf) 6 ((String.length (Lexing.lexeme lexbuf)) - 7)) }
    | inp { STDIN_INPUT }
    | print { OUTPUT (String.sub (Lexing.lexeme lexbuf) 6 ((String.length (Lexing.lexeme lexbuf)) - 7)) }
    | bool { BOOL (bool_of_string (Lexing.lexeme lexbuf)) }
    | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | string_regex {STRING (String.sub (Lexing.lexeme lexbuf) 1 ((String.length (Lexing.lexeme lexbuf)) - 2))}
    | sci_not { FLOAT (sci_not_to_float (Lexing.lexeme lexbuf)) }
    | vector_int { let (dim, vec) = (str_to_int_list (Lexing.lexeme lexbuf)) in VECTOR_INT (dim, vec) }
    | empty_vi { let (dim, vec) = (empty_vi_init (Lexing.lexeme lexbuf)) in VECTOR_INT (dim, vec) }
    | vector_float { let (dim, vec) = (str_to_float_list (Lexing.lexeme lexbuf)) in VECTOR_FLOAT (dim, vec) }
    | empty_fi { let (dim, vec) = (empty_fi_init (Lexing.lexeme lexbuf)) in VECTOR_FLOAT (dim, vec) }
    | matrix_int { let (dim1, dim2, mat) = (str_to_int_matrix (Lexing.lexeme lexbuf)) in MATRIX_INT (dim1, dim2, mat) }
    | empty_mi { let (dim1, dim2, mat) = (empty_mi_init (Lexing.lexeme lexbuf)) in MATRIX_INT (dim1, dim2, mat) }
    | matrix_float { let (dim1, dim2, mat) = (str_to_float_matrix (Lexing.lexeme lexbuf)) in MATRIX_FLOAT (dim1, dim2, mat) }
    | empty_mf { let mat = (empty_mf_init (Lexing.lexeme lexbuf)) in MATRIX_FLOAT mat }
    | "b~" { BOOL_NEG }
    | "b&" { BOOL_AND }
    | "b|" { BOOL_OR }
    | "[" { LSQBR }
    | "]" { RSQBR }
    | "," { COMMA }
    | "+" { INT_ADD }
    | "-" { INT_SUB }
    | "*" { INT_MUL }
    | "/" { INT_DIV }
    | "abs" { INT_ABS }
    | "^" { INT_POW }
    | "==" { INT_EQ }
    | ">" { INT_GT }
    | "<" { INT_LT }
    | ">=" { INT_GE }
    | "<=" { INT_LE }
    | "!=" { INT_NE }
    | "%" { INT_MOD }
    | "itf" { INT_TO_FLOAT }
    | "f+" { FLOAT_ADD }
    | "f-" { FLOAT_SUB }
    | "f*" { FLOAT_MUL }
    | "f/" { FLOAT_DIV }
    | "fabs" { FLOAT_ABS }
    | "f^" { FLOAT_POW }
    | "f==" { FLOAT_EQ }
    | "f>" { FLOAT_GT }
    | "f<" { FLOAT_LT }
    | "f>=" { FLOAT_GE }
    | "f<=" { FLOAT_LE }
    | "f!=" { FLOAT_NE }
    | "vi+" { INT_VEC_ADD }
    | "vi-" { INT_VEC_SUB }
    | "vi*" { INT_VEC_MUL }
    | "vi." { INT_VEC_DOT }
    | "vi==" { INT_VEC_EQ }
    | "vi/_" { INT_VEC_ANGLE}
    | "vi#" { INT_VEC_DIM }
    | "vi::" { INT_VEC_MAG }
    | "vf+" { FLOAT_VEC_ADD }
    | "vf-" { FLOAT_VEC_SUB }
    | "vf*" { FLOAT_VEC_MUL }
    | "vf." { FLOAT_VEC_DOT }
    | "vf==" { FLOAT_VEC_EQ }
    | "vf/_" { FLOAT_VEC_ANGLE}
    | "vf#" { FLOAT_VEC_DIM }
    | "vf::" { FLOAT_VEC_MAG }
    | "mi+" { INT_MAT_ADD }
    | "mi-" { INT_MAT_SUB }
    | "mi*" { INT_MAT_MUL }
    | "mivi*" { INT_MAT_VEC_MUL }
    | "misi*" { INT_MAT_SCAL_MUL }
    | "mi'" { INT_MAT_TRANS } 
    | "mi|^|" { INT_MAT_DET }
    | "mi==" { INT_MAT_EQ }
    | "mi#r" { INT_MAT_DIM_ROW }
    | "mi#c" { INT_MAT_DIM_COL }
    | "minori" { INT_MAT_MINOR }
    | "mf+" { FLOAT_MAT_ADD }
    | "mf-" { FLOAT_MAT_SUB }
    | "mf*" { FLOAT_MAT_MUL }
    | "mfvf*" { FLOAT_MAT_VEC_MUL }
    | "mfvf*" { FLOAT_MAT_SCAL_MUL }
    | "mf'" { FLOAT_MAT_TRANS }
    | "mf|^|" { FLOAT_MAT_DET }
    | "mf==" { FLOAT_MAT_EQ }
    | "mf#r" { FLOAT_MAT_DIM_ROW }
    | "mf#c" { FLOAT_MAT_DIM_COL }
    | "minorf" { FLOAT_MAT_MINOR }
    | ":=" { ASSIGN }
    | ";" { SEMICOLON }
    | "{" { LCURLY }
    | "}" { RCURLY }
    | "(" {LROUND}
    | ")" {RROUND}
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "for" { FOR }
    | "while" { WHILE }
    | var { VAR (Lexing.lexeme lexbuf) }
    | _ { raise (Failure ("illegal character " ^ Lexing.lexeme lexbuf)) }

and comment = parse 
    | comm_end { token lexbuf }
    | _ { comment lexbuf }
    | eof { raise (Failure "unterminated comment") }














