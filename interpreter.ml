open Ast

(* Exception for runtime errors *)
exception Runtime_error of string

type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VString of string
  | VVectorInt of int list
  | VVectorFloat of float list
  | VMatrixInt of int list list
  | VMatrixFloat of float list list

(* Environment to store variables *)
module StringMap = Map.Make (String)

type environment = value StringMap.t

let env = ref StringMap.empty

(* Helper functions *)
let get_var name =
  match StringMap.find_opt name !env with
  | Some v -> v
  | None -> raise (Runtime_error ("Undefined variable: " ^ name))

let set_var name value = env := StringMap.add name value !env

(* Type conversion helpers *)
let to_int = function
  | VInt i -> i
  | _ -> raise (Runtime_error "Expected integer value")

let to_float = function
  | VFloat f -> f
  | VInt i -> float_of_int i
  | _ -> raise (Runtime_error "Expected numeric value")

let to_bool = function
  | VBool b -> b
  | _ -> raise (Runtime_error "Expected boolean value")

let to_vector_int = function
  | VVectorInt v -> v
  | _ -> raise (Runtime_error "Expected integer vector")

let to_vector_float = function
  | VVectorFloat v -> v
  | _ -> raise (Runtime_error "Expected float vector")

let to_matrix_int = function
  | VMatrixInt m -> m
  | _ -> raise (Runtime_error "Expected integer matrix")

let to_matrix_float = function
  | VMatrixFloat m -> m
  | _ -> raise (Runtime_error "Expected float matrix")

let eval_bool_binop op v1 v2 =
  match op with
  | Band -> VBool (to_bool v1 && to_bool v2)
  | Bor -> VBool (to_bool v1 || to_bool v2)
  | _ -> raise (Runtime_error "Unsupported boolean operation")

(* Basic arithmetic operations *)
let eval_int_binop op v1 v2 =
  match op with
  | Iadd -> VInt (to_int v1 + to_int v2)
  | Isub -> VInt (to_int v1 - to_int v2)
  | Imul -> VInt (to_int v1 * to_int v2)
  | Idiv ->
      if to_int v2 = 0 then raise (Runtime_error "Division by zero")
      else VInt (to_int v1 / to_int v2)
  | Ieq -> VBool (to_int v1 = to_int v2)
  | Igt -> VBool (to_int v1 > to_int v2)
  | Ilt -> VBool (to_int v1 < to_int v2)
  | Ige -> VBool (to_int v1 >= to_int v2)
  | Ile -> VBool (to_int v1 <= to_int v2)
  | Ine -> VBool (to_int v1 <> to_int v2)
  | Imod -> VInt (to_int v1 mod to_int v2)
  | Ipow ->
      VInt (int_of_float (float_of_int (to_int v1) ** float_of_int (to_int v2)))
  | _ -> raise (Runtime_error "Unsupported integer binary operation")

let eval_float_binop op v1 v2 =
  match op with
  | Fadd -> VFloat (to_float v1 +. to_float v2)
  | Fsub -> VFloat (to_float v1 -. to_float v2)
  | Fmul -> VFloat (to_float v1 *. to_float v2)
  | Fdiv ->
      if to_float v2 = 0.0 then raise (Runtime_error "Division by zero")
      else VFloat (to_float v1 /. to_float v2)
  | Feq -> VBool (to_float v1 = to_float v2)
  | Fgt -> VBool (to_float v1 > to_float v2)
  | Flt -> VBool (to_float v1 < to_float v2)
  | Fge -> VBool (to_float v1 >= to_float v2)
  | Fle -> VBool (to_float v1 <= to_float v2)
  | Fne -> VBool (to_float v1 <> to_float v2)
  | Fpow -> VFloat (to_float v1 ** to_float v2)
  | _ -> raise (Runtime_error "Unsupported float operation")

(* Vector operations *)
let eval_vector_int_binop op v1 v2 =
  match op with
  | IntVecAdd ->
      let vec1 = to_vector_int v1 and vec2 = to_vector_int v2 in
      VVectorInt (List.map2 ( + ) vec1 vec2)
  | IntVecSub ->
      let vec1 = to_vector_int v1 and vec2 = to_vector_int v2 in
      VVectorInt (List.map2 ( - ) vec1 vec2)
  | IntVecDot ->
      let vec1 = to_vector_int v1 and vec2 = to_vector_int v2 in
      VInt (List.fold_left ( + ) 0 (List.map2 ( * ) vec1 vec2))
  | IntVecEq ->
      let vec1 = to_vector_int v1 and vec2 = to_vector_int v2 in
      VBool (vec1 = vec2)
  | IntVecAngle ->
      let vec1 = to_vector_int v1 and vec2 = to_vector_int v2 in
      let dot = List.fold_left ( + ) 0 (List.map2 ( * ) vec1 vec2) in
      let mag1 =
        sqrt
          (float_of_int
             (List.fold_left ( + ) 0 (List.map (fun x -> x * x) vec1)))
      in
      let mag2 =
        sqrt
          (float_of_int
             (List.fold_left ( + ) 0 (List.map (fun x -> x * x) vec2)))
      in
      if mag1 = 0.0 || mag2 = 0.0 then raise (Runtime_error "Division by zero")
      else VFloat (acos (float_of_int dot /. (mag1 *. mag2)))
  | _ -> raise (Runtime_error "Unsupported vector operation")

let vec_index_i i vec = if i < 0 || i >= List.length vec then raise (Runtime_error "Index out of bounds") else List.nth vec i

let eval_vector_float_binop op v1 v2 =
  match op with
  | FloatVecAdd ->
      let vec1 = to_vector_float v1 and vec2 = to_vector_float v2 in
      VVectorFloat (List.map2 ( +. ) vec1 vec2)
  | FloatVecSub ->
      let vec1 = to_vector_float v1 and vec2 = to_vector_float v2 in
      VVectorFloat (List.map2 ( -. ) vec1 vec2)
  | FloatVecDot ->
      let vec1 = to_vector_float v1 and vec2 = to_vector_float v2 in
      VFloat (List.fold_left ( +. ) 0.0 (List.map2 ( *. ) vec1 vec2))
  | FloatVecEq ->
      let vec1 = to_vector_float v1 and vec2 = to_vector_float v2 in
      VBool (vec1 = vec2)
  | FloatVecAngle ->
      let vec1 = to_vector_float v1 and vec2 = to_vector_float v2 in
      let dot = List.fold_left ( +. ) 0.0 (List.map2 ( *. ) vec1 vec2) in
      let mag1 =
        sqrt (List.fold_left ( +. ) 0.0 (List.map (fun x -> x *. x) vec1))
      in
      let mag2 =
        sqrt (List.fold_left ( +. ) 0.0 (List.map (fun x -> x *. x) vec2))
      in
      if mag1 = 0.0 || mag2 = 0.0 then raise (Runtime_error "Division by zero")
      else VFloat (dot /. (mag1 *. mag2))
  | _ -> raise (Runtime_error "Unsupported vector operation")

let mat_index_i_j i j mat =
  if i < 0 || i >= List.length mat then raise (Runtime_error "Row index out of bounds");
  let row = List.nth mat i in
  if j < 0 || j >= List.length row then raise (Runtime_error "Column index out of bounds");
  List.nth row j

let eval_matrix_int_binop op v1 v2 =
  match op with
  | IntMatAdd ->
      let mat1 = to_matrix_int v1 and mat2 = to_matrix_int v2 in
      VMatrixInt
        (List.map2 (fun row1 row2 -> List.map2 ( + ) row1 row2) mat1 mat2)
  | IntMatSub ->
      let mat1 = to_matrix_int v1 and mat2 = to_matrix_int v2 in
      VMatrixInt
        (List.map2 (fun row1 row2 -> List.map2 ( - ) row1 row2) mat1 mat2)
  | IntMatMul ->
      let mat1 = to_matrix_int v1 and mat2 = to_matrix_int v2 in
      let rows1 = List.length mat1 and cols1 = List.length (List.hd mat1) in
      let rows2 = List.length mat2 and cols2 = List.length (List.hd mat2) in
      if cols1 <> rows2 then
        raise (Runtime_error "Matrix dimensions do not match")
      else
        let result =
          List.init rows1 (fun i ->
              List.init cols2 (fun j ->
                  let sum = ref 0 in
                  for k = 0 to cols1 - 1 do
                    sum :=
                      !sum
                      + List.nth (List.nth mat1 i) k
                        * List.nth (List.nth mat2 k) j
                  done;
                  !sum))
        in
        VMatrixInt result
  | IntMatVecMul ->
      let mat = to_matrix_int v2 and vec = to_vector_int v1 in
      let rows = List.length mat in
      let cols = List.length vec in
      if List.length (List.hd mat) <> cols then
        raise (Runtime_error "Matrix and vector dimensions do not match")
      else
        let result =
          List.init rows (fun i ->
              let sum = ref 0 in
              for j = 0 to cols - 1 do
                sum := !sum + (List.nth (List.nth mat i) j * List.nth vec j)
              done;
              !sum)
        in
        VVectorInt result
  | IntMatScalMul ->
      let mat = to_matrix_int v2 and scalar = to_int v1 in
      VMatrixInt (List.map (fun row -> List.map (fun x -> x * scalar) row) mat)
  | IntMatEq ->
      let mat1 = to_matrix_int v1 and mat2 = to_matrix_int v2 in
      VBool (mat1 = mat2)
  | _ -> raise (Runtime_error "Unsupported matrix operation")

let eval_matrix_float_binop op v1 v2 =
  match op with
  | FloatMatAdd ->
      let mat1 = to_matrix_float v1 and mat2 = to_matrix_float v2 in
      VMatrixFloat
        (List.map2 (fun row1 row2 -> List.map2 ( +. ) row1 row2) mat1 mat2)
  | FloatMatSub ->
      let mat1 = to_matrix_float v1 and mat2 = to_matrix_float v2 in
      VMatrixFloat
        (List.map2 (fun row1 row2 -> List.map2 ( -. ) row1 row2) mat1 mat2)
  | FloatMatMul ->
      let mat1 = to_matrix_float v1 and mat2 = to_matrix_float v2 in
      let rows1 = List.length mat1 and cols1 = List.length (List.hd mat1) in
      let rows2 = List.length mat2 and cols2 = List.length (List.hd mat2) in
      if cols1 <> rows2 then
        raise (Runtime_error "Matrix dimensions do not match")
      else
        let result =
          List.init rows1 (fun i ->
              List.init cols2 (fun j ->
                  let sum = ref 0.0 in
                  for k = 0 to cols1 - 1 do
                    sum :=
                      !sum +. (mat_index_i_j i k mat1 *. mat_index_i_j k j mat2)
                  done;
                  !sum))
        in
        VMatrixFloat result
  | FloatMatVecMul ->
      let mat = to_matrix_float v2 and vec = to_vector_float v1 in
      let rows = List.length mat in
      let cols = List.length vec in
      if List.length (List.hd mat) <> cols then
        raise (Runtime_error "Matrix and vector dimensions do not match")
      else
        let result =
          List.init rows (fun i ->
              let sum = ref 0.0 in
              for j = 0 to cols - 1 do
                sum := !sum +. (List.nth (List.nth mat i) j *. List.nth vec j)
              done;
              !sum)
        in
        VVectorFloat result
  | FloatMatScalMul ->
      let mat = to_matrix_float v2 and scalar = to_float v1 in
      VMatrixFloat
        (List.map (fun row -> List.map (fun x -> x *. scalar) row) mat)
  | FloatMatEq ->
      let mat1 = to_matrix_float v1 and mat2 = to_matrix_float v2 in
      VBool (mat1 = mat2)
  | _ -> raise (Runtime_error "Unsupported matrix operation")

let eval_bool_unop op v =
  match op with
  | BoolNeg -> VBool (not (to_bool v))
  | _ -> raise (Runtime_error "Unsupported boolean operation")

let eval_int_unop op v =
  match op with
  | IntAbs -> VInt (abs (to_int v))
  | IntToFloat -> VFloat (float_of_int (to_int v))
  | _ -> raise (Runtime_error "Unsupported integer unary operation")

let eval_float_unop op v =
  match op with
  | FloatAbs -> VFloat (abs_float (to_float v))
  | _ -> raise (Runtime_error "Unsupported float operation")

let eval_int_vec_unop op v =
  match op with
  | IntVecDim -> VInt (List.length (to_vector_int v))
  | IntVecMag ->
      VFloat
        (sqrt
           (float_of_int
              (List.fold_left ( + ) 0
                 (List.map (fun x -> x * x) (to_vector_int v)))))
  | _ -> raise (Runtime_error "Unsupported vector operation")

let eval_float_vec_unop op v =
  match op with
  | FloatVecDim -> VInt (List.length (to_vector_float v))
  | FloatVecMag ->
      VFloat
        (sqrt
           (List.fold_left ( +. ) 0.0
              (List.map (fun x -> x *. x) (to_vector_float v))))
  | _ -> raise (Runtime_error "Unsupported vector operation")

let transpose mat =
  if List.length mat = 0 then []
  else
    let rows = List.length mat and cols = List.length (List.hd mat) in
    List.init cols (fun i ->
        List.init rows (fun j -> List.nth (List.nth mat j) i))

let minor mat i j =
  if List.length mat = 0 then []
  else if List.length (List.hd mat) = 0 then []
  else
    mat
    |> List.mapi (fun row_idx row -> (row_idx, row))
    |> List.filter (fun (row_idx, _) -> row_idx <> i)
    |> List.map (fun (_, row) ->
           row
           |> List.mapi (fun col_idx elem -> (col_idx, elem))
           |> List.filter (fun (col_idx, _) -> col_idx <> j)
           |> List.map snd)

let rec int_det mat =
  if List.length mat = 0 then 0 (* Empty matrix has determinant 0 *)
  else if List.length (List.hd mat) = 0 then 0
    (* Empty matrix has determinant 0 *)
  else
    let rows = List.length mat and cols = List.length (List.hd mat) in
    if rows <> cols then raise (Runtime_error "Matrix is not square")
    else if rows = 1 then List.hd (List.hd mat)
    else
      let sign i j = if (i + j) mod 2 = 0 then 1 else -1 in
      let rec compute_sum i acc =
        if i >= rows then acc
        else
          let minor_det = int_det (minor mat i 0) in
          let term = sign i 0 * List.nth (List.nth mat i) 0 * minor_det in
          compute_sum (i + 1) (acc + term)
      in
      compute_sum 0 0

let rec float_det mat =
  if List.length mat = 0 then 0.0 (* Empty matrix has determinant 0 *)
  else if List.length (List.hd mat) = 0 then 0.0
    (* Empty matrix has determinant 0 *)
  else
    let rows = List.length mat and cols = List.length (List.hd mat) in
    if rows <> cols then raise (Runtime_error "Matrix is not square")
    else if rows = 1 then List.hd (List.hd mat)
    else
      let sign i j = if (i + j) mod 2 = 0 then 1 else -1 in
      let rec compute_sum i acc =
        if i >= rows then acc
        else
          let minor_det = float_det (minor mat i 0) in
          let term =
            float_of_int (sign i 0) *. List.nth (List.nth mat i) 0 *. minor_det
          in
          compute_sum (i + 1) (acc +. term)
      in
      compute_sum 0 0.0

let eval_int_mat_unop op v =
  match op with
  | IntMatTrans -> VMatrixInt (transpose (to_matrix_int v))
  | IntMatDet -> VInt (int_det (to_matrix_int v))
  | IntMatDimRow -> VInt (List.length (to_matrix_int v))
  | IntMatDimCol ->
      let ans =
        if List.length (to_matrix_int v) = 0 then 0
        else List.length (List.hd (to_matrix_int v))
      in
      VInt ans
  | _ -> raise (Runtime_error "Unsupported matrix operation")

let eval_float_mat_unop op v =
  match op with
  | FloatMatTrans -> VMatrixFloat (transpose (to_matrix_float v))
  | FloatMatDet -> VFloat (float_det (to_matrix_float v))
  | FloatMatDimRow -> VInt (List.length (to_matrix_float v))
  | FloatMatDimCol ->
      let ans =
        if List.length (to_matrix_float v) = 0 then 0
        else List.length (List.hd (to_matrix_float v))
      in
      VInt ans
  | _ -> raise (Runtime_error "Unsupported matrix operation")

let eval_int_mat_ternop op v1 v2 v3 =
  match op with
  | IntMatMinor -> VMatrixInt (minor (to_matrix_int v1) (to_int v2) (to_int v3))
  | _ -> raise (Runtime_error "Unsupported matrix operation")

let eval_float_mat_ternop op v1 v2 v3 =
  match op with
  | FloatMatMinor ->
      VMatrixFloat (minor (to_matrix_float v1) (to_int v2) (to_int v3))
  | _ -> raise (Runtime_error "Unsupported matrix operation")

let eval_unary_op op v =
  match v with
  | VInt _ -> eval_int_unop op v
  | VFloat _ -> eval_float_unop op v
  | VBool _ -> eval_bool_unop op v
  | VVectorInt _ -> eval_int_vec_unop op v
  | VVectorFloat _ -> eval_float_vec_unop op v
  | VMatrixInt _ -> eval_int_mat_unop op v
  | VMatrixFloat _ -> eval_float_mat_unop op v
  | _ -> raise (Runtime_error "Unsupported unary operation")

let eval_binary_op op v1 v2 =
  match v2 with
  | VInt _ -> eval_int_binop op v1 v2
  | VFloat _ -> eval_float_binop op v1 v2
  | VVectorInt _ -> eval_vector_int_binop op v1 v2
  | VVectorFloat _ -> eval_vector_float_binop op v1 v2
  | VMatrixInt _ -> eval_matrix_int_binop op v1 v2
  | VMatrixFloat _ -> eval_matrix_float_binop op v1 v2
  | _ -> raise (Runtime_error "Unsupported binary operation")

let eval_ternary_op op v1 v2 v3 =
  match v1 with
  | VMatrixInt _ -> eval_int_mat_ternop op v1 v2 v3
  | VMatrixFloat _ -> eval_float_mat_ternop op v1 v2 v3
  | _ -> raise (Runtime_error "Unsupported ternary operation")

let eval_vec_index vector index =
  match vector with
  | VVectorInt lst -> VInt (vec_index_i (to_int index) lst)
  | VVectorFloat lst -> VFloat (vec_index_i (to_int index) lst)
  | _ -> raise (Runtime_error "Invalid vector indexing")

let eval_mat_index matrix row_idx col_idx =
  match matrix with
  | VMatrixInt mat -> VInt (mat_index_i_j (to_int row_idx) (to_int col_idx) mat)
  | VMatrixFloat mat ->
      VFloat (mat_index_i_j (to_int row_idx) (to_int col_idx) mat)
  | _ -> raise (Runtime_error "Invalid matrix indexing")

let eval_data d =
  match d with
  | Int i -> VInt i
  | Float f -> VFloat f
  | Bool b -> VBool b
  | String s -> VString s
  | VectorInt (_, lst) -> VVectorInt lst
  | VectorFloat (_, lst) -> VVectorFloat lst
  | MatrixInt (_, _, mat) -> VMatrixInt mat
  | MatrixFloat (_, _, mat) -> VMatrixFloat mat

(* Main evaluation function *)
let rec eval_expr expr =
  match expr with
  | Data d -> eval_data d
  | Var name -> get_var name
  | UnaryOp (op, e) ->
      let v = eval_expr e in
      eval_unary_op op v
  | BinaryOp (op, e1, e2) ->
      let v1 = eval_expr e1 in
      let v2 = eval_expr e2 in
      eval_binary_op op v1 v2
  | TernaryOp (op, e1, e2, e3) ->
      let v1 = eval_expr e1 in
      let v2 = eval_expr e2 in
      let v3 = eval_expr e3 in
      eval_ternary_op op v1 v2 v3
  | VecIndex (e1, e2) ->
      let v1 = eval_expr e1 in
      let v2 = eval_expr e2 in
      eval_vec_index v1 v2
  | MatIndex (e1, e2, e3) ->
      let v1 = eval_expr e1 in
      let v2 = eval_expr e2 in
      let v3 = eval_expr e3 in
      eval_mat_index v1 v2 v3
  | Std_inp ->
      let line1 = read_line () in
      let line2 = read_line () in
      VString (line1 ^ "\n" ^ line2)
  | File_inp filename ->
      let channel =
        try open_in filename
        with Sys_error msg ->
          raise (Runtime_error ("Could not open file: " ^ msg))
      in
      let line1 = input_line channel in
      let line2 = try input_line channel with End_of_file -> "" in
      let trimmed_line1 = String.trim line1 in
      let trimmed_line2 = String.trim line2 in
      let content = trimmed_line1 ^ "\n" ^ trimmed_line2 in
      close_in channel;
      VString content
  | _ -> raise (Runtime_error "Unsupported expression")

let string_to_int str =
  try int_of_string (String.trim str)
  with Failure _ -> raise (Runtime_error "Invalid integer string")

let string_to_float str =
  try float_of_string (String.trim str)
  with Failure _ -> raise (Runtime_error "Invalid float string")

let string_to_bool str =
  match String.trim str with
  | "T" -> VBool true
  | "F" -> VBool false
  | _ -> raise (Runtime_error "Invalid boolean string")

let string_to_vector_int str =
  let _, lst = Lexer.str_to_int_list str in
  VVectorInt lst

let string_to_vector_float str =
  let _, lst = Lexer.str_to_float_list str in
  VVectorFloat lst

let string_to_matrix_int str =
  let _, _, lst = Lexer.str_to_int_matrix str in
  VMatrixInt lst

let string_to_matrix_float str =
  let _, _, lst = Lexer.str_to_float_matrix str in
  VMatrixFloat lst

let string_of_value value =
  match value with
  | VInt i -> string_of_int i
  | VFloat f -> string_of_float f
  | VBool b -> string_of_bool b
  | VString s -> s
  | VVectorInt lst ->
      "[" ^ String.concat ", " (List.map string_of_int lst) ^ "]"
  | VVectorFloat lst ->
      "[" ^ String.concat ", " (List.map string_of_float lst) ^ "]"
  | VMatrixInt mat ->
      "["
      ^ String.concat "; "
          (List.map
             (fun row ->
               "[" ^ String.concat ", " (List.map string_of_int row) ^ "]")
             mat)
      ^ "]"
  | VMatrixFloat mat ->
      "["
      ^ String.concat "; "
          (List.map
             (fun row ->
               "[" ^ String.concat ", " (List.map string_of_float row) ^ "]")
             mat)
      ^ "]"

(* Statement evaluation *)
let rec eval_stmt stmt =
  match stmt with
  | If (cond, then_branch, else_branch) ->
      let cond_value = eval_expr cond in
      if to_bool cond_value then eval_stmts then_branch
      else eval_stmts else_branch
  | For (init, cond, update, body) -> (
      match init with
      | Assign (var, expr) ->
          eval_stmt init;
          let rec loop () =
            let cond_value = eval_expr cond in
            if to_bool cond_value then (
              eval_stmts body;
              eval_stmt update |> ignore;
              loop ())
          in
          loop ()
      | _ -> raise (Runtime_error "Invalid for loop initialization"))
  | Assign (var, expr) -> (
      let to_assgn = eval_expr expr in
      match to_assgn with
      | VString s -> (
          match get_var var with
          | VInt _ -> set_var var (VInt (string_to_int s))
          | VFloat _ -> set_var var (VFloat (string_to_float s))
          | VBool _ -> set_var var (string_to_bool s)
          | VVectorInt orig -> (
              let new_vec_value = string_to_vector_int s in
              match new_vec_value with
              | VVectorInt new_vec ->
                  if List.length new_vec = List.length orig then
                    set_var var (VVectorInt new_vec)
                  else
                    raise (Runtime_error "Input Vector dimensions do not match")
              | _ -> raise (Runtime_error "Invalid data"))
          | VVectorFloat orig -> (
              let new_vec_value = string_to_vector_float s in
              match new_vec_value with
              | VVectorFloat new_vec ->
                  if List.length new_vec = List.length orig then
                    set_var var (VVectorFloat new_vec)
                  else
                    raise (Runtime_error "Input Vector dimensions do not match")
              | _ -> raise (Runtime_error "Invalid data"))
          | VMatrixInt orig -> (
              let new_mat_val = string_to_matrix_int s in
              match new_mat_val with
              | VMatrixInt new_mat ->
                  if
                    List.length new_mat = List.length orig
                    && List.length (List.hd new_mat)
                       = List.length (List.hd orig)
                  then set_var var (VMatrixInt new_mat)
                  else
                    raise (Runtime_error "Input Matrix dimensions do not match")
              | _ -> raise (Runtime_error "Invalid data"))
          | VMatrixFloat orig -> (
              let new_mat_value = string_to_matrix_float s in
              match new_mat_value with
              | VMatrixFloat new_mat ->
                  if
                    List.length new_mat = List.length orig
                    && List.length (List.hd new_mat)
                       = List.length (List.hd orig)
                  then set_var var (VMatrixFloat new_mat)
                  else
                    raise (Runtime_error "Input Matrix dimensions do not match")
              | _ -> raise (Runtime_error "Invalid data"))
          | _ -> raise (Runtime_error "Invalid data"))
      | _ -> set_var var to_assgn)
  | IndexAssign (index, expr) -> (
      match index with
      | VecIndex (Var var, idx_expr) -> (
          let vec_val = get_var var in
          let idx = eval_expr idx_expr in
          let new_value = eval_expr expr in
          match vec_val with
          | VVectorInt lst ->
              let i = to_int idx in
              if i < 0 || i >= List.length lst then
                raise (Runtime_error "Index out of bounds")
              else
                let new_list =
                  List.mapi
                    (fun j x -> if j = i then to_int new_value else x)
                    lst
                in
                set_var var (VVectorInt new_list)
          | VVectorFloat lst ->
              let i = to_int idx in
              if i < 0 || i >= List.length lst then
                raise (Runtime_error "Index out of bounds")
              else
                let new_list =
                  List.mapi
                    (fun j x -> if j = i then to_float new_value else x)
                    lst
                in
                set_var var (VVectorFloat new_list)
          | _ -> raise (Runtime_error "Index assignment on non-vector value"))
      | MatIndex (Var var, row_expr, col_expr) -> (
          let mat_val = get_var var in
          let row = eval_expr row_expr in
          let col = eval_expr col_expr in
          let new_value = eval_expr expr in
          match mat_val with
          | VMatrixInt mat ->
              let i = to_int row in
              let j = to_int col in
              if
                i < 0
                || i >= List.length mat
                || j < 0
                || j >= List.length (List.hd mat)
              then raise (Runtime_error "Index out of bounds")
              else
                let new_mat =
                  List.mapi
                    (fun row_idx row ->
                      if row_idx = i then
                        List.mapi
                          (fun col_idx v ->
                            if col_idx = j then to_int new_value else v)
                          row
                      else row)
                    mat
                in
                set_var var (VMatrixInt new_mat)
          | VMatrixFloat mat ->
              let i = to_int row in
              let j = to_int col in
              if
                i < 0
                || i >= List.length mat
                || j < 0
                || j >= List.length (List.hd mat)
              then raise (Runtime_error "Index out of bounds")
              else
                let new_mat =
                  List.mapi
                    (fun row_idx row ->
                      if row_idx = i then
                        List.mapi
                          (fun col_idx v ->
                            if col_idx = j then to_float new_value else v)
                          row
                      else row)
                    mat
                in
                set_var var (VMatrixFloat new_mat)
          | _ -> raise (Runtime_error "Index assignment on non-matrix value"))
      | _ -> raise (Runtime_error "Illegal indexing"))
  | While (cond, body) ->
      let rec loop () =
        let cond_value = eval_expr cond in
        if to_bool cond_value then (
          eval_stmts body;
          loop ())
      in
      loop ()
  | Output expr -> (
      try
        let value = get_var expr in
        print_endline (string_of_value value)
      with Runtime_error msg -> ( match expr with s -> print_endline s))
  | Block stmts -> eval_stmts stmts

and eval_stmts stmts =
  match stmts with
  | [] -> ()
  | stmt :: rest_stmts ->
      eval_stmt stmt;
      eval_stmts rest_stmts

and eval_program program =
  match program with
  | [] -> ()
  | stmt :: rest_stmts ->
      eval_stmt stmt;
      eval_program rest_stmts