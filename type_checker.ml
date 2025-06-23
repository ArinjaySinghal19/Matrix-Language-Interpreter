open Ast

type data_type =
  | IntType
  | FloatType
  | BoolType
  | StringType
  | VectorIntType of int
  | VectorFloatType of int
  | MatrixIntType of int * int
  | MatrixFloatType of int * int
  | AssignType of data_type
  | InputType

let rec data_type_of_data = function
  | Int _ -> IntType
  | Float _ -> FloatType
  | Bool _ -> BoolType
  | String _ -> StringType
  | VectorInt (dim, vec) ->
      if List.length vec = dim then VectorIntType dim
      else raise (Failure "Invalid vector")
  | VectorFloat (dim, vec) ->
      if List.length vec = dim then VectorFloatType dim
      else raise (Failure "Invalid vector")
  | MatrixInt (rows, cols, mat) ->
      if
        List.length mat = rows
        && List.for_all (fun row -> List.length row = cols) mat
      then MatrixIntType (rows, cols)
      else raise (Failure "Invalid matrix")
  | MatrixFloat (rows, cols, mat) ->
      if
        List.length mat = rows
        && List.for_all (fun row -> List.length row = cols) mat
      then MatrixFloatType (rows, cols)
      else raise (Failure "Invalid matrix")

let rec string_of_data_type = function
  | IntType -> "Int"
  | FloatType -> "Float"
  | BoolType -> "Bool"
  | StringType -> "String"
  | VectorIntType dim -> "VectorInt(" ^ string_of_int dim ^ ")"
  | VectorFloatType dim -> "VectorFloat(" ^ string_of_int dim ^ ")"
  | MatrixIntType (rows, cols) ->
      "MatrixInt(" ^ string_of_int rows ^ ", " ^ string_of_int cols ^ ")"
  | MatrixFloatType (rows, cols) ->
      "MatrixFloat(" ^ string_of_int rows ^ ", " ^ string_of_int cols ^ ")"
  | AssignType ty -> "Assign(" ^ string_of_data_type ty ^ ")"
  | InputType -> "Input"

let rec lookup env var = Hashtbl.find env var
(* with Not_found -> raise (Failure ("Variable not found: " ^ var)) *)

let add env var ty = Hashtbl.add env var ty

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

let rec check_unop env op e =
  match (op, e) with
  | BoolNeg, BoolType -> BoolType
  | IntAbs, IntType -> IntType
  | IntToFloat, IntType -> FloatType
  | FloatAbs, FloatType -> FloatType
  | IntVecDim, VectorIntType _ -> IntType
  | IntVecMag, VectorIntType _ -> IntType
  | FloatVecDim, VectorFloatType _ -> IntType
  | FloatVecMag, VectorFloatType _ -> IntType
  | IntMatTrans, MatrixIntType (r, c) -> MatrixIntType (c, r)
  | IntMatDet, MatrixIntType (_, _) -> IntType
  | IntMatDimRow, MatrixIntType (_, _) -> IntType
  | IntMatDimCol, MatrixIntType (_, _) -> IntType
  | FloatMatTrans, MatrixFloatType (r, c) -> MatrixFloatType (c, r)
  | FloatMatDet, MatrixFloatType (_, _) -> FloatType
  | FloatMatDimRow, MatrixFloatType (_, _) -> IntType
  | FloatMatDimCol, MatrixFloatType (_, _) -> IntType
  | _ ->
      raise
        (Failure
           ("Invalid unary operator: " ^ string_of_data_type e ^ " "
          ^ string_of_unop op))

let rec string_of_binop = function
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

let rec check_binop env e1 op e2 =
  match (e1, op, e2) with
  | IntType, Band, IntType -> BoolType
  | BoolType, Bor, BoolType -> BoolType
  | IntType, Iadd, IntType -> IntType
  | IntType, Isub, IntType -> IntType
  | IntType, Imul, IntType -> IntType
  | IntType, Idiv, IntType -> IntType
  | IntType, Ieq, IntType -> BoolType
  | IntType, Igt, IntType -> BoolType
  | IntType, Ilt, IntType -> BoolType
  | IntType, Ige, IntType -> BoolType
  | IntType, Ile, IntType -> BoolType
  | IntType, Ine, IntType -> BoolType
  | IntType, Imod, IntType -> IntType
  | IntType, Ipow, IntType -> IntType
  | FloatType, Fadd, FloatType -> FloatType
  | FloatType, Fsub, FloatType -> FloatType
  | FloatType, Fmul, FloatType -> FloatType
  | FloatType, Fdiv, FloatType -> FloatType
  | FloatType, Fpow, FloatType -> FloatType
  | FloatType, Feq, FloatType -> BoolType
  | FloatType, Fgt, FloatType -> BoolType
  | FloatType, Flt, FloatType -> BoolType
  | FloatType, Fge, FloatType -> BoolType
  | FloatType, Fle, FloatType -> BoolType
  | FloatType, Fne, FloatType -> BoolType
  | VectorIntType d1, IntVecAdd, VectorIntType d2 when d1 = d2 ->
      VectorIntType d1
  | VectorIntType d1, IntVecSub, VectorIntType d2 when d1 = d2 ->
      VectorIntType d1
  | VectorIntType d1, IntVecMul, VectorIntType d2 when d1 = d2 -> IntType
  | VectorIntType d1, IntVecDot, VectorIntType d2 when d1 = d2 -> IntType
  | VectorIntType d1, IntVecEq, VectorIntType d2 when d1 = d2 -> BoolType
  | VectorIntType d1, IntVecAngle, VectorIntType d2 when d1 = d2 -> FloatType
  | VectorFloatType d1, FloatVecAdd, VectorFloatType d2 when d1 = d2 ->
      VectorFloatType d1
  | VectorFloatType d1, FloatVecSub, VectorFloatType d2 when d1 = d2 ->
      VectorFloatType d1
  | VectorFloatType d1, FloatVecMul, VectorFloatType d2 when d1 = d2 ->
      FloatType
  | VectorFloatType d1, FloatVecDot, VectorFloatType d2 when d1 = d2 ->
      FloatType
  | VectorFloatType d1, FloatVecEq, VectorFloatType d2 when d1 = d2 -> BoolType
  | VectorFloatType d1, FloatVecAngle, VectorFloatType d2 when d1 = d2 ->
      FloatType
  | MatrixIntType (r1, c1), IntMatAdd, MatrixIntType (r2, c2)
    when r1 = r2 && c1 = c2 ->
      MatrixIntType (r1, c1)
  | MatrixIntType (r1, c1), IntMatSub, MatrixIntType (r2, c2)
    when r1 = r2 && c1 = c2 ->
      MatrixIntType (r1, c1)
  | MatrixIntType (r1, c1), IntMatMul, MatrixIntType (r2, c2) when c1 = r2 ->
      MatrixIntType (r1, c2)
  | VectorIntType d1, IntMatVecMul, MatrixIntType (r, c) when d1 = r ->
      MatrixIntType (r, c)
  | IntType, IntMatScalMul, MatrixIntType (r, c) -> MatrixIntType (r, c)
  | MatrixIntType (r1, c1), IntMatEq, MatrixIntType (r2, c2)
    when r1 = r2 && c1 = c2 ->
      BoolType
  | MatrixFloatType (r1, c1), FloatMatAdd, MatrixFloatType (r2, c2)
    when r1 = r2 && c1 = c2 ->
      MatrixFloatType (r1, c1)
  | MatrixFloatType (r1, c1), FloatMatSub, MatrixFloatType (r2, c2)
    when r1 = r2 && c1 = c2 ->
      MatrixFloatType (r1, c1)
  | MatrixFloatType (r1, c1), FloatMatMul, MatrixFloatType (r2, c2) when c1 = r2
    ->
      MatrixFloatType (r1, c2)
  | VectorFloatType d1, FloatMatVecMul, MatrixFloatType (r, c) when d1 = r ->
      MatrixFloatType (r, c)
  | FloatType, FloatMatScalMul, MatrixFloatType (r, c) -> MatrixFloatType (r, c)
  | MatrixFloatType (r1, c1), FloatMatEq, MatrixFloatType (r2, c2)
    when r1 = r2 && c1 = c2 ->
      BoolType
  | _ ->
      raise
        (Failure
           ("Invalid binary operator: " ^ string_of_data_type e1 ^ " "
          ^ string_of_binop op ^ " " ^ string_of_data_type e2))

let rec string_of_ternary_op = function
  | IntMatMinor -> "IntMatMinor"
  | FloatMatMinor -> "FloatMatMinor"

let rec check_ternary_op env op e1 e2 e3 =
  match (op, e1, e2, e3) with
  | IntMatMinor, MatrixIntType (r, c), IntType, IntType when r = c ->
      MatrixIntType (r - 1, c - 1)
  | FloatMatMinor, MatrixFloatType (r, c), IntType, IntType when r = c ->
      MatrixFloatType (r - 1, c - 1)
  | _ ->
      raise
        (Failure
           ("Invalid ternary operator: " ^ string_of_ternary_op op ^ " "
          ^ string_of_data_type e1 ^ " " ^ string_of_data_type e2 ^ " "
          ^ string_of_data_type e3))

let rec check_vec_index env e1 e2 =
  match (e1, e2) with
  | VectorIntType _, IntType -> IntType
  | VectorFloatType _, IntType -> FloatType
  | _ ->
      raise
        (Failure
           ("Invalid vector index: " ^ string_of_data_type e1 ^ " "
          ^ string_of_data_type e2))

let rec check_mat_index env e1 e2 e3 =
  match (e1, e2, e3) with
  | MatrixIntType (_, _), IntType, IntType -> IntType
  | MatrixFloatType (_, _), IntType, IntType -> FloatType
  | _ ->
      raise
        (Failure
           ("Invalid matrix index: " ^ string_of_data_type e1 ^ " "
          ^ string_of_data_type e2 ^ " " ^ string_of_data_type e3))

let rec check_expr env = function
  | Var v -> (
      try lookup env v
      with Not_found -> raise (Failure ("Variable not found: " ^ v)))
  | Data d -> data_type_of_data d
  | TernaryOp (op, e1, e2, e3) ->
      check_ternary_op env op (check_expr env e1) (check_expr env e2)
        (check_expr env e3)
  | BinaryOp (op, e1, e2) ->
      check_binop env (check_expr env e1) op (check_expr env e2)
  | UnaryOp (op, e) -> check_unop env op (check_expr env e)
  | VecIndex (e1, e2) ->
      check_vec_index env (check_expr env e1) (check_expr env e2)
  | MatIndex (e1, e2, e3) ->
      check_mat_index env (check_expr env e1) (check_expr env e2)
        (check_expr env e3)
  | Std_inp -> InputType
  | File_inp _ -> InputType
  | Noexpr -> BoolType

let rec check_stmt env = function
  | Assign (v, e) -> (
      let ty = check_expr env e in
      try
        let existing_type = lookup env v in
        match ty with
        | InputType ->
            existing_type (* For input to existing var, keep existing type *)
        | _ ->
            if existing_type = ty then AssignType ty
            else
              raise
                (Failure
                   ("Type mismatch in assignment: variable " ^ v ^ " has type "
                   ^ string_of_data_type existing_type
                   ^ " but expression has type " ^ string_of_data_type ty))
      with Not_found -> (
        match ty with
        | InputType ->
            raise
              (Failure
                 ("Cannot initialize variable " ^ v ^ " with input without type"))
        | _ ->
            add env v ty;
            AssignType ty))
  | IndexAssign (e1, e2) ->
      let ty1 = check_expr env e1 in
      let ty2 = check_expr env e2 in
      if ty1 = ty2 then AssignType ty1
      else
        raise
          (Failure
             ("Invalid index assignment: " ^ string_of_data_type ty1 ^ " "
            ^ string_of_data_type ty2))
  | If (cond, then_stmts, else_stmts) ->
      if check_expr env cond = BoolType then
        let _ = process_stmts env then_stmts in
        let _ = process_stmts env else_stmts in
        BoolType
      else
        raise
          (Failure
             ("Invalid if condition: "
             ^ string_of_data_type (check_expr env cond)))
  | While (cond, body) ->
      if check_expr env cond = BoolType then
        let _ = process_stmts env body in
        BoolType
      else
        raise
          (Failure
             ("Invalid while condition: "
             ^ string_of_data_type (check_expr env cond)))
  | Block stmts -> process_stmts env stmts
  | Output _ -> BoolType
  | For (init, cond, incr, body) -> (
      match (check_stmt env init, check_expr env cond, check_stmt env incr) with
      | AssignType _, BoolType, AssignType _ ->
          let _ = process_stmts env body in
          BoolType
      | _ ->
          raise
            (Failure
               ("Invalid for loop: "
               ^ string_of_data_type (check_stmt env init)
               ^ " "
               ^ string_of_data_type (check_expr env cond)
               ^ " "
               ^ string_of_data_type (check_stmt env incr))))

and process_stmts env stmts =
  match stmts with
  | [] -> BoolType
  | hd :: tl ->
      let _ = check_stmt env hd in
      process_stmts env tl

let type_check program =
  let env = Hashtbl.create 100 in
  try
    let _ = process_stmts env program in
    Ok program
  with Failure msg -> Error msg