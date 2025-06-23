type variable = string

type data =
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
  | VectorInt of int * int list
  | VectorFloat of int * float list
  | MatrixInt of int * int * int list list
  | MatrixFloat of int * int * float list list

type binary_op =
  | Band
  | Bor
  | Iadd
  | Isub
  | Imul
  | Idiv
  | Ieq
  | Igt
  | Ilt
  | Ige
  | Ile
  | Ine
  | Imod
  | Ipow
  | Fadd
  | Fsub
  | Fmul
  | Fdiv
  | Feq
  | Fgt
  | Flt
  | Fge
  | Fle
  | Fne
  | Fpow
  | IntVecAdd
  | IntVecSub
  | IntVecMul
  | IntVecDot
  | IntVecEq
  | IntVecAngle
  | FloatVecAdd
  | FloatVecSub
  | FloatVecMul
  | FloatVecDot
  | FloatVecEq
  | FloatVecAngle
  | IntMatAdd
  | IntMatSub
  | IntMatMul
  | IntMatVecMul
  | IntMatScalMul
  | IntMatEq
  | FloatMatAdd
  | FloatMatSub
  | FloatMatMul
  | FloatMatVecMul
  | FloatMatScalMul
  | FloatMatEq

type unary_op =
  | BoolNeg
  | IntAbs
  | FloatAbs
  | IntToFloat
  | IntVecDim
  | IntVecMag
  | FloatVecDim
  | FloatVecMag
  | IntMatTrans
  | IntMatDet
  | IntMatDimRow
  | IntMatDimCol
  | FloatMatTrans
  | FloatMatDet
  | FloatMatDimRow
  | FloatMatDimCol

type ternary_op = IntMatMinor | FloatMatMinor

type expr =
  | Var of variable
  | Data of data
  | BinaryOp of binary_op * expr * expr
  | UnaryOp of unary_op * expr
  | TernaryOp of ternary_op * expr * expr * expr
  | VecIndex of expr * expr
  | MatIndex of expr * expr * expr
  | Std_inp
  | File_inp of string
  | Noexpr

type stmt =
  | If of expr * stmt list * stmt list
  | For of stmt * expr * stmt * stmt list
  | Assign of variable * expr
  | IndexAssign of expr * expr
  | While of expr * stmt list
  | Output of string
  | Block of stmt list

type program = stmt list
