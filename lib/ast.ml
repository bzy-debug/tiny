type expr =
  | Cst of int
  | Add of expr * expr
  | Mul of expr * expr
  | Var of string
  | Let of string * expr * expr
[@@deriving show]
