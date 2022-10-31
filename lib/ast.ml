type expr =
  | Cst of int
  | Add of expr * expr
  | Mul of expr * expr
  | Var of string
  | Let of string * expr * expr
  | Fn of string list * expr
  | App of expr * expr list
[@@deriving show]
