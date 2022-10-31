open Ast
open Utility

type nameless_expr =
  | NCst of int
  | NAdd of nameless_expr * nameless_expr
  | NMul of nameless_expr * nameless_expr
  | NVar of int
  | NLet of nameless_expr * nameless_expr
  | NFn of nameless_expr
  | NApp of nameless_expr * nameless_expr list
[@@deriving show]

(* expr -> nameless_expr *)
let lowering expr =
let rec lowering_inner venv = function
  | Cst i -> NCst i
  | Add(e1, e2) -> NAdd(lowering_inner venv e1, lowering_inner venv e2)
  | Mul(e1, e2) -> NMul(lowering_inner venv e1, lowering_inner venv e2)
  | Var s -> NVar (index venv s)
  | Let(x, e1, e2) ->
      NLet(lowering_inner venv e1, lowering_inner (x::venv) e2)
  | Fn(args, body) ->
      NFn(lowering_inner (args @ venv) body)
  | App(rator, rands) ->
      NApp(lowering_inner venv rator, List.map ( lowering_inner venv ) rands)
  in lowering_inner [] expr
