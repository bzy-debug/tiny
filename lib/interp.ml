open Ast

(* expr interpreter *)
let rec eval env = function
  | Cst i -> i
  | Add (e1, e2) -> eval env e1 + eval env e2
  | Mul (e1, e2) -> eval env e1 * eval env e2
  | Var x -> List.assoc x env
  | Let (x, e1, e2) ->
      let v1 = eval env e1 in
      eval ((x, v1)::env) e2

