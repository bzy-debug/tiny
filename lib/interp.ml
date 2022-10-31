open Ast

exception RuntimeError of string

type value =
  | VInt of int
  | VClosure of env * string list * expr

and env = (string * value) list

let vadd v1 v2 =
  match (v1, v2) with
  | (VInt i1, VInt i2) -> VInt(i1 + i2)
  | _ -> raise (RuntimeError "vadd: not int" )

let vmul v1 v2 =
  match (v1, v2) with
  | (VInt i1, VInt i2) -> VInt(i1 * i2)
  | _ -> raise (RuntimeError "vmul: not int" )


(* expr interpreter *)
let rec eval env = function
  | Cst i -> VInt i
  | Add (e1, e2) -> vadd ( eval env e1 ) ( eval env e2 )
  | Mul (e1, e2) -> vmul ( eval env e1 ) ( eval env e2 )
  | Var x -> List.assoc x env
  | Let (x, e1, e2) ->
      let v1 = eval env e1 in
      eval ((x, v1)::env) e2
  | Fn (args, body) -> VClosure(env, args, body)
  | App (rator, rands) ->
      let closure = eval env rator in
      match closure with
      | VInt _ -> raise (RuntimeError "eval: rator not closure")
      | VClosure(env_rands, args, body) ->
        begin
          let val_rands = List.map (eval env) rands in
          let fun_env = ( List.combine args val_rands ) @ env_rands in
          eval fun_env body
        end
