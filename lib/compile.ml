open Ir
open Vm

type sv = Slocal | Stmp

let sindex senv i =
  let rec sindex_aux senv i acc =
    if i = -1 then acc - 1
    else match senv with
    | Slocal::tl -> sindex_aux tl (i-1) (acc+1)
    | Stmp::tl -> sindex_aux tl i acc + 1
    | [] -> failwith "sindex" in
  sindex_aux senv i 0

let compile_nameless expr =
  let rec go senv expr =
    match expr with
    | NCst i -> [ICst i]
    | NAdd (e1, e2) -> go senv e1 @ go (Stmp::senv) e2 @ [IAdd]
    | NMul (e1, e2) -> go senv e1 @ go (Stmp::senv) e2 @ [IMul]
    | NVar i -> [IVar (sindex senv i)]
    | NLet (e1, e2) -> go senv e1 @ go (Slocal::senv) e2 @ [ISwap; IPop] in
  go [] expr
