type instr =
  ICst of int | IAdd | IMul | IVar of int | IPop | ISwap
[@@deriving show]

let rec eval_instr instrs stack =
  match (instrs, stack) with
  | ([], v::[]) -> v
  | (ICst i::tl, _) -> eval_instr tl (i::stack)
  | (IAdd::tl, v1::v2::stk) -> eval_instr tl (v1+v2::stk)
  | (IMul::tl, v1::v2::stk) -> eval_instr tl (v1*v2::stk)
  | (IVar i::tl, _) -> eval_instr tl (List.nth stack i :: stack)
  | (IPop::tl, _::stk) -> eval_instr tl stk
  | (ISwap::tl, v1::v2::stk) -> eval_instr tl (v2::v1::stk)
  | _ -> assert false
