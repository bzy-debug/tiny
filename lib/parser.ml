open Ast
open Token

type state = {tokens: token array; mutable cur: int}

exception ParseError of string

let precedence = [
  (Token.Add, 300);
  (Token.Star, 400);
]

let is_higher tok prec =
  match List.assoc_opt tok precedence with
  | Some p -> p > prec
  | None -> false

let prec_on_associativity tok =
  match List.assoc_opt tok precedence with
  | Some p -> p + 1
  | None ->
    let msg = Printf.sprintf "Expected operator, but got %s" (show_token_type tok) in
    raise (ParseError msg)

let peek st = st.tokens.(st.cur)

let advance st = st.cur <- st.cur + 1

let expect_token_type st tok =
  let get = peek st in
  if get.kind == tok then
    advance st
  else
    let msg =
      Printf.sprintf "Expected %s, but got %s"
      (show_token_type tok) (show_token_type get.kind)
    in
    raise (ParseError msg)

let rec parse_expr cur_state min_prec =
  let atom_lhs = ref(parse_atom cur_state) in
  let cur_tok = ref({kind = Eof; lexeme = ""; literal = LNil; line = 0}) in
  while
    cur_tok := peek(cur_state);
    is_higher !cur_tok.kind min_prec
  do
    advance cur_state;
    let atom_rhs = parse_expr cur_state (prec_on_associativity cur_tok.contents.kind) in
    atom_lhs :=
      match !cur_tok.kind with
      | Token.Add -> Ast.Add(!atom_lhs, atom_rhs)
      | Token.Star -> Ast.Mul(!atom_lhs, atom_rhs)
      | _ -> assert false
  done;
  !atom_lhs

and parse_atom st =
  let tok = peek st in
  advance st;
  match tok.kind with
  | Cst ->
      (match tok.literal with
      | LNumber i -> Ast.Cst(Int.of_float i)
      | _ ->
          let msg = Printf.sprintf "Expected a number, but got %s" (show_token_type tok.kind) in
          raise (ParseError msg))

  | LeftParen ->
      let exp = parse_expr st 0 in
      expect_token_type st RightParen;
      exp

  | Let ->
      let val_name = (peek st).lexeme in
      expect_token_type st Identifier;
      expect_token_type st Equal;
      let erhs = parse_expr st 0 in
      expect_token_type st In;
      let ebody = parse_expr st 0 in
      Let(val_name, erhs, ebody)

  | Identifier ->
      let var_name = tok.lexeme in
      Var var_name
  | _ ->
      let msg = Printf.sprintf "Expected an atom, but got %s" (show_token_type tok.kind) in
      raise (ParseError msg)
