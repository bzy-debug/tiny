type token_type =
  | LeftParen
  | RightParen
  | Star
  | Add
  | Nil
  | Equal  
  | Identifier
  | String
  | Cst
  | Let
  | Var
  | In
  | Eof
[@@deriving show]

type literal_type =
  | LBool of bool
  | LInt of int
  | LNumber of float
  | LString of string
  | LNil
[@@deriving show]

type token = {
  kind: token_type;
  lexeme: string;
  literal: literal_type;
  line: int
}
[@@deriving show]

let keywords_map = [
  ("let", Let);
  ("var", Var);
  ("in", In)
]
