open Token

type scanner = {
  source: string;
  tokens: token list;
  start: int;
  current: int;
  line: int
}

let make_scanner source = {
  source;
  tokens = [];
  start = 0;
  current = 0;
  line = 1;
}

let is_digit c = '0' <= c && c <= '9'
let is_alpha c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
let is_alpha_dight c = (is_alpha c) || (is_digit c)

let is_at_end scanner =
  scanner.current >= (String.length scanner.source)

let advance scanner =
  {scanner with current = scanner.current + 1}

let get_char scanner =
  if scanner.current > (String.length scanner.source)
  then None
  else Some (String.get scanner.source (scanner.current - 1))

let peek scanner =
  if is_at_end scanner
  then '\000'
  else String.get scanner.source scanner.current

let get_lexeme scanner =
  String.sub scanner.source scanner.start (scanner.current - scanner.start)

let add_token scanner token_type =
  let token =
    {kind = token_type; lexeme = get_lexeme scanner; literal = LNil; line = scanner.line} in
  {scanner with tokens = token :: scanner.tokens}

let add_token_with_literal scanner token_type literal =
  let token =
    {kind = token_type; lexeme = get_lexeme scanner; literal; line = scanner.line} in
  {scanner with tokens = token :: scanner.tokens}

let rec number scanner =
  let c = peek scanner in
  if (not (is_digit c || c = '.')) then
    let lexeme = get_lexeme scanner in
    let num = Float.of_string lexeme in
    add_token_with_literal scanner Cst (LNumber num)
  else
    scanner |> advance |> number

let rec identifier scanner =
  let c = peek scanner in
  if not (is_alpha_dight c) then
    let lexeme = get_lexeme scanner in
    let token_type =
    match List.assoc_opt lexeme keywords_map with
    | Some t -> t
    | None -> Identifier
    in add_token scanner token_type
  else
    scanner |> advance |> identifier

let scan_token scanner =
  let scanner = scanner |> advance in
  match (scanner |> get_char) with
  | None -> scanner
  | Some c -> match c with
    | '(' -> add_token scanner LeftParen
    | ')' -> add_token scanner RightParen
    | '*' -> add_token scanner Star
    | '+' -> add_token scanner Add
    | '=' -> add_token scanner Equal
    | ' ' | '\r' | '\t' -> scanner
    | '\n' -> {scanner with line = scanner.line + 1}
    | c when is_digit c ->
        (try number scanner
        with Failure _ -> failwith (Printf.sprintf "line[%d]: invalid number" scanner.line))
    | c when is_alpha c -> identifier scanner
    | _ -> failwith (Printf.sprintf "line[%d]: unexpected char" scanner.line)

let rec scan_tokens scanner =
  if is_at_end scanner then
    let eof = {kind = Eof; lexeme = ""; literal = LNil; line = scanner.line} in
    List.rev (eof :: scanner.tokens)
  else
    let scanner = {scanner with start = scanner.current} in
    scanner |> scan_token |> scan_tokens
