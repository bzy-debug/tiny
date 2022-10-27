open Tiny.Scanner
open Tiny.Token

let read_all_strings filename =
  let file = open_in filename in
  let s = really_input_string file (in_channel_length file) in
  s

let run code =
  let scanner = make_scanner code in
  let tokens = scan_tokens scanner in
  List.iter (fun x -> x |> show_token |> print_endline) tokens

let run_file filename =
  read_all_strings filename |> run

let rec repl () =
  print_char '>';
  try
    run (read_line());
    repl()
  with End_of_file -> ()

let run_prompt () =
  repl()


let () =
  let len = Array.length Sys.argv in
  if len > 2
  then (
    print_endline "Usage: lox [script]";
    exit(64);
  )
  else if len = 2
  then run_file Sys.argv.(1)
  else if len = 1
  then run_prompt ()
