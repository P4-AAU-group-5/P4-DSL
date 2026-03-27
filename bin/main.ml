open My_project
open Ast

let parse_file filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  try
    let prog = Parser.program Lexer.read lexbuf in
    close_in chan;
    prog
  with
  | Parsing.Parse_error ->
      let pos = lexbuf.lex_curr_p in
      Printf.printf "Parse error at line %d\n" pos.pos_lnum;
      exit 1

let print_program (prog : Ast.program) =
  List.iter (fun (d : Ast.device) ->
    Printf.printf "device %s in %s\n"
      d.name d.location
  ) (prog.devices : Ast.device list);

  List.iter (fun (s : Ast.sensor) ->
    Printf.printf "sensor %s in %s\n"
      s.name s.location
  ) (prog.sensors : Ast.sensor list);

  List.iter (fun (r : Ast.rule) ->
    Printf.printf "rule %s\n"
      r.name
  ) (prog.rules : Ast.rule list)

let () =
  if Array.length Sys.argv < 2 then (
    Printf.printf "Usage: %s <file>\n" Sys.argv.(0);
    exit 1
  );

  let program : Ast.program = parse_file Sys.argv.(1) in
  print_program program