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
  | Parser.Error ->
      close_in chan;
      let pos = lexbuf.lex_curr_p in
      Printf.printf "Parse error at line %d\n" pos.pos_lnum;
      exit 1

let print_program (prog : Ast.program) =
  List.iter (fun (d : Ast.device) ->
    Printf.printf "device %s in %s\n - value: %s" d.name d.location d.state
  ) prog.devices;

  List.iter (fun (s : Ast.sensor) ->
    Printf.printf "sensor %s in %s - value: %s \n" s.name s.location s.value
  ) prog.sensors;

  List.iter (fun (r : Ast.rule) ->
    Printf.printf "rule %s\n" r.name
  ) prog.rules

let () =
  if Array.length Sys.argv < 2 then (
    Printf.printf "Usage: %s <file>\n" Sys.argv.(0);
    exit 1
  );

  let program = parse_file Sys.argv.(1) in
  
  Semantic.check_program program;

  print_program program

