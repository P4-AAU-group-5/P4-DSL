open My_project
open Interpreter

let parse_file filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  let prog = Parser.program Lexer.read lexbuf in
  close_in chan;
  prog

let () =
  if Array.length Sys.argv < 2 then (
    Printf.printf "Usage: %s <file>\n" Sys.argv.(0);
    exit 1
  );

  let filename = Sys.argv.(1) in
  let program = parse_file filename in

  (* Semantic checks *)
  Semantic.check_program program;

  (* Initialize runtime *)
  let runtime = init_runtime program in

  (* Example: manually set sensor values for testing *)
  set_int_sensor runtime "Temp" 18;
  set_bool_sensor runtime "Motion" true;

  (* Simulate 2 days *)
  simulate_minutes runtime program (2 * 24 * 60);

  (* Print final time *)
  let (h, m) = get_time runtime in
  Printf.printf "\nFinal Day: %d\n" (get_day runtime);
  Printf.printf "Final Time: %02d:%02d\n" h m;

  (* Print event log *)
  Printf.printf "\n=== EVENT LOG ===\n";

  List.iter
    (fun (e : event) ->
       let (h, m) = e.time in
       Printf.printf "Day %d %02d:%02d  %s\n"
         e.day h m e.message)
    (get_log runtime)