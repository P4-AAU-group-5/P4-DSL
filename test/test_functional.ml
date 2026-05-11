open Alcotest
open My_project

(* Helper to parse DSL text *)
let parse input =
  let lexbuf = Lexing.from_string input in
  Parser.program Lexer.read lexbuf

(* ---------------------------- *)
(* Functional Test 1: Heat Rule *)
(* ---------------------------- *)

let test_functional_heat_rule () =
  let input = "
device Heater in LivingRoom
sensor Temp in LivingRoom: int

rule heat:
if Temp < 20
then Heater.on
"
  in

  let program = parse input in
  Semantic.check_program program;

  let rt = Interpreter.init_runtime program in
  Interpreter.set_int_sensor rt "Temp" 15;
  Interpreter.simulate_minutes rt program 1;

  let heater =
    List.find
      (fun (d : Interpreter.runtime_device) ->
        d.name = "Heater")
      (Interpreter.get_devices rt)
  in

  check bool "Heater should turn ON" true heater.state


(* -------------------------------- *)
(* Functional Test 2: Boolean Rule *)
(* -------------------------------- *)

let test_functional_boolean_rule () =
  let input = "
device Light in Hall
sensor Motion in Hall: bool

rule welcome:
if Motion == true
then Light.on
"
  in

  let program = parse input in
  Semantic.check_program program;

  let rt = Interpreter.init_runtime program in
  Interpreter.set_bool_sensor rt "Motion" true;
  Interpreter.simulate_minutes rt program 1;

  let light =
    List.find
      (fun (d : Interpreter.runtime_device) ->
        d.name = "Light")
      (Interpreter.get_devices rt)
  in

  check bool "Light should turn ON" true light.state


(* ------------------------------ *)
(* Functional Test 3: Time Rule  *)
(* ------------------------------ *)

let test_functional_time_rule () =
  let input = "
device Light in Hall

rule night:
if time between 22:00 .. 23:59
then Light.on
"
  in

  let program = parse input in
  Semantic.check_program program;

  let rt = Interpreter.init_runtime program in
  Interpreter.set_time rt (22, 30);
  Interpreter.simulate_minutes rt program 1;

  let light =
    List.find
      (fun (d : Interpreter.runtime_device) ->
        d.name = "Light")
      (Interpreter.get_devices rt)
  in

  check bool "Light should turn ON at night" true light.state


(* ------------------------------ *)
(* Test Runner *)
(* ------------------------------ *)

let () =
  run "Functional Tests"
    [
      ( "End-to-End",
        [
          test_case "Heat rule works" `Quick test_functional_heat_rule;
          test_case "Boolean rule works" `Quick test_functional_boolean_rule;
          test_case "Time rule works" `Quick test_functional_time_rule;
        ] );
    ]