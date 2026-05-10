open Alcotest
open My_project
open Ast

let parse_string s =
  let lexbuf = Lexing.from_string s in
  Parser.program Lexer.read lexbuf

(* ================= PARSER TESTS ================= *)

let test_parse_simple_program () =
  let source =
"device Heater in LivingRoom
sensor Temp in LivingRoom : int
rule heat_rule:
if Temp < 20
then Heater.on
"
  in
  let program = parse_string source in
  check int "1 device" 1 (List.length program.devices);
  check int "1 sensor" 1 (List.length program.sensors);
  check int "1 rule" 1 (List.length program.rules)

let test_parse_boolean_rule () =
  let source =
"device Light in Hall
sensor Motion in Hall : bool
rule motion_rule:
if Motion == true
then Light.on
"
  in
  let program = parse_string source in
  check int "1 rule" 1 (List.length program.rules)

let test_invalid_syntax_fails () =
  let source =
"device Heater LivingRoom
"
  in
  check_raises
    "Invalid syntax"
    Parser.Error
    (fun () -> ignore (parse_string source))

(* ================= SEMANTIC TESTS ================= *)

let test_unknown_sensor_fails () =
  let source =
"device Heater in LivingRoom
rule bad_rule:
if Temp < 20
then Heater.on
"
  in
  let program = parse_string source in
  check_raises
    "Unknown sensor"
    (Failure "Unknown sensor: Temp")
    (fun () -> Semantic.check_program program)

let test_unknown_device_fails () =
  let source =
"sensor Temp in LivingRoom : int
rule bad_rule:
if Temp < 20
then Heater.on
"
  in
  let program = parse_string source in
  check_raises
    "Unknown device"
    (Failure "Unknown device: Heater")
    (fun () -> Semantic.check_program program)

let test_type_mismatch_fails () =
  let source =
"device Heater in LivingRoom
sensor Motion in LivingRoom : bool
rule bad_rule:
if Motion < 20
then Heater.on
"
  in
  let program = parse_string source in
  check_raises
    "Type mismatch"
    (Failure "Sensor Motion is bool, but used in numeric comparison")
    (fun () -> Semantic.check_program program)

(* ================= RUNNER ================= *)

let () =
  run "Parser & Semantic Tests"
    [
      ("Parser",
        [
          test_case "Simple program parses" `Quick test_parse_simple_program;
          test_case "Boolean rule parses" `Quick test_parse_boolean_rule;
          test_case "Invalid syntax fails" `Quick test_invalid_syntax_fails;
        ]);
      ("Semantic",
        [
          test_case "Unknown sensor fails" `Quick test_unknown_sensor_fails;
          test_case "Unknown device fails" `Quick test_unknown_device_fails;
          test_case "Type mismatch fails" `Quick test_type_mismatch_fails;
        ]);
    ]