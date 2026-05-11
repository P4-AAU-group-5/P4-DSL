open Lwt.Infix
open Yojson.Basic.Util
open My_project

let program_of_file filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  let program = Parser.program Lexer.read lexbuf in
  close_in chan;
  program

let int_of_json = function
  | `Int i -> Some i
  | _ -> None

let bool_of_json = function
  | `Bool b -> Some b
  | _ -> None

let set_sensor rt name = function
  | `Int i -> (try Interpreter.set_int_sensor rt name i with _ -> ())
  | `Bool b -> (try Interpreter.set_bool_sensor rt name b with _ -> ())
  | _ -> ()

let update_sensors rt json =
  json
  |> to_assoc
  |> List.iter (fun (name, value) -> set_sensor rt name value)

let update_devices rt json =
  json
  |> to_assoc
  |> List.iter (fun (name, value) ->
         match bool_of_json value with
         | Some b -> (try Interpreter.set_device_state rt name b with _ -> ())
         | None -> ())

let update_time rt json =
  match int_of_json (json |> member "hour"), int_of_json (json |> member "minute") with
  | Some h, Some m -> Interpreter.set_time rt (h, m)
  | _ -> () ;
  match int_of_json (json |> member "day") with
  | Some d -> Interpreter.set_day rt d
  | None -> ()

let devices_to_json (devices : Interpreter.runtime_device list) =
  `List
    (List.map
       (fun d ->
         `Assoc
           [ ("name", `String (Interpreter.runtime_device_name d))
           ; ("state", `Bool (Interpreter.runtime_device_state d))
           ])
       devices)

let runtime_to_json rt =
  let (h, m) = Interpreter.get_time rt in
  let day = Interpreter.get_day rt in
  `Assoc
    [ ("devices", devices_to_json (Interpreter.get_devices rt))
    ; ("time", `Assoc [ ("hour", `Int h); ("minute", `Int m); ("day", `Int day) ])
    ]

let execute_request runtime program body =
  let json = Yojson.Basic.from_string body in

  (match json |> member "sensorState" with
  | `Null -> ()
  | sensors -> update_sensors runtime sensors);

  (match json |> member "deviceState" with
  | `Null -> ()
  | devices -> update_devices runtime devices);

  (match json |> member "time" with
  | `Null -> ()
  | time -> update_time runtime time);

  Interpreter.evaluate_rules runtime program;
  runtime_to_json runtime

let handler runtime program req =
  Dream.body req >>= fun body ->
  let response = execute_request runtime program body in
  Dream.json (Yojson.Basic.to_string response)

let () =
  let program_file =
    if Array.length Sys.argv >= 2 then Sys.argv.(1)
    else failwith "Usage: server.exe <program-file>"
  in
  let program = program_of_file program_file in

  (* Semantic checks *)
  Semantic.check_program program;

  let runtime = ref (Interpreter.init_runtime program) in

  Dream.run
    (Dream.logger
       (Dream.router
          [ Dream.post "/execute" (fun req -> handler !runtime program req) ]))
 