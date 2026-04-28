open Ast

let check_sensor_exists (name : string) (sensors : sensor list) =
  List.exists (fun (s : sensor) -> s.name = name) sensors

  let find_sensor (name : string) (sensors : sensor list) =
  List.find_opt (fun (s : sensor) -> s.name = name) sensors

let check_device_exists (name : string) (devices : device list) =
  List.exists (fun (d : device) -> d.name = name) devices

let rec check_condition (cond : condition) (program : program) =
  match cond with
  
 (* | SensorCompare (name, _, _) ->
      if not (check_sensor_exists name program.sensors) then
        failwith ("Unknown sensor: " ^ name) *)
    | SensorCompare (name, _, _) ->
    begin match find_sensor name program.sensors with
    | None ->
        failwith ("Unknown sensor: " ^ name)

    | Some s ->
        if s.sensor_type <> IntSensor then
          failwith ("Type error: sensor " ^ name ^ " must be int")
    end

  | SensorEqualsBool (name, _) ->
      if not (check_sensor_exists name program.sensors) then
        failwith ("Unknown sensor: " ^ name)

  | And (c1, c2) ->
      check_condition c1 program;
      check_condition c2 program

  | _ -> ()

let check_action (action : action) (program : program) =
  match action with
  | TurnOn name | TurnOff name ->
      if not (check_device_exists name program.devices) then
        failwith ("Unknown device: " ^ name)

let check_rule (rule : rule) (program : program) =
  check_condition rule.condition program;
  check_action rule.action program

let check_program (program : program) =
  List.iter (fun (r : rule) -> check_rule r program) program.rules
