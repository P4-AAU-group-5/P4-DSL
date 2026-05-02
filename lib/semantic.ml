open Ast

module StringMap = Map.Make (String)

type scope = {
  sensors : sensor StringMap.t;
  devices : device StringMap.t;
}

type env = scope list

(* ---- Scope / symbol-table style ---- *)

let add_unique kind name value map =
  if StringMap.mem name map then
    failwith ("Duplicate " ^ kind ^ ": " ^ name)
  else
    StringMap.add name value map

let build_global_scope (program : program) : scope =
  let sensors =
    List.fold_left
      (fun acc (s : sensor) -> add_unique "sensor" s.name s acc)
      StringMap.empty
      program.sensors
  in
  let devices =
    List.fold_left
      (fun acc (d : device) -> add_unique "device" d.name d acc)
      StringMap.empty
      program.devices
  in
  { sensors; devices }

let rec lookup_sensor_def (env : env) (name : string) : sensor option =
  match env with
  | [] -> None
  | scope :: rest ->
      if StringMap.mem name scope.sensors then
        Some (StringMap.find name scope.sensors)
      else
        lookup_sensor_def rest name

let rec lookup_device_def (env : env) (name : string) : device option =
  match env with
  | [] -> None
  | scope :: rest ->
      if StringMap.mem name scope.devices then
        Some (StringMap.find name scope.devices)
      else
        lookup_device_def rest name

let lookup_device (env : env) (name : string) : bool =
  match lookup_device_def env name with
  | Some _ -> true
  | None -> false

(* ---- Semantic checks ---- *)

let rec check_condition (cond : condition) (env : env) =
  match cond with
  | SensorCompare (name, _, _) ->
      begin
        match lookup_sensor_def env name with
        | None ->
            failwith ("Unknown sensor: " ^ name)
        | Some s ->
            match s.sensor_type with
            | IntSensor -> ()
            | BoolSensor ->
                failwith ("Sensor " ^ name ^ " is bool, but used in numeric comparison")
      end

  | SensorEqualsBool (name, _) ->
      begin
        match lookup_sensor_def env name with
        | None ->
            failwith ("Unknown sensor: " ^ name)
        | Some s ->
            match s.sensor_type with
            | BoolSensor -> ()
            | IntSensor ->
                failwith ("Sensor " ^ name ^ " is int, but used in boolean comparison")
      end

  | And (c1, c2) ->
      check_condition c1 env;
      check_condition c2 env

  | TimeBetween _ | TimeEquals _ ->
      ()

let check_action (action : action) (env : env) =
  match action with
  | TurnOn name | TurnOff name ->
      if not (lookup_device env name) then
        failwith ("Unknown device: " ^ name)

let check_rule (rule : rule) (env : env) =
  check_condition rule.condition env;
  check_action rule.action env

let check_program (program : program) =
  let global_scope = build_global_scope program in
  let env = [ global_scope ] in
  List.iter (fun (r : rule) -> check_rule r env) program.rules