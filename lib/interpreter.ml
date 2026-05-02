open Ast

(* ================= Runtime Types ================= *)

type runtime_device = {
  name : string;
  mutable state : bool;
}

type runtime_sensor =
  | RuntimeInt of string * int ref
  | RuntimeBool of string * bool ref

type event = {
  day : int;
  time : int * int;
  message : string;
}

type runtime = {
  devices : runtime_device list;
  sensors : runtime_sensor list;
  mutable time : int * int;
  mutable day : int;
  mutable log : event list;
}

(* ================= Initialization ================= *)

let init_runtime (program : program) : runtime =
  let runtime_devices : runtime_device list =
    List.map
      (fun (d : device) : runtime_device ->
         { name = d.name; state = false })
      program.devices
  in

  let runtime_sensors : runtime_sensor list =
    List.map
      (fun (s : sensor) ->
         match s.sensor_type with
         | IntSensor  -> RuntimeInt (s.name, ref 0)
         | BoolSensor -> RuntimeBool (s.name, ref false))
      program.sensors
  in

  {
    devices = runtime_devices;
    sensors = runtime_sensors;
    time = (0,0);
    day = 1;
    log = [];
  }

(* ================= Lookup ================= *)

let sensor_name = function
  | RuntimeInt (n, _) | RuntimeBool (n, _) -> n

let find_device rt name =
  match List.find_opt (fun d -> d.name = name) rt.devices with
  | Some d -> d
  | None -> failwith ("Device not found: " ^ name)

let find_sensor rt name =
  match List.find_opt (fun s -> sensor_name s = name) rt.sensors with
  | Some s -> s
  | None -> failwith ("Sensor not found: " ^ name)

(* ================= External Sensor Control ================= *)

let set_int_sensor rt name value =
  match find_sensor rt name with
  | RuntimeInt (_, v) -> v := value
  | _ -> failwith ("Not an int sensor: " ^ name)

let set_bool_sensor rt name value =
  match find_sensor rt name with
  | RuntimeBool (_, v) -> v := value
  | _ -> failwith ("Not a bool sensor: " ^ name)

(* ================= Condition Evaluation ================= *)

let compare a op b =
  match op with
  | Lt -> a < b
  | Gt -> a > b
  | Eq -> a = b

let rec eval_condition rt = function

  | SensorCompare (name, op, v) ->
      begin match find_sensor rt name with
      | RuntimeInt (_, value) -> compare !value op v
      | _ -> false
      end

  | SensorEqualsBool (name, v) ->
      begin match find_sensor rt name with
      | RuntimeBool (_, value) -> !value = v
      | _ -> false
      end

  | TimeBetween ((h1,m1),(h2,m2)) ->
      let (h,m) = rt.time in
      let t  = h*60 + m in
      let t1 = h1*60 + m1 in
      let t2 = h2*60 + m2 in
      if t1 <= t2 then t >= t1 && t <= t2
      else t >= t1 || t <= t2

  | TimeEquals t ->
      rt.time = t

  | And (c1,c2) ->
      eval_condition rt c1 && eval_condition rt c2

(* ================= Action Execution ================= *)

let exec_action rt rule_name = function
  | TurnOn name ->
      let d = find_device rt name in
      if not d.state then (
        d.state <- true;
        rt.log <- {
          day = rt.day;
          time = rt.time;
          message = rule_name ^ " -> " ^ name ^ " turned ON"
        } :: rt.log
      )

  | TurnOff name ->
      let d = find_device rt name in
      if d.state then (
        d.state <- false;
        rt.log <- {
          day = rt.day;
          time = rt.time;
          message = rule_name ^ " -> " ^ name ^ " turned OFF"
        } :: rt.log
      )

(* ================= Rule Evaluation ================= *)

let evaluate_rules rt program =
  List.iter
    (fun rule ->
       if eval_condition rt rule.condition then
         exec_action rt rule.name rule.action)
    program.rules

(* ================= TICK (1 minute step) ================= *)

let tick rt program =
  let (h, m) = rt.time in
  let total = h * 60 + m + 1 in

  if total >= 24 * 60 then (
    rt.time <- (0,0);
    rt.day <- rt.day + 1
  ) else (
    rt.time <- (total / 60, total mod 60)
  );

  evaluate_rules rt program

(* ================= Skip Forward ================= *)

let simulate_minutes rt program minutes =
  for _ = 1 to minutes do
    tick rt program
  done

(* ================= Getters ================= *)

let get_time rt = rt.time
let get_day rt = rt.day
let get_devices rt = rt.devices
let get_log rt = List.rev rt.log