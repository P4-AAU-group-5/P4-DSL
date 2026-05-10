open Alcotest
open My_project
open Ast

let make_runtime program =
  Interpreter.init_runtime program


(* Rule triggers correctly *)

let test_rule_triggers () =
  let program =
    {
      devices =
        [ { name = "Heater"; location = "LivingRoom" } ];
      sensors =
        [ { name = "Temp"; location = "LivingRoom"; sensor_type = IntSensor } ];
      rules =
        [
          {
            name = "heat_rule";
            condition = SensorCompare ("Temp", Lt, 20);
            action = TurnOn "Heater";
          };
        ];
    }
  in
  let rt = make_runtime program in
  Interpreter.set_int_sensor rt "Temp" 15;
  Interpreter.simulate_minutes rt program 1;

  let devices = Interpreter.get_devices rt in
  let heater =
    List.find
      (fun (d : Interpreter.runtime_device) ->
         d.name = "Heater")
      devices
  in

  check bool "Heater should turn ON" true heater.state



(* Rule does NOT trigger incorrectly *)

let test_rule_does_not_trigger () =
  let program =
    {
      devices =
        [ { name = "Heater"; location = "LivingRoom" } ];
      sensors =
        [ { name = "Temp"; location = "LivingRoom"; sensor_type = IntSensor } ];
      rules =
        [
          {
            name = "heat_rule";
            condition = SensorCompare ("Temp", Lt, 20);
            action = TurnOn "Heater";
          };
        ];
    }
  in
  let rt = make_runtime program in
  Interpreter.set_int_sensor rt "Temp" 25;
  Interpreter.simulate_minutes rt program 1;

  let devices = Interpreter.get_devices rt in
  let heater =
    List.find
      (fun (d : Interpreter.runtime_device) ->
         d.name = "Heater")
      devices
  in

  check bool "Heater should remain OFF" false heater.state


(*  Midnight rollover works *)

let test_midnight_rollover () =
  let program =
    {
      devices = [];
      sensors = [];
      rules = [];
    }
  in
  let rt = make_runtime program in

  rt.time <- (23, 59);
  rt.day <- 1;

  Interpreter.simulate_minutes rt program 1;

  let (h, m) = Interpreter.get_time rt in
  let day = Interpreter.get_day rt in

  check (pair int int) "Time resets to 00:00" (0, 0) (h, m);
  check int "Day increments" 2 day


(* Logging only on state change *)

let test_logging_only_on_change () =
  let program =
    {
      devices =
        [ { name = "Heater"; location = "LivingRoom" } ];
      sensors =
        [ { name = "Temp"; location = "LivingRoom"; sensor_type = IntSensor } ];
      rules =
        [
          {
            name = "heat_rule";
            condition = SensorCompare ("Temp", Lt, 20);
            action = TurnOn "Heater";
          };
        ];
    }
  in
  let rt = make_runtime program in
  Interpreter.set_int_sensor rt "Temp" 15;

  Interpreter.simulate_minutes rt program 5;

  let log = Interpreter.get_log rt in
  check int "Should log only once" 1 (List.length log)


(* Boolean sensor rule works *)

let test_boolean_sensor_rule () =
  let program =
    {
      devices =
        [ { name = "Light"; location = "Hall" } ];
      sensors =
        [ { name = "Motion"; location = "Hall"; sensor_type = BoolSensor } ];
      rules =
        [
          {
            name = "motion_rule";
            condition = SensorEqualsBool ("Motion", true);
            action = TurnOn "Light";
          };
        ];
    }
  in
  let rt = make_runtime program in

  Interpreter.set_bool_sensor rt "Motion" true;
  Interpreter.simulate_minutes rt program 1;

  let devices = Interpreter.get_devices rt in
  let light =
    List.find
      (fun (d : Interpreter.runtime_device) ->
         d.name = "Light")
      devices
  in

  check bool "Light should turn ON" true light.state


(* Test Runner *)

let () =
  run "Interpreter Tests"
    [
      ( "Core Logic",
        [
          test_case "Rule triggers correctly" `Quick test_rule_triggers;
          test_case "Rule does NOT trigger incorrectly" `Quick test_rule_does_not_trigger;
          test_case "Midnight rollover works" `Quick test_midnight_rollover;
          test_case "Logging only on state change" `Quick test_logging_only_on_change;
          test_case "Boolean sensor rule works" `Quick test_boolean_sensor_rule;
        ] );
    ]