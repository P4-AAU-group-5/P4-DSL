open Dream
open Yojson.Basic
open Yojson.Basic.Util
open Ast

let run_dsl sensorState =
  (* will be replaced - should recieve sim state and return new device state *)
  match (sensor, value) with
  | ("light", "on") -> "Light turned on"
  | ("light", "off") -> "Light turned off"
  | _ -> "Unknown command"


let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [

    Dream.post "/execute" (fun req ->
      let%lwt body = Dream.body req in
      let json = Yojson.Basic.from_string body in

      let sensorState = json |> Util.member "sensorState" |> Util.to_string in

      let result = run_dsl sensorState in

      Dream.json (Printf.sprintf {|{ "result": "%s" }|} result)
    );

  ]