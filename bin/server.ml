open Dream
open Yojson.Basic

let run_dsl device action =
  (* will be replaced - should recieve sim state and return new device state *)
  match (sensor, value) with
  | ("lighe", "on") -> "Light turned on"
  | ("light", "off") -> "Light turned off"
  | _ -> "Unknown command"


let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [

    Dream.post "/execute" (fun req ->
      let%lwt body = Dream.body req in
      let json = Yojson.Basic.from_string body in

      let sensor = json |> Util.member "sensor" |> Util.to_string in
      let value = json |> Util.member "value" |> Util.to_string in

      let result = run_dsl sim value in

      Dream.json (Printf.sprintf {|{ "result": "%s" }|} result)
    );

  ]