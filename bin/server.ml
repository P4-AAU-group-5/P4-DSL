open Dream
open Yojson.Basic

let run_dsl device action =
  (* will be replaced - should recieve sim state and return new device state *)
  match (sim, value) with
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

      let device = json |> Util.member "device" |> Util.to_string in
      let action = json |> Util.member "action" |> Util.to_string in

      let result = run_dsl sim action in

      Dream.json (Printf.sprintf {|{ "result": "%s" }|} result)
    );

  ]