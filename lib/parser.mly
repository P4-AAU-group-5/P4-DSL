%{
open Ast

let parse_time (s : string) =
  match String.split_on_char ':' s with
  | [h; m] -> (int_of_string h, int_of_string m)
  | _ -> failwith "Invalid time"
%}

%token DEVICE SENSOR RULE IF THEN IN AND BETWEEN TIME_KW
%token LT GT EQ RANGE DOT COLON
%token TRUE FALSE
%token NEWLINE
%token <int> INT
%token <string> TIME
%token <string> IDENT
%token EOF

%start program
%type <Ast.program> program

%%

program:
  items EOF
    {
      let (devs, sens, rules) = $1 in
      { devices = devs; sensors = sens; rules = rules }
    }

(* ---------- TOP LEVEL ---------- *)

items:
  | item items
      {
        let (d1, s1, r1) = $1 in
        let (d2, s2, r2) = $2 in
        (d1 @ d2, s1 @ s2, r1 @ r2)
      }

  | NEWLINE items
      { $2 }

  | 
      { ([], [], []) }

item:
  | declaration
      {
        let (d, s) = $1 in
        (d, s, [])
      }

  | rule
      { ([], [], [$1]) }

(* ---------- DECLARATIONS ---------- *)

declaration:
  DEVICE IDENT IN IDENT
    { ([{ name = $2; location = $4 }], []) }

| SENSOR IDENT IN IDENT
    { ([], [{ name = $2; location = $4 }]) }



(* ---------- RULE ---------- *)

rule:
  RULE IDENT COLON NEWLINE IF condition NEWLINE THEN action
    {
      { name = $2; condition = $6; action = $9 }
    }

(* ---------- CONDITIONS ---------- *)

condition:
  simple_condition
    { $1 }

| condition AND simple_condition
    { And ($1, $3) }

simple_condition:
  IDENT LT INT
    { SensorCompare ($1, Lt, $3) }

| IDENT GT INT
    { SensorCompare ($1, Gt, $3) }

| IDENT EQ TRUE
    { SensorEqualsBool ($1, true) }

| IDENT EQ FALSE
    { SensorEqualsBool ($1, false) }

| TIME_KW BETWEEN TIME RANGE TIME
    { TimeBetween (parse_time $3, parse_time $5) }

| TIME_KW EQ TIME
    { TimeEquals (parse_time $3) }

(* ---------- ACTION ---------- *)

action:
  IDENT DOT IDENT
    {
      match $3 with
      | "on"  -> TurnOn $1
      | "off" -> TurnOff $1
      | _ -> failwith "Unknown action"
    }
