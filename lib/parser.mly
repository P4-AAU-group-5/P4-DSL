%{
open Ast
%}

%token DEVICE SENSOR RULE IF THEN IN AND BETWEEN TIME_KW
%token LT GT EQ RANGE DOT
%token TRUE FALSE
%token COLON
%token <string> IDENT
%token <int> INT
%token <string> TIME
%token EOF

%start program
%type <Ast.program> program
%type <Ast.device list * Ast.sensor list> declarations
%type <Ast.device list * Ast.sensor list> declaration
%type <Ast.rule list> rules
%type <Ast.rule> rule
%type <Ast.condition> condition
%type <Ast.condition> simple_condition
%type <Ast.action> action

%%

program:
  declarations rules EOF
    {
      let (devs, sens) = $1 in
      {
        devices = devs;
        sensors = sens;
        rules = $2;
      }
    }

(* ---------- DECLARATIONS ---------- *)

declarations:
  | declaration declarations
      {
        let (d1, s1) = $1 in
        let (d2, s2) = $2 in
        (d1 @ d2, s1 @ s2)
      }
  | 
      { ([], []) }

declaration:
  DEVICE IDENT IN IDENT
      { ([{ name = $2; location = $4 }], []) }

| SENSOR IDENT IN IDENT
      { ([], [{ name = $2; location = $4 }]) }

(* ---------- RULES ---------- *)

rules:
  | rule rules { $1 :: $2 }
  |            { [] }

rule:
  RULE IDENT COLON IF condition THEN action
      {
        {
          name = $2;
          cond = $5;
          action = $7;
        }
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
      { TimeBetween ($3, $5) }

| TIME_KW EQ TIME
      { TimeEquals $3 }

(* ---------- ACTIONS ---------- *)

action:
  IDENT DOT IDENT
      {
        match $3 with
        | "on"  -> TurnOn $1
        | "off" -> TurnOff $1
        | _ -> failwith "Unknown action"
      }