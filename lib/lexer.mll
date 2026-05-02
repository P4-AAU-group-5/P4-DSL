(*
  | "rule"             { RULE }
  | "function"         { FUNCTION }
  | "if"               { IF }
  | "else"             { ELSE }
  | "then"             { THEN }
  | "while"            { WHILE }
  | "do"               { DO }
  | "in"               { IN }
  | "and"              { AND }
  | "or"               { OR }
  | "not"              { NOT }
  | "true"             { TRUE }
  | "false"            { FALSE }
  | "missing"          { NONE }
  | "device"           { DEVICE }
  | "sensor"           { SENSOR }
  | "group"            { GROUP }
  | "state"            { STATE }
  | ":"                { COLON }  
  
  (* Operators *)
  | '+'                { PLUS }
  | '-'                { MINUS }
  | '*'                { MULTI }
  | '/'                { DIVIDE }
  | "is"               { EQUAL }
  | "equals"           { BEQ }
  | "lessThan"         { BLT }
  | "lessOrEquals"     { BLE }
  | "greaterThan"      { BGT }
  | "greaterOrEquals"  { BGE }

  (* Delimiters *)
  | '('                { LP }
  | ')'                { RP }

*)
  

{
open Parser
}

let digit = ['0'-'9']
let int = digit+
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let time = digit digit ':' digit digit
let whitespace = [' ' '\t' '\r']+

rule read = parse
  | "\r\n" 
    { Lexing.new_line lexbuf; NEWLINE }

  | '\n'   
    { Lexing.new_line lexbuf; NEWLINE }
    
  | whitespace         { read lexbuf }

  | "device"           { DEVICE }
  | "sensor"           { SENSOR }
  | "rule"             { RULE }
  | "if"               { IF }
  | "then"             { THEN }
  | "and"              { AND }
  | "in"               { IN }
  | "between"          { BETWEEN }
  | "time"             { TIME_KW }

  | "true"             { TRUE }
  | "false"            { FALSE }

  | "<"                { LT }
  | ">"                { GT }
  | "=="               { EQ }

  | ".."               { RANGE }
  | "."                { DOT }
  | ":"                { COLON }

  | time as t          { TIME t }
  | int as i           { INT (int_of_string i) }
  | id as s            { IDENT s }

  | eof                { EOF }
  | _ as c { failwith ("Unexpected character: " ^ String.make 1 c) }
