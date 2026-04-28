type device = {
  name : string;
  location : string;
  state: bool;  
}

type sensor = {
  name : string;
  location : string;
  value : value;
}

type value = 
  | Int of int
  | Bool of bool

// Nu ved compileren ikke kun, at noget er en sensor, men også hvilken type sensor det er.
type sensor_type =
  | IntSensor
  | BoolSensor

type sensor = {
  name : string;
  location : string;
  sensor_type : sensor_type;
}


type time = int * int

type comparison_op =
  | Lt
  | Gt
  | Eq

type condition =
  | SensorCompare of string * comparison_op * int
  | SensorEqualsBool of string * bool
  | TimeBetween of time * time
  | TimeEquals of time
  | And of condition * condition

type action =
  | TurnOn of string
  | TurnOff of string

type rule = {
  name : string;
  condition : condition;
  action : action;
}

type program = {
  devices : device list;
  sensors : sensor list;
  rules : rule list;
}
