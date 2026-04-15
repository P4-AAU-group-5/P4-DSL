type device = {
  name : string;
  location : string;
}

type sensor = {
  name : string;
  location : string;
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