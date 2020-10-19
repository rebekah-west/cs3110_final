(*****************************************************)
(* Implementations of command and it's functions*)
(*****************************************************)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(**Raised when a user gives an invalid input *)
exception InvalidInput

(**Raised when a user gives an input outside of the range of 
   acceptable values*)
exception ValueOutOfRange

type club = 
  | Driver
  | NineIron
  | EightIron
  | Putter
  | PitchingWedge
  | SandWedge

type power = float

type angle = float

type alignment =
  | Left of float
  | Right of float

type t = {
  club : club;
  power : power;
  angle : angle;
  alignment : alignment;
}

type command =
  | Swing of t
  | None

let parse_club (club : string) = 
  failwith "Unimplemented"

let parse_power (power : float)=
  failwith "Unimplemented"

let parse_angle (angle : float) = 
  failwith "Unimplemented"

let parse_alignment (degrees : float) =
  failwith "Unimplemented"

let parse_input () =
  failwith "Unimplemented"

let get_command (comm : command) = 
  failwith "Unimplemented"