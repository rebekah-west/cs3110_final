(*********************************************************************)
(** How much should we place in .mli versus .ml ? *)
(** When a function takes user input in the body, should it take type unit? *)
(*********************************************************************)


(**
   Parsing of player commands.
*)

(** The type [t] contains all the information relevant to a swing *)
type t 

(** The type [club] represents the types of clubs a player can use. *)
type club = 
  | Driver
  | NineIron
  | EightIron
  | Putter
  | PitchingWedge
  | SandWedge

(** The type [power] represents how hard the golf ball is hit. 
    Requires: Must be an int between 0 and 100
*)
type power = int

(** The type [angle] represents the vertical angle from the ground the ball 
    will travel 
    Requires: Must be an ine between 0 and 90
*)
type angle = int

(** The type [alignment] represents the offset the player takes from being
    pointed directly at the hole. Negative alignment implies turning left of 
    the hole and positive alignment implies aiming to the right
    Requires: Must be an int between -90 and 90 
*)
type alignment = int


(** The type [command] *)
type command =
  | Swing of t
  | None

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(**Raised when a user gives an invalid input *)
exception InvalidInput

(**Raised when a user gives an input outside of the range of 
   acceptable values*)
exception ValueOutOfRange


(* [parse_club clb] evaluates the type of club a player wishes to use on their 
   turn represented by string [clb]
   Requires: A valid input string which represents a club
   Raises: A Malformed exception if the string passed is not a valid
   representation of a club or an [Empty] exception if no argument is passed
*) 
val parse_club : string -> club

(* [parse_power pow] evaluates the power a player wishes to use on their 
   turn represented by int [pow]
   Requires: A valid input, an int between 0 and 100
*)
val parse_power : int -> power

(* [parse_angle ang] evaluates the angle a player wishes to use on their 
   turn represented by int [ang]
   Requires: A valid input, an int betwwen 0 and 90
*)
val parse_angle : int -> angle

(* [parse_alignment degrees] evaluates the angle a player wishes to use 
   to offset themselves from the initial positioning of being
   directly pointed at the hole. Takes in an int [degrees] and outputs the
   corresponding alignment
   Requires: A valid input, an int between -90 and 90
*)
val parse_alignment : int -> alignment

(** [parse_input] prompts the users for various inputs (which club they want
    to use, how much power they want to swing with, the vertical angle, and the 
    alignment) and creates a type t for the program to use going forward 
    Examples:
    Requires:
    Raises:
*)
val parse_swing : unit -> t

(* [get_command comm] is the command t contained within [comm] if 
   the result is legal and throws a "Not_Legal" exception otherwise.*)
val get_command : command -> t

val get_power : t -> float

val get_angle : t -> float 

val get_club : t -> club

val get_align : t -> float

val get_club_adjustments : club -> (float * float)