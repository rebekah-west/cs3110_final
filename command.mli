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
    Requires: A float between 0 and 100
*)
type power

(** The type [angle] represents the vertical angle from the ground the ball 
    will travel 
    Requires: A float between 0 and 90
*)
type angle

(** The type [alignment] represents the number of degrees away from facing the 
    hole 
    Requires: A float between -90 and positive 90
    Note: 0 is classified as Right
*)
type alignment =
  | Left of float
  | Right of float

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

(* [parse_club club] evaluates the type of club a player wishes to use on their 
   turn represented by string [club]
   Requires: A valid input string which represents a club
   Raises: A Malformed exception if the string passed is not a valid
   representation of a club or an [Empty] exception if no argument is passed
*) 
val parse_club : string -> club

(* [parse_power power] evaluates the power a player wishes to use on their 
   turn represented by float [club]
   Requires: A valid input, a float
   Raises: An [InvalidInput] exception if the passed argument is not a number 
   between 0 and 100 or an [Empty] exception if no argument is passed
*)
val parse_power : float -> power

(* [parse_angle angle] evaluates the angle a player wishes to use on their 
   turn represented by float [power]
   Requires: A valid input, a float
   Raises: An [InvalidInput] exception if the passed argument is not a number 
   between 0 and 90 or an [Empty] exception if no argument is passed
*)
val parse_angle : float -> angle

(* [parse_alignment degrees] evaluates the angle a player wishes to use 
   to offset themselves from the initial positioning of being
   directly pointed at the hole. Takes in float [degrees] and outputs the
   corresponding alignment
   Requires: A valid input, a float between -90.0 and 90.0
   Raises: An [InvalidInput] exception if the passed argument is not a number 
   between -90.0 and 90.0 or an [Empty] exception if no argument is passed
*)
val parse_alignment : float -> alignment

(** [parse_input] prompts the users for various inputs (which club they want
    to use, how much power they want to swing with, the vertical angle, and the 
    alignment) and creates a type t for the program to use going forward 
    Examples:
    Requires:
    Raises:
*)
val parse_input : unit -> command

(* [get_command comm] is the command t contained within [comm] if 
   the result is legal and throws a "Not_Legal" exception otherwise.*)
val get_command : command -> t

