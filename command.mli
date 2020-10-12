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
  | Putter

(** The type [power] represents how hard the golf ball is hit. *)
type power

(** The type [angle] represents the vertical angle from the ground the ball 
  will travel *)
type angle

(** The type [alignment] represents the number of degrees away from facing the hole *)
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


val parse_club : string -> club
val parse_power : float -> power
val parse_angle : float -> angle

(** [parse_input] prompts the users for various inputs (which club they want
    to use, how much power they want to swing with, the vertical angle, and the 
    alignment) and creates a type t for the program to use going forward 
    Examples:
    Requires:
    Raises:
    *)
val parse_input : unit -> t

