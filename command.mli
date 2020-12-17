(*********************************************************************)
(*Parsing of player commands.*)
(*********************************************************************)

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
  | ThreeWood
  | FourHybrid
  | FiveHybrid
  | SixIron
  | SevenIron

(** The type [power] represents how hard the golf ball is hit. 
    Requires: Must be an int between 0 and 100 *)
type power = int

(** The type [angle] represents the vertical angle from the ground the ball 
    will travel 
    Requires: Must be an int between 0 and 90 *)
type angle = int

(** The type [alignment] represents the offset the player takes from being
    pointed directly at the hole. Negative alignment implies turning left of 
    the hole and positive alignment implies aiming to the right
    Requires: Must be an int between -90 and 90  *)
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
   acceptable values *)
exception ValueOutOfRange


(* [parse_club clb] evaluates the type of club a player wishes to use on their 
   turn represented by string [clb]
   Requires: A valid input string which represents a club *) 
val parse_club : string -> club

(* [parse_power pow] evaluates the power a player wishes to use on their 
   turn represented by int [pow]
   Requires: A valid input, an int between 0 and 100 *)
val parse_power : int -> power

(* [parse_angle ang] evaluates the angle a player wishes to use on their 
   turn represented by int [ang]
   Requires: A valid input, an int betwwen 0 and 90 *)
val parse_angle : int -> angle

(* [parse_alignment degrees] evaluates the angle a player wishes to use 
   to offset themselves from the initial positioning of being
   directly pointed at the hole. Takes in an int [degrees] and outputs the
   corresponding alignment
   Requires: A valid input, an int between -90 and 90 *)
val parse_alignment : int -> alignment

(** [parse_swing] prompts the users for various inputs (which club they want
    to use, how much power they want to swing with, the vertical angle, and the 
    alignment) and creates a type t for the program to use going forward 
    Examples: sequential user input of driver, 90, 45, 0 results in 
    {club: driver; power: 90; angle: 45; alignment: 0;}
    Requires: A valid input for each of club, power, angle, and alignment as 
    described above.
    If any input is invalid, the user will be prompted to try again. *)
val parse_swing : unit -> t

(** [get_power t] returns the power associated with swing [t] *)
val get_power : t -> float

(** [get_angle t] returns the angle associated with swing [t] *)
val get_angle : t -> float 

(** [get_club t] returns the club associated with swing [t] *)
val get_club : t -> club

(** [get_align t] returns the alignment associated with swing [t] *)
val get_align : t -> float

(* [get_club_adjustments clb] returns the adjustment tuple 
   (power adjustment, accuracy adjustment) based on the input club [clb] *)
val get_club_adjustments : club -> (float * float)