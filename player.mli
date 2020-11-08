open Command
(*********************************************************************)
(** Gian *)
(*********************************************************************)

(** This module contains all data relevant to a player and their state. 
    Throughout the course of the game of golf, the player will be moving around
    the course and their score will be changing, a player object will contain the
    information to track this. *)

(**The abstract type of values representing a player *)
type t


(*The type representing how much a player's strength effects their swing power*)
type power_multiplier = float

(*The type representing how much a player's skill effects their swing accuracy*)
type accuracy_multiplier = float

(*The type representing how many additional strokes a player gets subtracted to 
  their score based on their skill level*)
type handicap = int


(**  [init_players] produces an array of players based on input from stdin 
     Raises: Invalid_argument if input is not one of the accepted forms *)
val init_players : unit -> t array

(** [read_players j] produces an array of players based on input from a json *)
val read_players : Yojson.Basic.t -> t array

(** *)
val get_player_name : t -> string

val get_player_power_multiplier : t -> float

val get_player_accuracy_multiplier : t -> float

val get_player_handicap : t -> int 

val get_player_location : t -> float*float

val update_player_location: t -> float*float -> t

(* val get_distance: int*int -> int*int -> int *)

val dist_from_hole : float * float -> float * float -> float

val calculate_location: t -> Command.t -> Course.hole_number -> 
  Course.t -> Course.hole_location