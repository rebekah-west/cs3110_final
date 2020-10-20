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


(** [read_players j] produces the list of players enumerated in 
    json j which contains player information. 
    Requires: j is a valid json representation of a list of players*)
val read_players : Yojson.Basic.t -> t list

(** *)
val get_player_name : t -> string

val get_player_power_multiplier : t -> float

val get_player_accuracy_multiplier : t -> float

val get_player_handicap : t -> int 

