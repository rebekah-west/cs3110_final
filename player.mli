(*********************************************************************)
(** This module contains all data relevant to a player and their state. 
    Throughout the course of the game of golf, the player will be moving around
    the course and their score will be changing, a player object will contain the
    information to track this. *)
(*********************************************************************)

open Command
open Parse

(**The abstract type of values representing a player *)
type t

(* type representing how much a player's strength effects their swing power *)
type power_multiplier = float

(* type representing how much a player's skill effects their swing accuracy *)
type accuracy_multiplier = float

(* The type representing how many additional strokes a player gets subtracted
   to their score based on their skill level *)
type handicap = int


(**  [init_players] produces an array of players based on input from stdin 
     Raises: Invalid_argument if input is not one of the accepted forms *)
val init_players : unit -> t array

(** [read_players j] produces an array of players based on input from a json *)
val read_players : Yojson.Basic.t -> t array

(** [get_player_name p] returns the name of a player [p] *)
val get_player_name : t -> string

(** [get_player_power_multiplier p] returns the power multiplier for a 
    player [p] *)
val get_player_power_multiplier : t -> float

(** [get_player_accuracy_multiplier p] returns the accuracy multiplier 
    for a player [p] *)
val get_player_accuracy_multiplier : t -> float

(** [get_player_handicap p] returns the handicap for a player [p] *)
val get_player_handicap : t -> int 

(** [get_player_location p] returns the location in cartesian coordinates of 
    the player [p] on the course. *)
val get_player_location : t -> float*float

(** [update_player_location p new_loc] returns a new instande of player [p] 
    containing infomration about a new location [new_loc] *)
val update_player_location: t -> float*float -> t

(* [dist_from_hole loc1 loc2] calculates the cartesian distance between 
   two sets of coordinates, [loc1] and [loc2] *)
val dist_from_hole : float * float -> float * float -> float

(* [calculate_location p comm hol_num cors] uses information contained within
   player [p] and their swing [comm] and calculates their new location based 
   on the hole [hole_num] and course [cors] they are on and returns their 
   updated location post swing. *)
val calculate_location: t -> Command.t -> Course.hole_number -> 
  Course.t -> Course.hole_location