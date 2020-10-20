(*********************************************************************)
(** Gian *)
(*********************************************************************)

(** This module contains all data relevant to a player and their state. 
    Throughout the course of the game of golf, the player will be moving around
    the course and their score will be changing, a player object will contain the
    information to track this. *)

(**The abstract type of values representing a player *)
type t

(* 
Abstraction Function: Some golf players are stronger or weaker than others. 
This is The type that represents a "power multiplier". It is assumed a
completely average player would have a multiplier of 1.0. A stronger player 
could have a mutiplier ranging from 1.0 up to 1.5 inclusive. A weaker player
could have a multiplier ranging from 1.0 down to 0.5 inclusive. In the game
itself, this multiplier will allow the player to hit the ball farther (for a 
stronger player) or less far (for a weaker player) given the same 
representation of power. 

Representation Invariant: The power_multiplier must be between 0.5 and 1.5 
inclusive. *)

type power_multiplier = float
(* 
Abstraction Function: Some golf players are more or less accurate than others
due to years of practice, a steady hand, or a good eye. This is The type that 
represents an "accuracy multiplier". It is assumed a
completely average player would have a multiplier of 1.0. A more accurate 
player could have a mutiplier ranging from 1.0 up to 1.5 inclusive. A less
accurate player could have a multiplier ranging from 1.0 down to 0.5 inclusive.
In the game itself, the accuracy multiplier will allow the player to reduce
(for a more accurate player) or increase (for a less accurate player) the 
amount their ball deviates from where they wanted it to go (reduce the noise 
from each swing due to random variation or wind)

Representation Invariant: The accuracy_multiplier must be between 0.5 and 1.5 
inclusive. *)
type accuracy_multiplier = float

(* 
The type that represents the handicap of a player. The interpretation is the
same as in normal golf. The value is subtracted from a player's final score at
the end of a match. This allows players of different skill levels to play
with each other.  *)
type handicap = int


(**  [init_players] produces an array of players based on input from stdin 
     Raises: Invalid_argument if input is not one of the accepted forms *)
val init_players : unit -> t array

(** [read_players j] produces the list of players enumerated in 
    json j which contains player information. 
    Requires: j is a valid json representation of a list of players*)
val read_players : Yojson.Basic.t -> t list

(** *)
val get_player_name : t -> string

val get_player_power_multiplier : t -> float

val get_player_accuracy_multiplier : t -> float

val get_player_handicap : t -> int 

