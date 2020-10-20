open Yojson.Basic.Util
(*****************************************************)
(* Implementations of player and it's functions*)
(*****************************************************)

(* 
The type that represents the handicap of a player. The interpretation is the
same as in normal golf. The value is subtracted from a player's final score at
the end of a match. This allows players of different skill levels to play
with each other.  *)
type handicap = int

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

type t ={
  player_name : string;
  power_multiplier : float;
  accuracy_multiplier : float;
  handicap : int ; 
  location : (float * float);
  overall_score : int;
  (*may be helpful based on everyone else's implementations *)
  (* current_hole : int or string ; <- to get hole location of a player
     hole_strokes : int or int list; <- to get score at every hole*)
}


let read_players j = 
  failwith "Unimplemented"

(*
-needs to communicate with command about where the ball lands
-we need to assume the player is just a point in space
-we assume they are on their ball or a foot to the left or something
*)
let update_location player = 
  failwith "Unimplemented"

(*perhaps uses a hole score tracked separately from overall score to help
  with a hole-by-hole scorecard?)
  let update_score player =
  failwith "Unimplemented" *)

let get_player_name t = 
  t.player_name

let get_player_power_multiplier t = 
  t.power_multiplier

let get_player_accuracy_multiplier t = 
  t.accuracy_multiplier

let get_player_handicap t = 
  t.handicap

let get_player_location t = 
  t.location