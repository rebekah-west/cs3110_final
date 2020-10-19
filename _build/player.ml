open Yojson.Basic.Util
(*****************************************************)
(* Implementations of player and it's functions*)
(*****************************************************)

type handicap = int
type accuracy_multiplier = float
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