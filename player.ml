open Yojson.Basic.Util
open Command
include Command
open Game
open Str
open Parse
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

type t = {
  player_name : string;
  power_multiplier : float;
  accuracy_multiplier : float;
  handicap : int ; 
  location : (int * int);
  overall_score : int;
  (*may be helpful based on everyone else's implementations *)
  (* current_hole : int or string ; <- to get hole location of a player
     hole_strokes : int or int list; <- to get score at every hole*)
}

(** [player_from_json j] reads in the player from the json *)
let player_from_json j =
  let open Yojson.Basic.Util in {
    player_name = j |> member "name" |> to_string;
    power_multiplier = j |> member "power_multiplier" |> to_float;
    accuracy_multiplier = j |> member "accuracy_multiplier" |> to_float;
    handicap = j |> member "handicap" |> to_int;
    location = (0,0);
    overall_score = 0;
  }

let read_players j =
  let open Yojson.Basic.Util in 
  let players =  
    j |> member "Players" |> to_list 
    |> Array.of_list |> Array.map player_from_json
  in players


(* [parse_acc_mult str] returns the accuracy multiplier based on how comfortable
   someone indicates they are with golf
   Raises: None
*)
let rec parse_acc_mult (acc : string) = 
  let parsed = parse acc in 
  match parsed with 
  | "beginner" -> 0.5
  | "intermediate" -> 1.0
  | "advanced" -> 1.5
  | _ -> Printf.printf "You must enter beginner, intermediate, or advanced, please check your spelling and try again. \n"; 
    parse_acc_mult(read_line())

(* [parse_pow_mult str] returns the power multiplier based on how strong
   someone indicates they are
   Raises: Invalid_argument if the string is not one of the suggested strengths*)
let rec parse_pow_mult (pow : string) = 
  let parsed = parse pow in 
  match parsed with 
  | "below average" -> 0.5
  | "average" -> 1.0
  | "above average" -> 1.5
  | _ -> Printf.printf "You must enter below average, average, or above average, please check your spelling and try again. \n"; 
    parse_pow_mult(read_line())

(* [create_player entry] prompts user for input, parses it, and returns type Player.t *)
let create_player entry =
  Printf.printf "\nWelcome new player. Please enter your name.\n";
  let name = read_line () in
  Printf.printf "For golf, are you beginner, intermediate, or advanced?\n";
  let acc_mult = parse_acc_mult (read_line ()) in
  Printf.printf "How strong are you? (below average, average, above average)\n";
  let pow_mult = parse_pow_mult (read_line ()) in
  Printf.printf "If you would like a handicap, enter it here as an integer. Otherwise enter 0.\n";
  let handicap = int_of_string (read_line ()) in
  Printf.printf "Thank you %s. We hope you enjoy the game.\n" name;
  let p = {
    player_name = name; 
    power_multiplier = pow_mult; 
    accuracy_multiplier = acc_mult;
    handicap = handicap;
    location = (0, 0);
    overall_score = 0;
  } in p

let init_players () =
  Printf.printf "How many players will be participating today? Enter an int \n";
  let num_players = int_of_string (read_line ()) in
  let player_array = Array.make num_players 0 in
  Array.map create_player player_array


(*
-needs to communicate with command about where the ball lands
-we need to assume the player is just a point in space
-we assume they are on their ball or a foot to the left or something
*)


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

let get_player_score t = t.overall_score

(*command gives a club, a power, an angle, and an alignment*)
let calculate_location t (swing : Command.t) = 
  let current_loc = t.location in
  let acc_mul = t.accuracy_multiplier in  
  let pow_mul = t.power_multiplier in
  let clb = get_club swing in
  let club_pow_adj = fst (get_club_adjustments clb) in
  let club_acc_adj = snd (get_club_adjustments clb) in
  let adj_pow = (swing |> get_power) *. pow_mul *. club_pow_adj in
  let adj_acc = acc_mul *. club_acc_adj in
  (*find horizontal and vertical speed, time in air from vertical, location 
    from horizontal and alignment*)
  failwith "fail" 