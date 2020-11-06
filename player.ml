open Yojson.Basic.Util
open Command
open Game
open Course 
open Str
open Parse
open Random

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
  location : (float * float);
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
    location = (0.,0.);
  }

let read_players j =
  let open Yojson.Basic.Util in 
  let players = 
    j |> member "Players" |> to_list 
    |> Array.of_list |>  Array.map player_from_json
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
    location = (0., 0.);
  } in p

let init_players () =
  Printf.printf "How many players will be participating today? Enter an int \n";
  let num_players = int_of_string (read_line ()) in
  let player_array = Array.make num_players 0. in
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

open Float

(* A helper that converts (int*int) to (float*float) *)
let float_of_int_tuple tup = 
  let ret = (tup |> fst |> float_of_int, tup |> snd |> float_of_int) in 
  ret 

(* let get_distance hole_loc player_loc = 
   let a = ((fst hole_loc - fst player_loc) * (fst hole_loc - fst player_loc)) + 
          ((snd hole_loc - snd player_loc) * (snd hole_loc - snd player_loc)) 
   in  1/ (a * a) *)

(* returns the distance from hole *)
let dist_from_hole hole_loc player_loc = 
  sqrt( pow (fst hole_loc -. fst player_loc) 2. 
        +. pow (snd hole_loc -. snd player_loc) 2. )

(*A helper to get the radian measure from degrees*)
let rad_from_deg degrees = 
  degrees /. 180.0 *. pi

(* AF: Converting a swing to a "speed" in meters per second for calculation with 
   gravity given a players power multiplier after personal and club adjustments
   and given the power they selected to use. We represent 100 power as hitting 
   a ball with an initial velocity of 50 m/s. This is because if a player sets 
   an "optimal" angle, given no multipliers this would lead to a 279 yard drive
   which we find to be a reasonable average. 
   Example: power_to_ms 30 converts to an initial velocity of 15 m/s*)
let power_to_ms adj_pow =
  adj_pow /. 2.0 

(* Use the Box-Muller transformation to generate a single standard 
   normal random variable 
   https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform *)
let random_gaussian () =
  sqrt (-2. *. log (Random.float 1.)) *. cos (2. *. pi *. Random.float 1.)

(* A helper that gets the initial direction a player is facing (towards
   the hole) before any adjustment. *)
let get_direction (p1 : float*float) (p2 : float*float) = 
  let x1 = fst p1 in
  let x2 = fst p2 in
  let y1 = snd p1 in 
  let y2 = snd p2 in 
  ( (y2 -. y1) /. (x2 -. x1) )|> atan 

(* TODO: make it take in only a game since it contains course*)
(*command gives a club, a power, an angle, and an alignment*)
let calculate_location t (swing : Command.t) (gam : Game.t) (cours : Course.t)= 
  let current_loc = t.location in
  let acc_mul = t.accuracy_multiplier in  
  let pow_mul = t.power_multiplier in
  let clb = get_club swing in
  let club_pow_adj = fst (get_club_adjustments clb) in
  let club_acc_adj = snd (get_club_adjustments clb) in
  let adj_pow = (swing |> get_power) *. pow_mul *. club_pow_adj in
  let adj_acc = acc_mul *. club_acc_adj in
  let chosen_ang = get_angle swing in 
  (* Adjust angle based on accuracy, the adjustment is a Normal Random 
     variable with mean 0 and standard deviation equal to the inverse of the
     accuracy multiplier. The adjustment is in degrees, it will never result in
     the angle being less than 0 or greater than 90.
     Example: Accuracy multiplier of .5 means 2 times as much of a standard 
     deviation as an Accuracy multiplier of 1. Accuracy multiplier of 1.25 
     means .8 times as much standard deviation.*)
  let adj_ang chosen_ang adj_acc = 
    let calced = chosen_ang *. random_gaussian() /. adj_acc in
    match calced < 0. with
    |true -> 0.
    |false -> begin 
        match calced > 90. with 
        |true -> 90.
        |false -> calced
      end in 
  let final_ang = adj_ang chosen_ang adj_acc in 
  let chosen_align = get_align swing in 
  (* Adjust alignment the same way except divide by 5 as alignment is much more 
     sensitive to change *)
  let adj_align chosen_align adj_acc = 
    let calced2 = chosen_align *. random_gaussian() /. adj_acc /. 5. in
    match calced2 < 90. with
    |false -> 90.
    |true -> begin 
        match calced2 > (-90.) with 
        |false -> (-90.)
        |true -> calced2
      end in 
  let final_align = adj_align chosen_align adj_acc in 
  let theta = rad_from_deg (final_ang) in 
  let init_velocity = power_to_ms adj_pow in
  let horiz_speed = (cos theta) *. init_velocity in
  let vert_speed = (sin theta) *. init_velocity in
  let time_in_air = ( vert_speed /. 9.8 ) *. 2.0 in
  let horiz_dist = time_in_air *. horiz_speed in
  let hol_loc = get_hole_loc cours (current_hole gam) in 
  let direction = get_direction current_loc hol_loc +. final_align in 
  let new_loc = 
    ( (direction |> rad_from_deg |> cos) *. horiz_dist +. fst current_loc ,  
      ( (direction |> rad_from_deg |> sin) *. horiz_dist +. snd current_loc) ) 
  in 
  if dist_from_hole hol_loc new_loc < 1.0 then 
    hol_loc else new_loc





