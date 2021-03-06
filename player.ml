open Yojson.Basic.Util
open Command
open Course 
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
}

let pp_player pl = 
  let name = pl.player_name in
  let power = string_of_float pl.power_multiplier in 
  let acc = string_of_float pl.accuracy_multiplier in 
  let handicap = string_of_int pl.handicap in 
  let loc = Parse.pp_tup pl.location in  
  "{ \nName: " ^ name ^ "\nPower: " ^ power ^ "\nAccuracy: " ^ acc 
  ^ "\nHandicap: " ^ handicap ^ "\nLocation: " ^ loc ^ "\n}"

(** [player_from_json j] reads in the player from the json *)
let player_from_json j =
  let open Yojson.Basic.Util in {
    player_name = j |> member "Name" |> to_string;
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
   Raises: None *)
let rec parse_acc_mult (acc : string) = 
  match acc with 
  | "beginner" -> 0.5
  | "intermediate" -> 1.0
  | "advanced" -> 1.5
  | _ -> let bia_message = "You must enter beginner, intermediate, or advanced, please check your spelling and try again. \n" in
    (for_string_output bia_message) |> parse_acc_mult

(* [parse_pow_mult str] returns the power multiplier based on how strong
   someone indicates they are
   Raises: Invalid_argument if the string is not one of the suggested 
   strengths *)
let rec parse_pow_mult (pow : string)= 
  match pow with 
  | "belowaverage" -> 0.5
  | "average" -> 1.0
  | "aboveaverage" -> 1.5
  | _ -> let pow_message =  "You must enter below average, average, or above average,please check your spelling and try again. \n" in 
    (for_string_output pow_message) |> parse_pow_mult

let rec parse_name (name : string) = 
  let parsed_name = parse name in
  if parsed_name == "help" 
  then begin
    Printf.printf "\n Please enter your name.\n";
    let new_name = read_line () in
    parse_name new_name;
  end
  else parsed_name

(*A recursive helper that either parses an int [pow] to it's corresponding 
  power or asks the user to give another power along with a helpful message
  if the value was outside the acceptable range*)
let rec parse_handicap (handicap: int)=
  if (handicap >= ~-9 && handicap <= 9) 
  then handicap
  else 
    let hand_message = "Your handicap must be an integer between -9 and 9. > \n" 
    in parse_handicap(for_int_output hand_message)

(* [create_player entry] prompts user for input, parses it, and 
   returns type Player.t *)
let create_player entry =
  let name_message = "\nWelcome new player. Please enter your name.\n" in 
  let name = (for_string_output name_message) |> parse_name in
  let level_message = "For golf, are you beginner, intermediate, or advanced?\n" in
  let acc_mult =  parse_acc_mult (for_string_output level_message) in
  let strength_message =  "How strong are you? (below average, average, above average)\n" in 
  let pow_mult = parse_pow_mult (for_string_output strength_message) in
  let handicap_message = "If you would like a handicap, enter it here as an integer between -9 and 9. Otherwise enter 0.\n"; in 
  let handicap = for_int_output handicap_message |> parse_handicap in
  Printf.printf "Thank you %s. We hope you enjoy the game.\n" (String. capitalize_ascii name);
  { 
    player_name = name; 
    power_multiplier = pow_mult; 
    accuracy_multiplier = acc_mult;
    handicap = handicap;
    location = (0., 0.);
  }

let init_players () =
  let message = "How many players will be participating today? Enter an int \n"; in 
  let num_players = Parse.for_int_output message in 
  let player_array = Array.make num_players 0. in
  Array.map create_player player_array

let get_player_name t = t.player_name

let get_player_power_multiplier t = t.power_multiplier

let get_player_accuracy_multiplier t = t.accuracy_multiplier

let get_player_handicap t = t.handicap

let get_player_location t = t.location

let update_player_location p new_loc= 
  { player_name = p.player_name; 
    power_multiplier = p.power_multiplier; 
    accuracy_multiplier = p.accuracy_multiplier ;
    handicap = p.handicap;
    location = new_loc;} 

open Float

(* A helper that converts (int*int) to (float*float) *)
let float_of_int_tuple tup = 
  let ret = (tup |> fst |> float_of_int, tup |> snd |> float_of_int) in 
  ret 

(* returns the distance from hole *)
let dist_from_hole hole_loc player_loc = 
  sqrt( pow (fst hole_loc -. fst player_loc) 2. 
        +. pow (snd hole_loc -. snd player_loc) 2. )

(*A helper to get the radian measure from degrees*)
let rad_from_deg degrees = 
  degrees /. 180.0 *. pi

let deg_from_rad radians = 
  radians *. 180.0 /. pi

(* AF: Converting a swing to a "speed" in meters per second for calculation 
   with gravity given a players power multiplier after personal and club 
   adjustments and given the power they selected to use. We represent 100 
   power as hitting a ball with an initial velocity of 50 m/s. This is 
   because if a player sets an "optimal" angle, given no multipliers this 
   would lead to a 279 yard drive which we find to be a reasonable average. 
   Example: power_to_ms 30 converts to an initial velocity of 15 m/s*)
let power_to_ms adj_pow =
  adj_pow /. 2.0 

(* Use the Box-Muller transformation to generate a single standard 
   normal random variable 
   https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform *)
let random_gaussian () =
  sqrt (-2. *. log (Random.float 1.)) *. cos (2. *. pi *. Random.float 1.)

(* Adjust angle based on a player's accuracy multiplier [adj_acc] and their
   chosen angle to hit the ball [chosen_ang], the adjustment is a Normal Random
   variable with mean 0 and standard deviation equal to the inverse of the
   accuracy multiplier. The adjustment is in degrees, it will never result in
   the angle being less than 0 or greater than 90.
   Example: Accuracy multiplier of .5 means 2 times as much of a standard 
   deviation as an Accuracy multiplier of 1. Accuracy multiplier of 1.25 
   means .8 times as much standard deviation.*)
let adj_ang chosen_ang adj_acc = 
  let calced = chosen_ang +. random_gaussian() /. adj_acc in
  match calced < 0. with
  |true -> 0.
  |false -> begin 
      match calced > 90. with 
      |true -> 90.
      |false -> calced
    end 

(* Adjust alignment takes in the player's chosen alisgnment [chosen_align] and 
   the calculcated accuracy multiplier from player information [adj_acc] and 
   returnsthe the final adjusted alignment in degrees *)
let adj_align chosen_align adj_acc = 
  let calced2 = chosen_align +. random_gaussian() /. adj_acc /. 5. in
  match calced2 < 90. with
  |false -> 90.
  |true -> begin 
      match calced2 > (-90.) with 
      |false -> (-90.)
      |true -> calced2
    end

(* A helper that gets the initial direction a player is facing (towards
   the hole) before any adjustment. p1 is the player location and 
   p2 is the hole location *)
let get_direction (p1 : float*float) (p2 : float*float) = 
  let x1 = fst p1 in
  let x2 = fst p2 in
  let y1 = snd p1 in 
  let y2 = snd p2 in 
  let degs = ( (y2 -. y1) /. (x2 -. x1) )|> atan |> deg_from_rad in 
  if x2 > x1 && y2 < y1 then 360. +. degs else
  if x2 < x1 then 180. +. degs
  else
    degs


let m_to_yd (meters : float) = (39.3701/. 36.) *. meters 

let float_abs flt = 
  if flt > 0.0 then flt else (flt *. -1.0)

let float_mod flt mod_num = 
  ((flt /. mod_num) -. 1.) *. flt

let rec bound_loc (pos : float * float)=
  match pos with 
  | (x , y) when x <= 500. && y <= 500. && x >= 0. && y >= 0. -> pos
  | (x , y) when x > 500. -> bound_loc (500., y)
  | (x , y) when y > 500. -> bound_loc (x , 500.)
  | (x , y) when x < 0. -> bound_loc (0.,y)
  | (x , y) when y < 0. -> bound_loc (x, 0.)
  | _ -> failwith "unknown location"

let obs_center obstacle = 
  match obstacle with 
  |x,y,_,_ -> (x,y)

let captured obstacle loc = 
  let center = obs_center obstacle in
  let distance =   sqrt( pow (fst loc -. fst center) 2. 
                         +. pow (snd loc -. snd center) 2. ) in 
  match obstacle with 
  | _,_,"l","s" -> if distance < 10. then (0.,0.,"l") else (0.,0.,"n")
  | _,_,"l","m" -> if distance < 25. then (0.,0.,"l") else (0.,0.,"n")
  | _,_,"l","l" -> if distance < 80. then (0.,0.,"l") else (0.,0.,"n")
  | _,_,"t","s" -> if distance < 5. then 
      (Random.float(50.),Random.float(50.),"t") else (0.,0.,"n")
  | _,_,"t","m" -> if distance < 10. 
    then (Random.float(50.),Random.float(50.),"t") else (0.,0.,"n")
  | _,_,"t","l" -> if distance < 15. 
    then (Random.float(50.),Random.float(50.),"t") else (0.,0.,"n")
  | _,_,"s","s" -> if distance < 8. 
    then (Random.float(20.),Random.float(20.),"s") else (0.,0.,"n")
  | _,_,"s","m" -> if distance < 20. 
    then (Random.float(20.),Random.float(20.),"s") else (0.,0.,"n")
  | _,_,"s","l" -> if distance < 40. 
    then (Random.float(20.),Random.float(20.),"s") else (0.,0.,"n")
  | _ -> failwith "impossible"

let rec go_through_obstacles init_loc obstacles = 
  match obstacles with
  | [] -> (0.,0.,"n")
  | h :: t -> begin
      let obs_result = captured h init_loc in 
      match obs_result with 
      | (_,_,"l")  -> obs_result
      | ( _,_,"t")  -> obs_result
      |(_,_,"s") -> obs_result
      | (_,_,"n") -> go_through_obstacles init_loc t
      |_ -> failwith "impossible"
    end

(*returns the final adjustment in terms of x, y, and the type of terrain hit*)
let obstacle_check init_loc cours hol_num = 
  let obs = get_obstacle_locs cours hol_num in
  let final_adj = go_through_obstacles init_loc obs in final_adj

let update_loc direction horiz_dist_yd current_loc cours hol_num= 
  let before_obstacle = ( (direction |> rad_from_deg |> cos) *. horiz_dist_yd 
                          +. fst current_loc ,  
                          ( (direction |> rad_from_deg |> sin) *. horiz_dist_yd 
                            +. snd current_loc) ) in 
  let obs_adjustment = obstacle_check before_obstacle cours hol_num in 
  match obs_adjustment with 
  | _,_,"l" -> print_string "You landed in a lake, you return to your previous position\n";
    current_loc
  | _,_,"n" -> print_string "You hit no obstacles \n"; before_obstacle
  | x,y,"s" -> print_string "You landed in some sand\n";
    (fst before_obstacle +. x , snd before_obstacle +. y)
  | x,y,"t" -> print_string "You bounced off a tree\n";
    (fst before_obstacle +. x , snd before_obstacle +. y)
  | _ -> failwith "impossible"

(* Calculate the final location on the ball based on the current location 
   of a player and whether it was a normal swing or a putt *)
let get_final_loc current_loc hol_loc chosen_ang adj_pow dir 
    horiz_dist_yd cours hol_num= 
  (*the case of rolling*)
  if chosen_ang = 0. then 
    let horiz_dist_yd = m_to_yd (adj_pow /. 2.) in 
    let upd_loc = update_loc dir horiz_dist_yd current_loc cours hol_num in 
    let new_loc = bound_loc upd_loc in 
    if dist_from_hole hol_loc new_loc < 30.0 then 
      hol_loc else new_loc
  else
    let upd_loc = update_loc dir horiz_dist_yd current_loc cours hol_num in 
    let new_loc = bound_loc upd_loc in 
    if dist_from_hole hol_loc new_loc < 30.0 then 
      hol_loc else new_loc

(* Calculate the horizontal distance travelled in the cartesian plane in 
   yards based on an initial velocity of the ball and and angle of direciton 
   theta *)
let horizontal_distance_yd theta init_velocity = 
  let horiz_speed = (cos theta) *. init_velocity in
  let vert_speed = (sin theta) *. init_velocity in
  let time_in_air = ( vert_speed /. 9.8 ) *. 2.0 in
  let horiz_dist = time_in_air *. horiz_speed in
  let horiz_dist_yd = m_to_yd horiz_dist in 
  horiz_dist_yd

let calculate_location t (swing : Command.t)( hol_num : Course.hole_number)
    (cours : Course.t)= 
  let current_loc = t.location in
  let acc_mul = t.accuracy_multiplier in  
  let pow_mul = t.power_multiplier in
  let clb = get_club swing in
  let club_pow_adj = fst (get_club_adjustments clb) in
  let club_acc_adj = snd (get_club_adjustments clb) in
  let adj_pow = (swing |> get_power) *. pow_mul *. club_pow_adj in
  let adj_acc = acc_mul *. club_acc_adj in
  let chosen_ang = get_angle swing in 
  let final_ang = adj_ang chosen_ang adj_acc in 
  let chosen_align = get_align swing in 
  let final_align = adj_align chosen_align adj_acc in 
  let theta = rad_from_deg (final_ang) in 
  let init_velocity = power_to_ms adj_pow in
  let horiz_dist_yd = horizontal_distance_yd theta init_velocity in 
  let hol_loc = get_hole_loc cours hol_num in 
  let direction = get_direction current_loc hol_loc -. final_align in 
  get_final_loc current_loc hol_loc chosen_ang adj_pow direction horiz_dist_yd cours hol_num


