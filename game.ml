
open Course
open Player
(*****************************************************)
(* Implementations of game and it's functions*)
(*****************************************************)

(* only need to keep track of score per hole since Player.t
   keeps track of the overall score of a player*)
type hole_score = {
  hole: Course.hole_number;
  player: Player.t;
  hole_score: int;
}

type scorecard = hole_score array array

type t = {
  roster: Player.t array;
  course: Course.t;
  current_hole: Course.hole_number;
  current_turn: Player.t;
  scores: scorecard;
  holes_played: Course.hole_number list;
}

exception InvalidHole

(** [current_hole game] returns the hole currently being played *)
let current_hole game = game.current_hole

(** [init_hole_score player hole] initializes a 0 score for that player and
    that hole *)
let init_hole_score hole player = {
  hole = hole;
  player = player;
  hole_score = 0; }

(** [init_scorecard players hole] initializes a 0 score for every player
    on hole [hole] *)
let init_scorecard (players: Player.t array) hole = 
  Array.map (init_hole_score hole) players 

(* [create_scorecard players holes]  *)
let create_scorecard (players: Player.t array) (course: Course.t) = 
  let hole_array = (Array.map get_hole_number (get_holes course)) in
  Array.map (init_scorecard players) hole_array

(** required: there must be at least one hole with*)
let init_game (players: Player.t array ) (course: Course.t) = 
  let scores = create_scorecard players course in
  let current_hole = Course.start_hole course in
  let frst_up = players.(0) in 
  let first = {
    roster=players; 
    course=course;
    scores=scores; 
    current_hole=current_hole;
    current_turn= frst_up; 
    holes_played=[];
  } in first


let current_hole game = game.current_hole

let played game = game.holes_played

let current_turn game = game.current_turn

let current_score game = game.scores

let game_roster game = game.roster

(* will update the scorecard in game, NEEDS TESTING*)
let update_score game = 
  let sc = game.scores.(game.current_hole) in 
  let current_player = game.current_turn in 
  for i = 0 to Array.length sc do 
    if sc.(i).player == current_player
    then let new_hole_score = {
        hole = game.current_hole;
        player = current_player;
        hole_score = sc.(i).hole_score + 1;
      } in game.scores.(game.current_hole).(i) <- new_hole_score
  done





(* type hole_score = {
   hole: Course.hole_number;
   player: Player.t;
   hole_score: int;
   }

   type scorecard = hole_score array array

   type t = {
   roster: Player.t array;
   course: Course.t;
   current_hole: Course.hole_number;
   current_turn: Player.t;
   scores: scorecard;
   holes_played: Course.hole_number list;
   } *)

let update_turn game = 
  failwith "Unimplemented" 

(* 1. if same hole: iterate over players to see who is furthest away from 
   hole  *)
(* if different hole, players should tee-off in order of who won last round *)
(* 2. make that player the current player  *)

(* gets just a specific hole from the holescore list of one player *)
(* let rec grab_hole_from_player (player:Player.t) (scores:hole_score list) 
    (hole: Course.hole_number) = 

   (* get score for one player at one hole from scorecard *)
   let rec player_score (player:Player.t) scorecard hole= 
   failwith "Unimplemented"

   (* get the hole_scores for all players at a specific hole  *)
   let rec scores_for_hole players scorecard hole = 
   failwith "Unimplemented"

   (* get the integer score of the best score for a specific hole  *)
   let rec top_score_of_hole (hole_list: hole_score list) max = 
   failwith "Unimplemented"

   (* returns a list of the winners of the hole given the best score  *)
   let rec get_winners score_list best_score = 
   failwith "Unimplemented" *)

let winner_of_hole (game:t) (hole:Course.hole_number) = 
  let scorecard = game.scores.(hole) in 
  let lowest_score = scorecard.(0) in 
  for x=0 to 3 do
    failwith "unimplemented"
  done

(* gets the winning score of all players 
   let rec winning_score roster (best:Player.t) = 
   failwith "Unimplemented"

   (* returns winner or winners of game best on who has best overall 
   score at end of the game*)
   let rec winners_roster roster sc = 
   failwith "Unimplemented" *)


(* returns a list of winners  *)
let winner_of_game game = 
  failwith "Unimplemented"

let play_hole game =
  failwith "unimplemented"

let print_scorecard game = failwith "Unimplemented"

