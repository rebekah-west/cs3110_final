
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

type scorecard = hole_score list list

type t = {
  roster: Player.t array;
  course: Course.t;
  current_hole: Course.hole_number;
  current_turn: Player.t;
  scores: scorecard;
  holes_played: hole_score list;
}

exception InvalidHole

(** [current_hole game] returns the hole currently being played *)
let current_hole game = game.current_hole

(** [init_hole_score player hole] initializes a 0 score for that player and
    that hole *)
let init_hole_score player hole = {
  hole = hole;
  player = player;
  hole_score = 0; }

(** [init_scorecard players hole] initializes a 0 score for every player
    on hole [hole] *)
let rec init_scorecard players hole = 
  failwith "Unimplemented"

(* [game_helper players holes] *)
let rec game_helper (players: Player.t array) (holes: Course.hole list) = 
  failwith "Unimplemented"

(** required: there must be at least one hole with*)
let init_game players (course: Course.t) = 
  failwith "Unimplemented"


let current_hole game = game.current_hole

let played game = game.holes_played

let current_turn game = game.current_turn

let current_score game = game.scores

let game_roster game = game.roster

let update_score game = 
  failwith "Unimplemented" 

let update_turn game = 
  failwith "Unimplemented" 

(* 1. if same hole: iterate over players to see who is furthest away from 
   hole  *)
(* if different hole, players should tee-off in order of who won last round *)
(* 2. make that player the current player  *)

(* gets just a specific hole from the holescore list of one player *)
let rec grab_hole_from_player (player:Player.t) (scores:hole_score list) hole= 
  failwith "Unimplemented"

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
  failwith "Unimplemented"

let winner_of_hole (game:t) hole = 
  failwith "Unimplemented"

(* gets the winning score of all players  *)
let rec winning_score roster (best:Player.t) = 
  failwith "Unimplemented"

(* returns winner or winners of game best on who has best overall 
   score at end of the game*)
let rec winners_roster roster sc = 
  failwith "Unimplemented"

(* returns a list of winners  *)
let winner_of_game game = 
  failwith "Unimplemented"
