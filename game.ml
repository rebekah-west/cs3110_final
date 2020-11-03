
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

   (* returns a list of the winners of the hole given the best score  *)
   let rec get_winners score_list best_score = 
   failwith "Unimplemented" *)

(* get the integer score of the best score for a specific hole  *)
let winning_score game hole = 
  let scorecard = game.scores.(hole) in 
  let lowest_score = scorecard.(0).hole_score in 
  let winner_array = Array.of_list [lowest_score] in 
  for i = 0 to Array.length scorecard do 
    let current_scorecard = scorecard.(i) in 
    if current_scorecard.hole_score < lowest_score then 
      let lowest_score = current_scorecard.hole_score
      in winner_array.(0) <- lowest_score 
    else 
      winner_array.(0) <- lowest_score
  done;
  winner_array.(0)

let rec winner_add winner_array scorecard low = 
  if (Array.length scorecard) = 0 then winner_array 
  else 
    let current_scorecard = scorecard.(0) in 
    if current_scorecard.hole_score = low then
      let add = Array.of_list ([current_scorecard.player]) in 
      let winners = Array.append winner_array add
      in let subs = Array.sub scorecard 1 (Array.length scorecard)
      in winner_add winners subs low
    else 
      let subs = Array.sub scorecard 1 (Array.length scorecard)
      in winner_add winner_array subs low

let winner_of_hole game hole = 
  let lowest_score= winning_score game hole in 
  let winner_array = Array.of_list [] in 
  let scorecard = game.scores.(hole) in 
  winner_add winner_array scorecard lowest_score

(* gets the winning score of all players 
   let rec winning_score roster (best:Player.t) = 
   failwith "Unimplemented" *)

let sum_scores game player = 
  let sc = game.scores in 
  let sums = Array.of_list [0] in 
  for i = 0 to Array.length sc do 
    let cur_sc = sc.(i) in 
    if sc.(i).player = player 
let update_sc = sums.(0) + sc.(i).hole_score
in sums.(0) <- update_sc 
else sums.(0) <- sum.(0) 
done;
sums.(0)

let winning_score_game game = 
  (* let roster = game.roster in 
     let lowest_score = roster.(0).total in 
     roster *)
  failwith "Unimplemented"

(* returns winner or winners of game best on who has best overall 
   score at end of the game*)
let rec winners_roster roster sc = 
  failwith "Unimplemented" 

(* returns a list of winners  *)
let winner_of_game game = 
  failwith "Unimplemented"

let play_hole game =
  failwith "unimplemented"

let print_scorecard game = failwith "Unimplemented"

