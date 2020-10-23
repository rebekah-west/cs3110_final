open Course
open Player
<<<<<<< HEAD

=======
>>>>>>> cdac30ed597d5ab6614bd02a20386ce5f381cf10
(*****************************************************)
(* Implementations of game and it's functions*)
(*****************************************************)
open Course
open Player

(* only need to keep track of score per hole since Player.t
   keeps track of the overall score of a player*)
type hole_score = {
  hole: Course.hole_number;
  player: Player.t;
  hole_score: int;
}

type scorecard = hole_score list list

type t = {
  roster: Player.t list;
  course: Course.t;
  current_hole: Course.hole_number;
  current_turn: Player.t;
  scores: scorecard;
  holes_played: hole_score list;
}

exception InvalidHole


let current_hole game = game.current_hole

let init_hole_score player (hole: Course.hole_number) = 
  {hole = hole; player=player; hole_score=0}


let rec init_scorecard players hole = 
  match players with 
  | [] -> []
  | p1::rest_of_players -> begin
      let p1_score = init_hole_score p1 hole in
      p1_score::init_scorecard rest_of_players hole
    end

let rec game_helper (players: Player.t list) holes = 
  match holes with 
  | [] -> []
  | h1::rest_of_holes -> begin 
      (* also need to be shown type hole  *)
      let hole_scores = init_scorecard players h1.hole_number in 
      hole_scores::game_helper players rest_of_holes
    end

(** required: there must be at least one hole with*)
let init_game players (course: Course.t) = 
  (* need a getter to get the course holes here  *)
  let scores = game_helper players course.holes in
  let current_hole = Course.start_hole course in
  let frst_up = List.hd players in 
  let game = {roster=players;
              scores=scores; 
              current_hole=current_hole;
              current_turn= frst_up; 
              holes_played=[];
             } 
  in game

(* getter for current hole *)
let current_hole game = game.current_hole

(* getter for holes already played *)
let played game = game.holes_played

(* getter for the current turn *)
let current_turn game = game.current_turn

let current_score game = game.scores

let game_roster game = game.roster

let update_score game = failwith "Unimplemented" 

let update_turn game = 
  failwith "Unimplemented" 

(* 1. if same hole: iterate over players to see who is furthest away from hole  *)
(* if different hole, players should tee-off in order of who won last round *)
(* 2. make that player the current player  *)

(* gets just a specific hole from the holescore list of one player *)
let rec grab_hole_from_player player (scores:hole_score list) hole= 
  begin 
    let is_hole hl = hl.hole == hole in 
    match scores with 
    | [] -> raise InvalidHole
    | s1::rest_of_scores -> 
      let score_for_hole = List.filter is_hole scores 
      in List.hd score_for_hole end

(* get score for one player at one hole from scorecard *)
let rec player_score (player:Player.t) scorecard hole= 
  match scorecard with 
  | [] -> raise InvalidHole
  | hole_list1::rest_of_scorecard-> 
    let their_scores = grab_hole_from_player player hole_list1 hole in 
    if their_scores.hole == hole then their_scores 
    else player_score player rest_of_scorecard hole

(* get the hole_scores for all players at a specific hole  *)
let rec scores_for_hole players scorecard hole = 
  match players with 
  | [] -> []
  | p1::rest_of_players -> begin 
      let just_their_scores = player_score p1 scorecard hole in
      just_their_scores::(scores_for_hole rest_of_players scorecard hole)
    end

(* get the integer score of the best score for a specific hole  *)
let rec top_score_of_hole (hole_list: hole_score list) max = 
  (* let max_score = List.hd hole_list in *)
  match hole_list with
  |[] -> max.hole_score
  | s1::rest_of_scores -> begin 
      if s1.hole_score < max.hole_score 
      then top_score_of_hole rest_of_scores s1
      else top_score_of_hole rest_of_scores max
    end

(* returns a list of the winners of the hole given the best score  *)
let rec get_winners score_list best_score = 
  match score_list with 
  |[] -> []
  |s1::rest_of_scores -> 
    if s1.hole_score = best_score 
    then s1::get_winners rest_of_scores best_score 
    else get_winners rest_of_scores best_score 

let winner_of_hole (game:t) hole = 
  let scorecard = game.scores in 
  let scores_for_hole = scores_for_hole game.roster scorecard hole in 
  let top_score = top_score_of_hole scores_for_hole (List.hd scores_for_hole) 
  in get_winners scores_for_hole top_score

(* gets the winning score of all players  *)
let rec winning_score roster (best:Player.t) = 
  match roster with 
  | [] -> best.overall_score
  | p1::rest_of_roster -> begin 
      if p1.overall_score < best.overall_score
      then winning_score rest_of_roster p1 
      else winning_score rest_of_roster best end

(* returns winner or winners of game best on who has best overall 
   score at end of the game*)
let rec winners_roster roster sc = 
  match roster with 
  |[] -> []
  |p1::rest_of_roster -> begin 
      if p1.overall_score = sc 
      then p1::winners_roster rest_of_roster sc 
      else winners_roster rest_of_roster sc 
    end

(* returns a list of winners  *)
let winner_of_game game = 
  failwith "Unimplemented"

let best_sc = winning_score game.roster (List.hd game.roster) in 
winners_roster game.roster best_sc

let get_course game = game.course

let update_location swing game = 
  let hole_num = current_hole game in 
  let hole_loc = Course.get_hole_loc (get_course game) hole_num in
  let player_loc = Player.get_player_location game.current_player in
  failwith "Unimplemented"
