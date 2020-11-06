
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
exception InvalidScore

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

let current_course game = game.course

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
  done;
  game.scores



let update_turn game (hole:Course.hole) = 
  let next_player = Array.make 1 game.roster.(0) in 
  let next_dist = 
    Array.make 1 (dist_from_hole (get_player_location next_player.(0)) 
                    (get_hole_loc game.course (get_hole_number hole))) in 
  for i = 0 to Array.length game.roster do 
    let cur_player = game.roster.(i) in 
    let cur_dist = dist_from_hole (get_player_location cur_player) 
        (get_hole_loc game.course (get_hole_number hole)) in 
    if cur_dist > next_dist.(0) && next_dist.(0) != 0. then 
      next_dist.(0) <- cur_dist;
    next_player.(0) <- cur_player
  done;
  next_player.(0)

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

let sum_scores game player = 
  let sc = game.scores in 
  let sums = Array.of_list [0] in 
  for i = 0 to Array.length sc do 
    let sc_per_hole = sc.(i) in 
    for j = 0 to Array.length sc_per_hole do 
      let cur_sc = sc_per_hole.(j) in
      if cur_sc.player = player then
        let update_sc = sums.(0) + cur_sc.hole_score
        in sums.(0) <- update_sc 
      else sums.(0) <- sums.(0) 
    done;
  done;
  sums.(0)

(* returns an array of all the current total scores of the player *)
let scores_list g p = Array.to_list (Array.map (sum_scores g) p )

let winning_score_game score_lst= List.fold_left max 0 score_lst

(* returns a list of winners  *)
let winner_of_game game = 
  (* let last_hole = Array.length game.scores -1 in *)
  let scores = scores_list game game.roster in 
  let winning_score = winning_score_game scores in 
  let winner_inds_unfiltered = List.mapi 
      (fun i elem -> if elem = winning_score then i else -1) scores in
  let winner_inds = Array.of_list (List.filter (fun x -> x > -1) 
                                     winner_inds_unfiltered) in 
  let winner_arr =Array.make (Array.length winner_inds) game.roster.(0) in 
  for i = 0 to Array.length(winner_inds) do 
    winner_arr.(i) <- game.roster.(i)
  done;
  winner_arr

let play_hole game =
  failwith "unimplemented"

let print_scorecard game = failwith "Unimplemented"

