open Course
open Player
open Command
open Str
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

(** [pp_tup] transforms a float tuple to a string for printing *)
let pp_tup (k,v) = "(" ^ string_of_float k ^ ", " ^ string_of_float v ^ ")"

(** [current_hole game] returns the hole currently being played *)
let current_hole game = game.current_hole

(** [init_hole_score player hole] initializes a 0 score for that player and
    that hole *)
let init_hole_score hole player = {
  hole = hole;
  player = player;
  hole_score = 0 - get_player_handicap player; }

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
  for i = 0 to (Array.length sc)-1 do 
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
  for i = 0 to (Array.length game.roster)-1 do 
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
  for i = 0 to (Array.length scorecard)-1 do 
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

(* returns the total score for a player for all holes played so far  *)
let sum_scores game player = 
  let sc = game.scores in 
  let sums = Array.of_list [0] in 
  for i = 0 to (Array.length sc)-1 do 
    let sc_per_hole = sc.(i) in 
    for j = 0 to (Array.length sc_per_hole)-1 do 
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

let winning_score_game score_lst= List.fold_left min 180 score_lst

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
  for i = 0 to (Array.length(winner_inds))-1 do 
    winner_arr.(i) <- game.roster.(i)
  done;
  winner_arr

(* [find_winner sw sl pl] returns a tuple containing the winning score and
   the winning player from a list of scores and players
   Requires: The list of scores [sl] and players [pl] are of the same length 
   and their indexing corresponds to each other *)
let rec find_winner scores_list players_list acc=
  match scores_list with 
  | [] -> acc
  | h :: t -> begin
      if h < fst (List.hd acc) 
      then find_winner t (List.tl players_list) [(h, List.hd players_list)]
      else if h > fst (List.hd acc)
      then find_winner t (List.tl players_list) acc
      else find_winner t (List.tl players_list) (acc @ [(h, List.hd players_list)])
    end

let winner_of_game2 game = 
  let scores = scores_list game game.roster in 
  let players = Array.to_list game.roster in 
  let curr_min = List.hd scores - get_player_handicap (List.hd players) in
  let curr_winner = List.hd players in 
  find_winner (List.tl scores) (List.tl players) [(curr_min, curr_winner)]

(** [updated_rostr roster p] takes in a roster and the new player to update
    with and returns that updated roster*)
let update_roster roster player = 
  for i = 0 to (Array.length roster)-1 do
    if get_player_name roster.(i) = get_player_name player then 
      roster.(i) <- player 
  done;
  roster

(** [print_location player] prints the current location of the player *)
let print_location player = print_string 
    (Player.get_player_name player ^ "\'s new location is " ^
     (pp_tup (Player.get_player_location player)) ^ "\n")

(* [print_init_loc g] prints the location of the player who is about to swing 
   along with the location of the hole *)
let print_init_locs game = 
  let hole = current_hole game in 
  let hole_loc = Course.get_hole_loc game.course hole in 
  let pl_loc = Player.get_player_location game.current_turn in
  let pl_name = Player.get_player_name game.current_turn in 
  print_string ("\nIt is now " ^ pl_name ^ "'s turn. \n");
  print_string (pl_name ^ "\'s location is " ^ (pp_tup (pl_loc)) ^ "\n");
  print_string ("The hole's location is " ^ (pp_tup (hole_loc)) ^ "\n")

(** [play_one_swing_of_hole g] takes in the current game and iterates the game
    to its newest version, returning the updated game*)
let play_one_swing_of_hole game =
  print_init_locs game;
  let command = parse_swing () in 
  let new_loc = calculate_location game.current_turn command 
      game.current_hole game.course in 
  let updated_player = update_player_location game.current_turn new_loc in
  print_location updated_player;
  let updated_roster = update_roster game.roster updated_player in
  {
    roster = updated_roster; 
    course = game.course;
    scores = update_score game; 
    current_hole = game.current_hole;
    current_turn = update_turn game (get_hole game.course game.current_hole); 
    holes_played = game.holes_played;
  } 

(* [get_player_score] p game gets the current score for a player on the current
   hole *) 
let get_player_score p game = 
  let hole_sc = game.scores.(game.current_hole) in 
  let score = Array.make 1 0 in 
  for i =0 to (Array.length hole_sc)-1 do 
    if hole_sc.(i).player == p
    then score.(0) <- hole_sc.(i).hole_score
  done;
  score.(0)

(** [someone_still_playing roster] *)
let rec someone_still_playing roster game hol_loc =
  match Array.to_list roster with 
  | [] -> false
  | h::t -> begin
      let player_score = get_player_score h game in 
      if (get_player_location h != hol_loc) && (player_score <= 10)
      then true else (someone_still_playing (Array.of_list t) game hol_loc)
    end

let pp_string s = "\"" ^ s ^ "\""

let print_score game player = print_string 
    (Player.get_player_name player ^ "\'s score is " ^
     (pp_string (string_of_int (sum_scores game player))) ^ 
     "after hole" ^ pp_string (string_of_int game.current_hole) ^ "\n")


let print_scorecard (game:t) = 
  for i = 0 to Array.length (game.roster) do
    (* Printf.printf "Player %s" (pp_string (get_player_name game.roster.(i)));
       Printf.printf "Your current Score is %s" 
       (pp_string (string_of_int (sum_scores game game.roster.(i)))); *)
    print_score game game.roster.(i)
  done


(** [switch_holes g] updates the game to a the new game when it is time to 
    switch holes*)
let switch_holes game = 
  let update_ind = game.current_hole in
  let course_arr = get_holes game.course in 
  print_scorecard game;
  let new_hole = course_arr.(update_ind) in
  { roster = game.roster; 
    course = game.course;
    scores = game.scores; 
    current_hole = get_hole_number new_hole;
    current_turn = game.current_turn; 
    holes_played = game.holes_played@[game.current_hole]; }

(* plays a hole to completion *)
let rec play_hole game = 
  if someone_still_playing 
      game.roster game (Course.get_hole_loc game.course game.current_hole)
  then play_hole (play_one_swing_of_hole game)
  else switch_holes game

let play_game players course = 
  let game = init_game players course in 
  let game_arr = Array.make 1 game in 
  for i = 0 to (Array.length (get_holes course))-1 do
    game_arr.(0) <- (play_hole game)
  done;
  game_arr.(0)
