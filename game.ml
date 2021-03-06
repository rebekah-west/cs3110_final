(*****************************************************)
(* Implementations of game and it's functions*)
(*****************************************************)
open Course
open Player
open Command
open Parse
open Visual


(* keeps track of score per hole 
   Player.t keeps track of the overall score of a player *)
type hole_score = {
  hole: Course.hole_number;
  player: Player.t;
  hole_score: int;}

type scorecard = hole_score array array

type t = {
  roster: Player.t array;
  course: Course.t;
  current_hole: Course.hole_number;
  current_turn: Player.t;
  scores: scorecard;
  holes_played: Course.hole_number list;}

exception InvalidHole
exception InvalidScore



(* [current_hole game] returns the hole currently being played *)
let current_hole game = game.current_hole

(* [init_hole_score hole player] initializes a 0 score for that player and
    that hole *)
let init_hole_score hole player = {
  hole = hole;
  player = player;
  hole_score = 0; }

(* [init_scorecard players hole] initializes a 0 score for every player
    on hole [hole] *)
let init_scorecard (players: Player.t array) hole = 
  Array.map (init_hole_score hole) players 

(* [create_scorecard players holes]  *)
let create_scorecard (players: Player.t array) (course: Course.t) = 
  let hole_array = (Array.map get_hole_number (get_holes course)) in
  Array.map (init_scorecard players) hole_array

(* required: there must be at least one hole and one player *)
let init_game (players: Player.t array ) (course: Course.t) = 
  let scores = create_scorecard players course in
  let current_hole = Course.start_hole course in
  let frst_up = players.(0) in 

  let first = {
    roster = players; 
    course = course;
    scores = scores; 
    current_hole = current_hole;
    current_turn = frst_up; 
    holes_played = [];
  } in first

let current_hole game = game.current_hole

let current_course game = game.course

let played game = game.holes_played

let current_turn game = game.current_turn

let current_score game = game.scores

let game_roster game = game.roster

let update_score game = 
  let sc = game.scores.(game.current_hole - 1) in 
  let current_player = game.current_turn in
  for i = 0 to (Array.length sc)-1 do 
    if get_player_name (sc.(i).player) == get_player_name (current_player)
    then let new_hole_score = {
        hole = game.current_hole;
        player = current_player; 
        hole_score = if 9 > sc.(i).hole_score + 1 then 
            sc.(i).hole_score + 1 else 9;}
      in game.scores.(game.current_hole - 1).(i) <- new_hole_score;
      if new_hole_score.hole_score = 9 then 
        print_string "You have maxed out swings for this hole"
  done;
  game.scores

let update_turn game updated_roster (hole:Course.hole) = 
  let next_player = ref updated_roster.(0) in 
  let next_dist = ref (dist_from_hole (get_player_location !next_player) 
                         (get_hole_loc game.course (get_hole_number hole))) in 
  for i = 0 to (Array.length updated_roster)-1 do 
    let cur_player = updated_roster.(i) in 
    let cur_dist = dist_from_hole (get_player_location cur_player) 
        (get_hole_loc game.course (get_hole_number hole)) in 
    if cur_dist > !next_dist && !next_dist != 0. then 
      next_player := cur_player;
    if cur_dist > !next_dist && !next_dist != 0. then 
      next_dist :=  cur_dist;
  done;
  !next_player

(* [winning_score game hole] gets the integer score of the best score 
   for a specific hole  *)
let winning_score game hole = 
  let scorecard = game.scores.(hole) in 
  let lowest_score = scorecard.(0).hole_score in 
  let winner = ref lowest_score in
  for i = 0 to (Array.length scorecard)-1 do 
    let current_scorecard = scorecard.(i) in 
    if current_scorecard.hole_score < lowest_score then 
      let lowest_score = current_scorecard.hole_score
      in winner := lowest_score 
    else 
      winner := lowest_score
  done;
  !winner

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

(* sum_score_helper to make sure loop isn't too nested *)
let sum_score_helper cur_sc player sums= 
  if get_player_name(cur_sc.player) = get_player_name player then
    let update_sc = !sums+ cur_sc.hole_score
    in sums := update_sc 

(* returns the total score for a player for all holes played so far *)
let sum_scores game player = 
  let sc = game.scores in 
  let sums = ref 0 in
  for i = 0 to (Array.length sc)-1 do 
    let sc_per_hole = sc.(i) in 
    for j = 0 to (Array.length sc_per_hole)-1 do 
      let cur_sc = sc_per_hole.(j) in
      sum_score_helper cur_sc player sums
    done; 
  done;
  !sums - get_player_handicap player

let get_hole_score game player hole = 
  let hole_list = game.scores.(hole-1) in 
  let hole = ref hole_list.(0) in
  for i = 0 to Array.length hole_list -1 do 
    let hole_sc = hole_list.(i) in 
    if get_player_name hole_sc.player = get_player_name player then 
      hole := hole_sc
  done;
  !hole.hole_score

(* returns an array of all the current total scores of the player *)
let scores_list g p = 
  Array.to_list (Array.map (sum_scores g) p )

let winning_score_game score_lst= 
  List.fold_left min 180 score_lst

(* returns a list of winners  *)
let winner_of_game game = 
  let scores = scores_list game game.roster in 
  let winning_score = winning_score_game scores in 
  let winner_inds_unfiltered = List.mapi 
      (fun i elem -> if elem = winning_score then i else -1) scores in
  let winner_inds = Array.of_list (List.filter (fun x -> x > -1) 
                                     winner_inds_unfiltered) in 
  let winner_arr =Array.make (Array.length winner_inds) game.roster.(0) in 
  for i = 0 to (Array.length(winner_inds))-1 do 
    let winner_ind = winner_inds.(i) in
    winner_arr.(i) <- game.roster.(winner_ind)
  done;
  winner_arr

(* [print_post_location name pl_loc hole_loc] prints a text message about the 
   location of the player after they have swung *)
let print_post_location name player_loc hole_loc hole_score = 
  print_string (name ^ "\'s new location is " ^ (pp_tup (player_loc) ^ "\n"))

(* [print_pre_location name pl_loc hole_loc] prints text messages about the 
   location of the player and the hole before a swing *)
let print_pre_location name player_loc hole_loc hole_score =  
  print_string ("\nIt is now " ^ name ^ "'s turn. \n");
  print_string (name ^ " is on swing " ^ (string_of_int hole_score) ^ "\n");
  print_string (name ^ "\'s location is " ^ (pp_tup (player_loc)) ^ "\n");
  print_string ("The hole's location is " ^ (pp_tup (hole_loc)) ^ "\n")

let print_location game player func = 
  let hole_num = current_hole game in 
  let hole_loc = Course.get_hole_loc game.course hole_num in 
  let pl_loc = Player.get_player_location player in
  let pl_name = Player.get_player_name game.current_turn in 
  let name = String.capitalize_ascii pl_name in
  let obstacle_locs = Course.get_obstacle_locs game.course hole_num in
  let hole_score = get_hole_score game player hole_num in
  if pl_loc = hole_loc then Visual.congrats ()
  else begin
    func name pl_loc hole_loc (hole_score+1);
    Visual.print_loc hole_loc pl_loc obstacle_locs
  end

(* [update_roster roster p] takes in a roster and the new player to update
    with and returns that updated roster *)
let update_roster roster player = 
  let new_roster = Array.make (Array.length roster) roster.(0) in
  for i = 0 to (Array.length roster)-1 do
    if get_player_name roster.(i) = get_player_name player then 
      new_roster.(i) <- player else
      new_roster.(i) <- roster.(i)
  done;
  new_roster

(* [play_one_swing_of_hole g] takes in the current game and iterates the game
    to its newest version, returning the updated game *)
let play_one_swing_of_hole game =
  print_location game game.current_turn print_pre_location;
  let command = parse_swing () in 
  let new_loc = calculate_location game.current_turn command 
      game.current_hole game.course in 
  let new_score = update_score game in 
  let hole_score = get_hole_score game game.current_turn game.current_hole in
  let updated_player = 
    if hole_score < 10 then update_player_location game.current_turn new_loc
    else update_player_location game.current_turn 
        (get_hole_loc game.course game.current_hole) in
  print_location game updated_player print_post_location;
  let updated_roster = update_roster game.roster updated_player in
  {roster = updated_roster; 
   course = game.course;
   scores = new_score ;
   current_hole = game.current_hole;
   current_turn = update_turn game updated_roster 
       (get_hole game.course game.current_hole); 
   holes_played = game.holes_played;} 

(* [get_player_score] p game gets the current score for a player on the current
   hole *) 
let get_player_score p game = 
  let hole_sc = game.scores.(game.current_hole-1) in 
  let score = ref 0 in 
  for i =0 to (Array.length hole_sc)-1 do 
    if hole_sc.(i).player == p
    then score := hole_sc.(i).hole_score
  done;
  !score

(* [someone_still_playing roster game hole_loc returns true if someone is 
    still playing that hole and false if not] *)
let rec someone_still_playing roster game hol_loc =
  match Array.to_list roster with 
  | [] -> false
  | h::t -> begin
      let player_score = get_player_score h game in 
      if (get_player_location h != hol_loc) && (player_score <= 10)
      then true else (someone_still_playing (Array.of_list t) game hol_loc)
    end

let print_score game player = print_string 
    (Player.get_player_name player ^ "\'s score is " ^
     (pp_string (string_of_int (sum_scores game player))) ^ 
     "after hole" ^ pp_string (string_of_int game.current_hole) ^ "\n")

let print_scorecard (game:t) = 
  for i = 0 to Array.length (game.roster) do
    print_score game game.roster.(i)
  done

let reset_player_loc p = update_player_location p (0., 0.)

(* [switch_holes g] updates the game to a the new game when it is time to 
    switch holes *)
let switch_holes game = 
  let update_ind = game.current_hole in
  let course_arr = get_holes game.course in 
  let new_hole = course_arr.(update_ind) in
  let new_rost = Array.map reset_player_loc game.roster in 
  { roster = new_rost; 
    course = game.course;
    scores = game.scores; 
    current_hole = get_hole_number new_hole;
    current_turn = update_turn game new_rost new_hole; 
    holes_played = game.holes_played@[game.current_hole]; 
  }

(* plays a hole to completion *)
let rec play_hole game = 
  let still = someone_still_playing 
      game.roster game (Course.get_hole_loc game.course game.current_hole) in
  if still then
    play_hole (play_one_swing_of_hole game) else 
  if game.current_hole < Array.length (get_holes game.course) then 
    switch_holes game 
  else game

let play_game players course = 
  let game = init_game players course in 
  let game_update = ref game in 
  for i = 0 to (Array.length (get_holes course))-1 do
    game_update := (play_hole !game_update);
  done;
  !game_update
