open Game
open Player
open Course

(*********************************************************************)
(*  Example Representation of one players scorecard

    _____________________________________________
    | Hole Number | 1 | 2 | 3 | 4 | Total | Par | 
    |_____________|___|___|___|___|_______|_____|
    |    Jenna    | 4 | 3 | 5 | 4 |  16   | 14  | 
    |_____________|___|___|___|___|_______|_____|
    |   Rebekah   | 2 | 5 | 4 | 4 |  15   | 14  | 
    |_____________|___|___|___|___|_______|_____|


*)
(*********************************************************************)

let player_name_str player = Player.get_player_name player 

let make_zero i = if i < 0 then 0 else i

let get_course_par course = 
  let par = ref 0 in 
  let holes = get_holes course in
  for i=0 to Array.length holes -1 do 
    let hole_par = get_hole_par holes.(i) in
    par := !par + hole_par
  done;
  !par

let total_str game player course = 
  let total_score = sum_scores game player in
  string_of_int (make_zero total_score)

let par_str course = 
  let par = get_course_par course in 
  string_of_int par

let hole_num_str hole = 
  let hl = get_hole_number hole in 
  " " ^ string_of_int hl ^ " |"

let hole_score_str game player hole_num = 
  let hl = get_hole_score game player hole_num in 
  " " ^ string_of_int (make_zero hl) ^ " |"

let hole1 = "____"
let hole3_5 = "___|"

let scorecard_player_init game course = 
  let s1 = ref "_______________" in 
  let s2 = ref "| Hole Number |" in
  let s3 = ref "|_____________|" in
  let tp1 = "______________" in 
  let tp2 = " Total | Par |" in
  let tp3 = "_______|_____|" in

  let holes = get_holes course in 
  for i = 0 to Array.length holes -1 do 

    let add2 = hole_num_str holes.(i) in 
    s1 := !s1 ^ hole1; s2 := !s2 ^ add2;
    s3 := !s3 ^ hole3_5; 

  done;
  (* print_string ( "\n" ^ player_name ^ "\n") ; *)
  print_string (!s1 ^ tp1 ^ "\n"); 
  print_string (!s2 ^ tp2 ^"\n");
  print_string (!s3 ^ tp3 ^ "\n"); 
  (* print_string (!s4 ^ tot ^ "\n"); *)
  (* print_string (!s5 ^ par ^ "\n" ^ "\n") *)
;;

let scorecard_per_player game player course = 
  let s4 = ref "|" in
  let s5 = ref "|_____________|" in
  let space = " " in
  let hole3_5 = "___|" in
  let holes = get_holes course in 
  let player_name = player_name_str player in
  let player_name_len = String.length player_name in 
  let spacing = 13 - player_name_len in 
  let space_before = if spacing mod 2 = 0 then spacing/2 else (spacing-1)/2 in
  let space_after = spacing - space_before in
  for i = 0 to space_before - 1 do 
    s4 := !s4 ^ space
  done;
  s4 := !s4 ^ player_name;
  for i = 0 to space_after- 1 do 
    s4 := !s4 ^ space
  done;
  s4 := !s4 ^ "|";
  let tot = total_str game player course in
  let par = par_str course in
  for i = 0 to Array.length holes -1 do 
    let hole_num = get_hole_number holes.(i) in 
    let add4 = hole_score_str game player hole_num in
    s4 := !s4 ^ add4;
    s5 := !s5 ^ hole3_5;
  done;
  let total4 = "   " ^ tot ^ "   |" in 
  let par4 = "  " ^ par ^ "  |" in 
  let tp5 = "_______|_____|" in
  print_string (!s4 ^ total4 ^ par4 ^ "\n"); 
  print_string (!s5 ^ tp5 ^ "\n");
;;


let scorecard_printer game (course:Course.t) = 
  let roster = game_roster game in 
  scorecard_player_init game course;
  for i = 0 to Array.length roster -1 do 
    let cur_player = roster.(i) in 
    scorecard_per_player game cur_player course
  done;