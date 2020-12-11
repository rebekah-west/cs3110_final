open Game
open Player
open Course

(*********************************************************************)
(*  Example Representation of one players scorecard

    Player: Jenna
    _______________________________
    | Hole Number | 1 | 2 | 3 | 4 |
    |_____________|___|___|___|___| 
    |    Score    | 4 | 3 | 5 | 4 | Total: 16, (+2)
    |_____________|___|___|___|___| Par: 14

*)
(*********************************************************************)

let player_name_str player = "Player: " ^ Player.get_player_name player 

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
  " Total: " ^ string_of_int (make_zero total_score)

let par_str course = 
  let par = get_course_par course in 
  " Par: " ^ string_of_int par

let hole_num_str hole = 
  let hl = get_hole_number hole in 
  " " ^ string_of_int hl ^ " |"

let hole_score_str game player hole_num = 
  let hl = get_hole_score game player hole_num in 
  " " ^ string_of_int (make_zero hl) ^ " |"

let hole1 = "____"
let hole3_5 = "___|"

let scorecard_player game player course = 
  let s1 = ref "_______________" in 
  let s2 = ref "| Hole Number |" in
  let s3 = ref "|_____________|" in
  let s4 = ref "|    Score    |" in
  let s5 = ref "|_____________|" in
  let holes = get_holes course in 
  let player_name = player_name_str player in
  let tot = total_str game player course in
  let par = par_str course in
  for i = 0 to Array.length holes -1 do 
    let hole_num = get_hole_number holes.(i) in 
    let add2 = hole_num_str holes.(i) in 
    let add4 = hole_score_str game player hole_num in
    s1 := !s1 ^ hole1; s2 := !s2 ^ add2;
    s3 := !s3 ^ hole3_5; s4 := !s4 ^ add4;
    s5 := !s5 ^ hole3_5;
  done;
  print_string ( "\n" ^ player_name ^ "\n") ;
  print_string (!s1 ^ "\n"); 
  print_string (!s2 ^ "\n");
  print_string (!s3 ^ "\n"); 
  print_string (!s4 ^ tot ^ "\n");
  print_string (!s5 ^ par ^ "\n" ^ "\n")
;;

let scorecard_printer game (course:Course.t) = 
  let roster = game_roster game in 
  for i = 0 to Array.length roster -1 do 
    let cur_player = roster.(i) in 
    scorecard_player game cur_player course
  done;