open Course
open Game
open Player

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(** [play_game f] starts the adventure in file [f]. *)
let play_game f =
  let course = Course.from_json(Yojson.Basic.from_file(f)) in
  let players = Player.init_players () in
  let game = Game.init_game players course in
  let rec hole game hole_num = 
    let valid = hole_num <= (Course.num_holes course) in
    match valid with
    | false -> Printf.printf "Congratulations! You have completed the course.";
    | true -> hole (Game.play_hole game) (hole_num+1);
  in hole game 1;
  let winners = Array.to_list (Array.map Player.get_player_name (Game.winner_of_game game)) in
  Printf.printf "The winner is %s" (pp_list pp_string winners);
  Printf.printf "The complete scorecard is" ;
  Printf.printf "Thank you for visiting Golf, Inc. We hope you come back soon."

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to your remote golf game.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()