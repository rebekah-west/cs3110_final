open Course
open Game
open Player
open Parse
open Scorecard

let play_game f =
  let course = Course.from_json(Yojson.Basic.from_file(f)) in
  let players = Player.init_players () in
  let game = ref (Game.init_game players course) in
  scorecard_printer !game course;
  (* let rec hole game hole_num = 
     let valid = (hole_num ) <= (Course.num_holes course) in
     match valid with
     | false -> Printf.printf "Congratulations! You have completed the course.";
     | true -> hole (Game.play_hole game) (hole_num+1)
     in hole game 0; *)
  for i=0 to Array.length (Course.get_holes course)-1 do 
    game := (Game.play_hole !game)
  done;
  let winners = Array.to_list 
      (Array.map Player.get_player_name (Game.winner_of_game !game)) in
  Printf.printf "The winner is %s" (pp_list pp_string winners);
  print_string "\n";
  print_string "The complete scorecard is: " ;
  scorecard_printer !game course;
  Printf.printf "Thank you for visiting Golf, Inc. We hope you come back soon."


let course_options = 
  "The two courses that come with this Golf System are PebbleBeach.json and 
  RobertTrent.json. PebbleBeach has 3 holes and is hard. RobertTrent has 2 
  holes and is easy. You can also upload your own course file. Instructions 
  on how to construct your own course json can be found in the help menu. \n"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to your remote golf game.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string course_options;
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()