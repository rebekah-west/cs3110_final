open Course
open Game
open Player
open Parse
open Scorecard

exception End of string

let play_game f =
  let course = Course.from_json(Yojson.Basic.from_file(f)) in
  let players = Player.init_players () in
  let game = ref (Game.init_game players course) in
  scorecard_printer !game course;
  let counter = ref 0 in 
  for i=0 to Array.length (Course.get_holes course)-1 do 
    let cur_hole = get_hole_number ((Course.get_holes course).(i)) in 
    print_string ("\nHole " ^ (string_of_int cur_hole ) ^ "\n" );
    print_string (description course cur_hole);
    game := (Game.play_hole !game);
    if !counter < Array.length (Course.get_holes course)-1 then
      scorecard_printer !game course;
    counter := !counter + 1
  done;
  let winners = Array.to_list 
      (Array.map Player.get_player_name (Game.winner_of_game !game)) in
  winner_printer winners;
  print_string "\nThe complete scorecard is: \n" ;
  scorecard_printer !game course;
  raise (End "Thank you for visiting Golf, Inc. We hope you come back soon.")



let course_options = 
  "The two courses that come with this Golf System are PebbleBeach.json and 
  RobertTrent.json. PebbleBeach has 3 holes and is hard. RobertTrent has 2 
  holes and is easy. You can also upload your own course file. Instructions 
  on how to construct your own course json can be found in the help menu. \n"

let rec course_menu_init () = 
  Printf.printf "Please select the number next to the course you would like to play! \n";
  Printf.printf "1) Robert Trent \n";
  Printf.printf "2) Pebble Beach \n";
  Printf.printf "3) Self-Uploaded \n";
  print_string  "> ";
  match read_line () with
  | "1" -> print_string "Thank you for selecting the Robert Trent golf course, enjoy your game!\n"; 
    play_game "RobertTrent.json"
  | "2" -> print_string "Thank you for selecting the Pebble Beach golf course, enjoy your game!\n"; 
    play_game "PebbleBeach.json"
  | "3" -> failwith "unimplemented"
  | _ -> print_string "Your input was not recognized, please try again\n"; 
    course_menu_init()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to your remote golf game.\n");
  print_string course_options;
  course_menu_init ()

(* Execute the game engine. *)
let () = main ()