open Course
open Game
open Player

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
  Printf.printf "The winner is %s" (Player.get_player_name (Game.winner_of_game game));
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