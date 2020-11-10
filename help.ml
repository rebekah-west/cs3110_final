open Str

let table_of_contents = 
  "How the game works \n Player input \n Swing input \n Who goes first
  Scoring procedure \n"


let game_explanation = 
  "This game is modeled after a normal golf game. You begin by entering your
  player information. You then select which course you would like to play.
  There are several courses that come with the game. These courses vary in
  length, difficulty, and terrain. When making the decision of which course
  to play, you will be provided with a short description of each of the options
  After selecting a course, you choose how many players there are and input 
  player information. You will then be taken to the first hole of the course.
  Each hole will be played according to the rules of golf. After completing all
  the holes, the game will end and a winner will be declared.\n"

let player_input = 
  "You will first be prompted to enter how many players will be in the game. 
  Please enter an integer from 1 to 5. \n For each player you will then ...\n"

let swing_input = "yay i love describing\n"

let who_goes_first = 
  "The first person to enter their information will go first on the first hole. 
  After that, the person with the lowest score on the previous hole will go
  first on the next hole. Within a hole, the person farthest from the hole 
  always goes next. This is all calculated for you.\n"

let scoring_explanation =
  "A point is added to your score for each swing it takes to reach the hole. 
  Wait idk how golf scoring works. Par...? Handicap...?\n"



(* let help_menu () =
   Printf.printf "We are here to help. \n";
   Printf.printf "\nTable of Contents: \n %s"  table_of_contents;
   Printf.printf "\nGame Explanation: \n %s"  game_explanation;
   Printf.printf "\nPlayer Input: \n %s"  player_input;
   Printf.printf "\nSwing Input: \n %s" swing_input;
   Printf.printf "\nWho Goes First: \n %s" who_goes_first;
   Printf.printf "\nScoring Explanation: \n %s" scoring_explanation; *)

let num_players_explanation = "TODO"
let name_explanation = "TODO"
let skill_explanation = "TODO"
let strength_explanation = "TODO"
let handicap_explanation = "TODO"


let rec player_input_menu_parser selection =
  match selection with 
  | 1 -> Printf.printf "Showing explanation for number of players... \n %s" 
           num_players_explanation;
    Printf.printf "\n Please enter another selection from the player input menu >";
    player_input_menu_parser (int_of_string (read_line()))
  | 2 -> Printf.printf "Showing explanation for names... \n %s" 
           name_explanation;
    Printf.printf "\n Please enter another selection from the player input menu >";
    player_input_menu_parser (int_of_string (read_line()))
  | 3 -> Printf.printf "Showing explanation for skill level options... \n %s" 
           skill_explanation;
    Printf.printf "\n Please enter another selection from the player input menu >";
    player_input_menu_parser (int_of_string (read_line()))
  | 4 -> Printf.printf "Showing an explanation of strength options... \n %s" 
           strength_explanation;
    Printf.printf "\n Please enter another selection from the player input menu >";
    player_input_menu_parser (int_of_string (read_line()))
  | 5 -> Printf.printf "Showing an explanation of handicap... \n %s"
           handicap_explanation;
    Printf.printf "\n Please enter another selection from the player input menu >";
    player_input_menu_parser (int_of_string (read_line()))
  | 6 -> Printf.printf "Going back to the main menu... \n";

  | _ -> Printf.printf "Unknown selection, please enter another selection> ";
    player_input_menu_parser (int_of_string (read_line()))

let player_input_menu () = 
  Printf.printf "Welcome to the player input help menu! Please enter the \n";
  Printf.printf "number next to the topic you'd like to learn more about! \n";
  Printf.printf "1) Number of players \n";
  Printf.printf "2) Name \n";
  Printf.printf "3) Beginner, Intermediate, Advanced Options \n";
  Printf.printf "4) Below Average, average, above average strength options \n";
  Printf.printf "5) Handicap \n";
  Printf.printf "6) Go back to the main menu \n";
  Printf.printf "Enter Number > ";
  let selection = int_of_string (read_line()) in 
  player_input_menu_parser selection

let rec main_menu_parser selection = 
  match selection with
  | 1 -> Printf.printf "Showing game explanation... \n \n %s" game_explanation;
    Printf.printf "\n Please enter another selection from the menu >";
    main_menu_parser (int_of_string (read_line()))

  | 2 -> Printf.printf "Going to the player input help menu... \n";
    player_input_menu()

  | 3 -> Printf.printf "Going to the swing input help menu... \n"

  | 4 -> Printf.printf "Showing an explanation of who goes first... \n %s"
           who_goes_first;
    Printf.printf "\n Please enter another selection from the menu >";
    main_menu_parser (int_of_string (read_line()))

  | 5 -> Printf.printf 
           "Showing an explanation of scoring... \n %s" scoring_explanation;
    Printf.printf "\n Please enter another selection from the menu > ";
    main_menu_parser (int_of_string (read_line()))

  | 6 -> Printf.printf "Going back to the game... \n"

  | _ -> Printf.printf "Unknown selection, please enter another selection> ";
    main_menu_parser (int_of_string (read_line()))

let rec main_menu_rep (first : bool) =
  if first != true then 
    Printf.printf "Welcome back to the help menu! Please enter the number \n";
  if first != true then 
    Printf.printf "next to the topic you would like to learn more about! \n";
  Printf.printf "1) Game Explanation \n";
  Printf.printf "2) Player Input \n";
  Printf.printf "3) Swing Input \n";
  Printf.printf "4) Who Goes First \n";
  Printf.printf "5) Scoring Explanation:\n";
  Printf.printf "6) Go back to the game \n";
  Printf.printf "Enter Number > ";
  let selection = int_of_string (read_line()) in 
  main_menu_parser selection;
  if selection != 6 then 
    main_menu_rep false


let main_menu_init () = 
  Printf.printf "Welcome to the help menu! Please enter the number next to \n";
  Printf.printf "the topic you would like to learn more about! \n";
  main_menu_rep true;



