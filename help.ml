open Str


(* main menu explanations *)
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

let swing_input = "yay i love describing\n"

let who_goes_first = 
  "The first person to enter their information will go first on the first hole. 
  After that, the person with the lowest score on the previous hole will go
  first on the next hole. Within a hole, the person farthest from the hole 
  always goes next. This is all calculated for you.\n"

let scoring_explanation =
  "A point is added to your score for each swing it takes to reach the hole. 
  Wait idk how golf scoring works. Par...? Handicap...?\n"

(* for player initialization menu*)
let num_players_explanation = "TODO"
let name_explanation = "TODO"
let skill_explanation = "TODO"
let strength_explanation = "TODO"
let handicap_explanation = "TODO"

(*for clubs menu*)
let driver_explanation = "The driver is a club that will increase your power
by 50% but will result in a decrease in a 20% decrease inyour accuracy. It is 
best used for very long shots"
let nine_iron_explanation = "The Nine Iron will decrease your power by 10% but
increase your accuracy by 10%. It is best for mid-long range shots."
let eight_iron_explanation = "The Eight Iron will slightly decrease your power 
by 20% but increase your accuracy by 20%. It is best used for mid-long 
range shots."
let putter_explanation = "The putter will decrease your power by 50% but
 increase your accuracy by 30%. It is best for shots very close to the hole."
let sand_wedge_explanation = "The sand wedge will decrease your power by 20%
but increase your power by 10%. It is best for mid-ranged shots."
let pitching_wedge_explanation = "The pitching wedge will decrease your power 
by 30% but increase your accuracy by 50%. It is best used for short to 
mid-ranged shots."

(* for player input help menu*)
let power_explanation = "TODO"
let angle_explanation = "TODO"
let alignment_explanation = "TODO"

(* for scoring help menu *)
let turn_explanation = "TODO"
let par_explanation = "TODO"
let stroke_limit_explanation = "TODO"
let winning_a_hole_explanation = "TODO"
let winning_the_game_explanation = "TODO"

let scoring_menu_parser selection =
  match selection with 
  | 1 -> Printf.printf "Showing turn overview... \n %s" 
           turn_explanation;
    Printf.printf "\n Please enter another selection from the scoring menu >";

  | 2 -> Printf.printf "Showing explanation of Par... \n %s" 
           par_explanation;
    Printf.printf "\n Please enter another selection from the scoring menu >";

  | 3 -> Printf.printf "Showing explanation of Stroke Limit... \n %s" 
           stroke_limit_explanation;
    Printf.printf "\n Please enter another selection from the scoring menu >";

  | 4 -> Printf.printf "Showing an explanation of winning a hole... \n %s" 
           winning_a_hole_explanation;
    Printf.printf "\n Please enter another selection from the scoring menu >";

  | 5 -> Printf.printf "Showing an explanation of Winning the Game... \n %s" 
           winning_the_game_explanation;
    Printf.printf "\n Please enter another selection from the scoring menu >";

  | 6 -> Printf.printf "Going back to the main menu... \n";
    ()
  | _ -> Printf.printf "Unknown selection, please enter another selection> ";
    ()

let scoring_menu () = 
  Printf.printf "\n Welcome to the scoring help menu! Please enter the number \n";
  Printf.printf "next to the topic you'd like to learn more about! \n";
  Printf.printf "1) Turn Overview \n";
  Printf.printf "2) Par \n";
  Printf.printf "3) Stroke Limit \n";
  Printf.printf "4) Winning a hole \n";
  Printf.printf "5) Winning the game \n";
  Printf.printf "6) Go back to the main menu \n";
  Printf.printf "Enter Number > ";
  let selection = ref (int_of_string (read_line()) )in 
  while (!selection != 6) do 
    scoring_menu_parser (!selection);
    selection := (int_of_string (read_line()))
  done;
  scoring_menu_parser 6


let club_menu_parser selection =
  match selection with 
  | 1 -> Printf.printf "Showing explanation for Driver... \n %s" 
           driver_explanation;
    Printf.printf "\n Please enter another selection from the club menu >";
    (* club_menu_parser (int_of_string (read_line())) *)
  | 2 -> Printf.printf "Showing explanation for Nine Iron... \n %s" 
           nine_iron_explanation;
    Printf.printf "\n Please enter another selection from the club menu >";
    (* club_menu_parser (int_of_string (read_line())) *)
  | 3 -> Printf.printf "Showing explanation for Eight Iron... \n %s" 
           eight_iron_explanation;
    Printf.printf "\n Please enter another selection from the club menu >";
    (* club_menu_parser (int_of_string (read_line())) *)
  | 4 -> Printf.printf "Showing an explanation of Putter... \n %s" 
           putter_explanation;
    Printf.printf "\n Please enter another selection from the club menu >";
    (* club_menu_parser (int_of_string (read_line())) *)
  | 5 -> Printf.printf "Showing an explanation of Pitching Wedge... \n %s" 
           pitching_wedge_explanation;
    Printf.printf "\n Please enter another selection from the club menu >";
    (* club_menu_parser (int_of_string (read_line())) *)
  | 6 -> Printf.printf "Showing an explanation of Sand Wedge... \n %s" 
           sand_wedge_explanation;
    Printf.printf "\n Please enter another selection from the club menu >";
    (* club_menu_parser (int_of_string (read_line())) *)
  | 7 -> Printf.printf "Going back to the swing input menu... \n";
    ()

  | _ -> Printf.printf "Unknown selection, please enter another selection> ";
    ()

(* club_menu_parser (int_of_string (read_line())) *)

let club_menu () = 
  Printf.printf "\n Welcome to the club help menu! Each club has different \n";
  Printf.printf "attributes that will alter your swing and should be used \n";
  Printf.printf "in different scenarios.  Please enter the number next to \n";
  Printf.printf "the topic you'd like to learn more about! \n";
  Printf.printf "1) Driver \n";
  Printf.printf "2) Nine Iron \n";
  Printf.printf "3) Eight Iron \n";
  Printf.printf "4) Putter \n";
  Printf.printf "5) Pitching Wedge \n";
  Printf.printf "6) Sand Wedge \n";
  Printf.printf "7) Go back to the swing input menu \n";
  Printf.printf "Enter Number > ";
  let selection = ref (int_of_string (read_line()) )in 
  while (!selection != 7) do 
    club_menu_parser (!selection);
    selection := (int_of_string (read_line()) )
  done;
  club_menu_parser 7

let swing_input_menu_parser selection =
  match selection with 

  | 1 -> Printf.printf "Going to the clubs help menu... \n";
    club_menu()

  | 2 -> Printf.printf "Showing explanation for power... \n %s" 
           power_explanation;
    Printf.printf "\n Please enter another selection from the swing input menu >";

  | 3 -> Printf.printf "Showing explanation for angle... \n %s" 
           angle_explanation;
    Printf.printf "\n Please enter another selection from the swing input menu >";

  | 4 -> Printf.printf "Showing an explanation of alignment... \n %s" 
           alignment_explanation;
    Printf.printf "\n Please enter another selection from the swing_input menu >";

  | 5 -> Printf.printf "Going back to the main menu... \n";

  | _ -> Printf.printf "Unknown selection, please enter another selection> ";
    ()

let swing_input_menu_disp (header : bool) = 
  if header then 
    Printf.printf "\n Welcome back to the swing input help menu! Please enter the \n";
  if header then 
    Printf.printf "number next to the topic you'd like to learn more about! \n";
  Printf.printf "1) Clubs \n";
  Printf.printf "2) Power \n";
  Printf.printf "3) Angle \n";
  Printf.printf "4) Alignment \n";
  Printf.printf "5) Go back to the main menu \n";
  Printf.printf "Enter Number > ";
  ()

let swing_input_menu_rep () = 
  Printf.printf "\n Welcome back to the swing input help menu! Please enter the \n";
  Printf.printf "number next to the topic you'd like to learn more about! \n";
  swing_input_menu_disp(false);
  let selection = ref (int_of_string (read_line()) )in 
  while (!selection != 5) do 
    swing_input_menu_parser (!selection);
    if (!selection == 1) 
    then swing_input_menu_disp (true);
    selection := (int_of_string (read_line()) )
  done;
  swing_input_menu_parser 4

let swing_input_menu_init () = 
  Printf.printf "\n Welcome to the swing input help menu! Please enter the \n";
  Printf.printf "number next to the topic you'd like to learn more about! \n";
  swing_input_menu_rep()

let player_input_menu_parser selection =
  match selection with 
  | 1 -> Printf.printf "Showing explanation for number of players... \n %s" 
           num_players_explanation;
    Printf.printf "\n Please enter another selection from the player input menu >";

  | 2 -> Printf.printf "Showing explanation for names... \n %s" 
           name_explanation;
    Printf.printf "\n Please enter another selection from the player input menu >";

  | 3 -> Printf.printf "Showing explanation for skill level options... \n %s" 
           skill_explanation;
    Printf.printf "\n Please enter another selection from the player input menu >";

  | 4 -> Printf.printf "Showing an explanation of strength options... \n %s" 
           strength_explanation;
    Printf.printf "\n Please enter another selection from the player input menu >";

  | 5 -> Printf.printf "Showing an explanation of handicap... \n %s"
           handicap_explanation;
    Printf.printf "\n Please enter another selection from the player input menu >";

  | 6 -> Printf.printf "Going back to the main menu... \n";
    ()

  | _ -> Printf.printf "Unknown selection, please enter another selection> ";
    ()

let player_input_menu_rep () = 
  Printf.printf "\n Welcome to the player initialization input help \n";
  Printf.printf "menu! Please enter the number learn more about! \n";
  Printf.printf "1) Number of players \n";
  Printf.printf "2) Names \n";
  Printf.printf "3) Beginner, Intermediate, Advanced Options \n";
  Printf.printf "4) Below Average, average, above average strength options \n";
  Printf.printf "5) Handicap \n";
  Printf.printf "6) Go back to the main menu \n";
  Printf.printf "Enter Number > ";
  let selection = ref (int_of_string (read_line()) )in 
  while (!selection != 6) do 
    player_input_menu_parser (!selection);
    selection := (int_of_string (read_line()) )
  done;
  player_input_menu_parser 6


let player_input_menu_init () = 
  Printf.printf "\n Welcome to the player initialization input help menu!  \n";
  Printf.printf "Please enter the number next to the topic you'd like to \n";
  player_input_menu_rep ()

let main_menu_parser selection = 
  match selection with
  | 1 -> Printf.printf "Showing game explanation... \n \n %s" game_explanation;
    Printf.printf "\n Please enter another selection from the menu >";

  | 2 -> Printf.printf "Going to the player initialization input help menu... \n";
    player_input_menu_init()

  | 3 -> Printf.printf "Going to the swing input help menu... \n";
    swing_input_menu_init()

  | 4 -> Printf.printf "Showing an explanation of who goes first... \n %s"
           who_goes_first;
    Printf.printf "\n Please enter another selection from the menu >";

  | 5 -> Printf.printf "Showing an explanation of scoring... \n"; 
    scoring_menu()

  | 6 -> Printf.printf "Going back to the game... \n"; ()

  | _ -> Printf.printf "Unknown selection, please enter another selection> ";
    ()

let main_menu_disp (header : bool) = 
  if header then 
    Printf.printf "\n Welcome back to the main help menu! Please enter the number \n";
  if header then 
    Printf.printf "next to the topic you would like to learn more about! \n";
  Printf.printf "1) Game Explanation \n";
  Printf.printf "2) Player Initialization Input \n";
  Printf.printf "3) Swing Input \n";
  Printf.printf "4) Who Goes First \n";
  Printf.printf "5) Scoring and Turns:\n";
  Printf.printf "6) Go back to the game \n";
  Printf.printf "Enter Number > ";
  ()

let main_menu_rep () =
  main_menu_disp(false);
  let selection = ref (int_of_string (read_line()) )in 
  while (!selection != 6) do
    main_menu_parser (!selection);
    if (!selection == 2 || !selection == 3 || !selection == 5) 
    then main_menu_disp (true);
    selection := (int_of_string (read_line()));
  done;
  main_menu_parser 6

let main_menu_init () = 
  Printf.printf "Welcome to the help menu! Please enter the number next to \n";
  Printf.printf "the topic you would like to learn more about! \n";
  main_menu_rep()



