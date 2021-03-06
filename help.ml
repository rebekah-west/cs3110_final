open Str


let remove_blanks parsbl = 
  parsbl |> Str.global_replace (Str.regexp " ") ""

let parse str = 
  let parsed = str|> remove_blanks |> String.lowercase_ascii in 
  parsed

let rec string_catcher str = 
  try int_of_string str
  with Failure _ -> begin
      Printf.printf "\n Your input was not recognized, please try again> ";
      read_line () |> parse |> string_catcher
    end

(* main menu explanations *)
let game_explanation = 
  "This game is modeled after a normal golf game. You begin by selecting which 
  course you would like to play. There are several courses that come with the 
  game. These courses vary in length, difficulty, and terrain. When making the 
  decision of which course to play, you will be provided with a short 
  description of each of the options. After selecting a course, you choose how 
  many players there are and input player information. You will then be taken 
  to the first hole of the course. Each hole will be played according to the 
  rules of golf. After completing all the holes, the game will end and a 
  winner will be declared.\n"

let swing_input = 
  "When it becomes your turn, you are prompted to enter information about 
  your swing. You will be asked 4 different questions to determine your swing.
  The first is which club you would like to use. The second is how hard you 
  want to hit the ball. The third is what angle up from the ground you want 
  the ball to travel (think of the difference between a line drive and a 
  rainbow). The fourth is alignment, such as hitting the ball to the left of 
  the hole to account for wind, avoiding obstacles, slope of the green etc. \n"

let who_goes_first = 
  "The first person to enter their information will go first on the first hole. 
  After that, the person with the lowest score on the previous hole will go
  first on the next hole. Within a hole, the person farthest from the hole 
  always goes next. This is all calculated for you.\n"

let scoring_explanation =
  "A point is added to your score for each swing it takes to reach the hole. 
  Wait idk how golf scoring works. Par...? Handicap...?\n"


(* for course selection menu*)
let course_selection_explanation = 
  "There are two courses provided with the game. These are PebbleBeach.json 
  and RobertTrent.json. You can also choose to create your own golf course 
  and upload it to the same folder as the source code. You would then be 
  able to type in the name of the json representing your own course and play 
  that course instead of one of the two pre-made options. 

  In creating your own course, you must mimic the structure of the given 
  course json files. This includes having a list of holes and a difficulty 
  (string easy, medium, or hard). 
  For each hole, you must include hole_number (int), par_number (int), 
  hole_location (string of an integer double), description (string), and a 
  list of terrain objects. Each terrain object must include name (string), 
  location (string of an integer double) and size (string small, medium, or 
  large). All locations must be within a 500x500 grid. If these types are not 
  followed, program behavior is not defined.\n"

(* for player initialization menu*)
let num_players_explanation = 
  "The number of players represents how many people will be playing golf during 
  this round. For example, if you are playing with two friends, you should 
  enter the number 3. \n"
let name_explanation = 
  "The name entered will be used to represent you throughout the game. It will 
  be referenced at the start and end of your turn as well as in the printed 
  scores. \n"
let skill_explanation = 
  "Skill represents your estimated skill at golf. Since swings are calculated 
  with math instead of physical laws of nature, we factor this in to improve 
  your swings. However, just like real life, there is an element of randomness 
  since every good golf player knows that skill is not a guarantee of 
  perfection with every swing. \n"
let strength_explanation = 
  "Strength represents a baseline power behind each swing. Stronger players 
  will hit the ball further with less effort, while weaker players will have 
  to swing harder to have the same result. \n"
let handicap_explanation = 
  "The handicap represents a feature from golf that allows players of varying 
  skill levels to be on more equal playing ground. For those who are 
  significantly worse at golf, they can enter a handicap which will be 
  subtracted from their final score. \n"

(*for clubs menu*)
let driver_explanation = 
  "The driver is a club that will increase your power by 50% but will result 
  in a decrease in a 20% decrease in your accuracy. It is best used for very 
  long shots. \n"
let nine_iron_explanation = 
  "The Nine Iron will decrease your power by 10% but increase your accuracy 
  by 10%. It is best for mid-long range shots.\n"
let eight_iron_explanation = 
  "The Eight Iron will slightly decrease your power by 20% but increase your 
  accuracy by 20%. It is best used for mid-long range shots.\n"
let putter_explanation = 
  "The putter will decrease your power by 50% but increase your accuracy 
  by 30%. It is best for shots very close to the hole.\n"
let sand_wedge_explanation = 
  "The sand wedge will decrease your power by 20% but increase your power 
  by 10%. It is best for mid-ranged shots.\n"
let pitching_wedge_explanation = 
  "The pitching wedge will decrease your power by 30% but increase your 
  accuracy by 50%. It is best used for short to mid-ranged shots.\n"
let three_wood_explanation = "Similar to the driver, the three wood will 
  increase your power (by 30%) but with less of an impact on accurcy (only l
  osing 15% accuracy). It is best used for longer range shots.\n"
let four_hybrid_explanation = "The four hybrid will increase your power by
  20% but decrease your accuracy by 10%. It is best used for longer-medium 
  ranged shots\n"
let five_hybrid_explanation = "The five hybrid increases your power by 10%
  and decreases your accuracy by 5%. The five hybrid is a great 'in-between' 
  club for long and mid-range shots.\n"
let six_iron_explanation = "The six iron has no effect on your power but 
  will increase your accuracy by 5%. It is best used for mid-range shots."
let seven_iron_explanation = "The seven iron will decrease your power 
  by 5% but increase your accuracy by 7.5%. It is best used on mid-range shots
  where a high degree of precision is needed."

(* for player input help menu*)
let power_explanation = 
  "Power impacts how far the golf ball will travel on that swing. An average 
  player who hits the ball with 100 power on a 45 degree angle will travel
  roughly 270 yards using a club that has no power adjustment. \n"
let angle_explanation = 
  "Angle impacts how arced the path travelled by the golf ball will be. A 45 
  degree angle is the 'optimal' angle as it will result in the greatest
  distance travelled with power held constant. \n"
let alignment_explanation = 
  "Alignment impacts which direction the golf ball will travel on that swing. 
  A negative number represents aiming to the left of the hole, 0 represents 
  aiming directly at the hole, and a positive number represents aiming to the 
  right of the hole. \n"

(* for scoring help menu *)
let turn_explanation = 
  "On each turn, a player will be shown the layout of their course, including 
  their location, the location of the hole, and the locations of any obstacles. 
  They will then be prompted to enter input for their swing (club, power, 
  angle, alignment). The trajectory of the golf ball will be calculated and 
  their new location will be shown. The play will then transition to the next 
  player's turn. \n"
let par_explanation = 
  "Par is a traditional golf concept that we have implemented in this game. 
  By way of definition, par is the predetermined number of strokes that a 
  proficient golfer should require to complete a hole. \n"
let stroke_limit_explanation = 
  "To avoid excessively long games, after 10 strokes a player's attempt to get 
  the ball in the hole will be truncated and they will be given their current 
  score on that hole. \n"
let winning_a_hole_explanation = 
  "The player who gets their ball in the hole in the fewest number of strokes 
  is considered the winner for that hole. They will then be the player who 
  gets to go first on the next hole. \n"
let winning_the_game_explanation = 
  "Score is aggregated over all of the holes players. When all holes have 
  been played to completion, the winning player is the player with the lowest 
  overall score. \n"

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
  let selection = ref (read_line() |> parse |> string_catcher)in 
  while (!selection != 6) do 
    scoring_menu_parser (!selection);
    selection := (read_line() |> parse |> string_catcher)
  done;
  scoring_menu_parser 6


let club_menu_parser selection =
  match selection with 
  | 1 -> Printf.printf "Showing explanation for Driver... \n %s" 
           driver_explanation;
    Printf.printf "\n Please enter another selection from the club menu >";

  | 2 -> Printf.printf "Showing explanation for Nine Iron... \n %s" 
           nine_iron_explanation;
    Printf.printf "\n Please enter another selection from the club menu >";

  | 3 -> Printf.printf "Showing explanation for Eight Iron... \n %s" 
           eight_iron_explanation;
    Printf.printf "\n Please enter another selection from the club menu >";

  | 4 -> Printf.printf "Showing an explanation of Putter... \n %s" 
           putter_explanation;
    Printf.printf "\n Please enter another selection from the club menu >";

  | 5 -> Printf.printf "Showing an explanation of Pitching Wedge... \n %s" 
           pitching_wedge_explanation;
    Printf.printf "\n Please enter another selection from the club menu >";

  | 6 -> Printf.printf "Showing an explanation of Sand Wedge... \n %s" 
           sand_wedge_explanation;
    Printf.printf "\n Please enter another selection from the club menu >";

  | 7 -> Printf.printf "Showing an explanation of Three Wood... \n %s" 
           three_wood_explanation;
    Printf.printf "\n Please enter another selection from the club menu >";

  | 8 -> Printf.printf "Showing an explanation of Four Hybrid... \n %s" 
           four_hybrid_explanation;
    Printf.printf "\n Please enter another selection from the club menu >";

  | 9 -> Printf.printf "Showing an explanation of Five Hybrid... \n %s" 
           five_hybrid_explanation;
    Printf.printf "\n Please enter another selection from the club menu >";

  | 10 -> Printf.printf "Showing an explanation of Six Iron... \n %s" 
            six_iron_explanation;
    Printf.printf "\n Please enter another selection from the club menu >";

  | 11 -> Printf.printf "Showing an explanation of Seven Iron... \n %s" 
            seven_iron_explanation;
    Printf.printf "\n Please enter another selection from the club menu >";

  | 12 -> Printf.printf "Going back to the swing input menu... \n";
    ()

  | _ -> Printf.printf "Unknown selection, please enter another selection> ";
    ()


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
  Printf.printf "7) Three Wood \n";
  Printf.printf "8) Four Hybrid \n";
  Printf.printf "9) Five Hybrid \n";
  Printf.printf "10) Six Iron \n";
  Printf.printf "11) Seven Iron\n";
  Printf.printf "12) Go back to the swing input menu \n";
  Printf.printf "Enter Number > ";
  let selection = ref (read_line() |> parse |> string_catcher)in 
  while (!selection != 12) do 
    club_menu_parser (!selection);
    selection := (read_line() |> parse |> string_catcher)
  done;
  club_menu_parser 12

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
  swing_input_menu_disp(false);
  let selection = ref (read_line() |> parse |> string_catcher )in 
  while (!selection != 5) do 
    swing_input_menu_parser (!selection);
    if (!selection == 1) 
    then swing_input_menu_disp (true);
    selection := (read_line() |> parse |> string_catcher )
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
  | 6 -> Printf.printf "Going back to the main menu... \n"; ()
  | _ -> Printf.printf "Unknown selection, please enter another selection> ";
    ()

let player_input_menu_rep () = 
  Printf.printf "1) Number of players \n";
  Printf.printf "2) Names \n";
  Printf.printf "3) Beginner, Intermediate, Advanced Options \n";
  Printf.printf "4) Below Average, average, above average strength options \n";
  Printf.printf "5) Handicap \n";
  Printf.printf "6) Go back to the main menu \n";
  Printf.printf "Enter Number > ";
  let selection = ref (read_line() |> parse |> string_catcher )in 
  while (!selection != 6) do 
    player_input_menu_parser (!selection);
    selection := (read_line() |> parse |> string_catcher)
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
  | 2 -> Printf.printf "Showing course selection explanation... \n \n %s" 
           course_selection_explanation;
    Printf.printf "\n Please enter another selection from the menu >";
  | 3 -> Printf.printf "Going to the player initialization input help menu... \n";
    player_input_menu_init()
  | 4 -> Printf.printf "Going to the swing input help menu... \n";
    swing_input_menu_init()
  | 5 -> Printf.printf "Showing an explanation of who goes first... \n %s"
           who_goes_first;
    Printf.printf "\n Please enter another selection from the menu >";
  | 6 -> Printf.printf "Showing an explanation of scoring... \n"; 
    scoring_menu()
  | 7 -> Printf.printf "Going back to the game... \n"; ()
  | _ -> Printf.printf "Unknown selection, please enter another selection> "; 
    ()

let main_menu_disp (header : bool) = 
  if header then 
    Printf.printf "\n Welcome back to the main help menu! Please enter the number \n";
  if header then 
    Printf.printf "next to the topic you would like to learn more about! \n";
  Printf.printf "1) Game Explanation \n";
  Printf.printf "2) Course Selection Explanation \n";
  Printf.printf "3) Player Initialization Input \n";
  Printf.printf "4) Swing Input \n";
  Printf.printf "5) Who Goes First \n";
  Printf.printf "6) Scoring and Turns:\n";
  Printf.printf "7) Go back to the game \n";
  Printf.printf "Enter Number > ";
  ()

let main_menu_rep () =
  main_menu_disp(false);
  let selection = ref (read_line() |> parse |> string_catcher)in 
  while (!selection != 7) do
    main_menu_parser (!selection);
    if (!selection == 3 || !selection == 4 || !selection == 6) 
    then main_menu_disp (true);
    selection := (read_line() |> parse |> string_catcher);
  done;
  main_menu_parser 7

let help_menu_init () = 
  Printf.printf "Welcome to the help menu! Please enter the number next to \n";
  Printf.printf "the topic you would like to learn more about! \n";
  main_menu_rep()



