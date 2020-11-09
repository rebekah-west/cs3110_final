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



let help_menu () =
  Printf.printf "We are here to help. \n";
  Printf.printf "\nTable of Contents: \n %s"  table_of_contents;
  Printf.printf "\nGame Explanation: \n %s"  game_explanation;
  Printf.printf "\nPlayer Input: \n %s"  player_input;
  Printf.printf "\nSwing Input: \n %s" swing_input;
  Printf.printf "\nWho Goes First: \n %s" who_goes_first;
  Printf.printf "\nScoring Explanation: \n %s" scoring_explanation;



