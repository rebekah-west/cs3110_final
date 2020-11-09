(*********************************************************************)
(** Jenna *)
(*********************************************************************)

(** 
   Representation of a game being played.

   This module represents the scorecard for a specific game, as well as 
   tracks game functionality such as whose turn is next and who is winnning
*)

(** The abstract type of values representing games. *)
type t

(** the type of the game scorecard *)
type scorecard

(** the type of the a score for one player at one hole*)
type hole_score

(* exception if the hole is not a valid representation *)
exception InvalidHole

(*the exception if the score is not the actual score of a player in the game *)
exception InvalidScore

(** [init_scorecard p c] is the initial state of the scorecard 
    when players p play a game on course c. In this game the players are 
    currently located at hole 1, and they all have scores of 0 *)
val init_game: Player.t array -> Course.t -> t

(** [current_hole gm] is the identifier of the hole on which the players are
    currently located in game [gm]. *)
val current_hole: t -> Course.hole_number

(** [played gm] is the list of all the holes of game [gm] the players 
    have already completed*)
val played: t -> Course.hole_number list

(** [current_turn gm] is the player who is currently up to swing.  *)
val current_turn: t -> Player.t 

(** [current_score gm] returns the scorecord object for the game at its current 
    state *)
val current_score: t -> scorecard 

(** [game_roster gm] returns the list of players playing the game *)
val game_roster: t -> Player.t array

(** [current_course gm] returns the course being played on *)
val current_course : t -> Course.t

(** [update_score gm] takes a game and returns the updated scorecard
    for when a player takes a swing *)
val update_score: t -> scorecard

(** [update turn gm] is attempting to update the turn after a player swings. 
    During the first hole, players go in order of lineup. After that, player 
    furthest from hole is up. At the start of any other hole, the player with
    honors, who won the last hole, wins *)
val update_turn: t -> Course.hole -> Player.t

(** [winner_of_hole gm] returns the player who won the hole just 
    played, If it is a tie, all players with lowest score listed *)
val winner_of_hole: t -> Course.hole_number -> Player.t array

(** [winner_of_game gm] returns the player or players 
    who won the game of golf *)
val winner_of_game: t -> Player.t array

(** [update_roster r p] takes in the game roster and the new updated 
    player that should be updated in the roster*)
val update_roster: Player.t array -> t -> Player.t array

(** [play_hole t] plays the current hole, including prompting each player to
    swing, updating their location and score, and changing the current hole
    to the next one *)
val play_hole: t -> t

(** [print_scorecard t] prints the scorecard for all players for a specific 
    hole into the terminal*)
val print_scorecard: t -> unit