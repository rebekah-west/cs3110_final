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

(** [init_scorecard p c] is the initial state of the scorecard 
    when players p play a game on course c. In this game the players are 
    currently located at hole 1, and they all have scores of 0 *)
val init_game: Player.t list -> Course.hole list -> t

(** [current_hole gm] is the identifier of the hole on which the players are
    currently located in game [gm]. *)
val current_hole: t -> Course.hole_number

(** [played gm] is the list of all the holes of game [gm] the players 
    have already completed*)
val played: t -> Course.hole_number list

(** [current_turn gm] is the player who is currently up to swing.  *)
val current_turn: t -> Player.t 

(** [update turn gm] is attempting to update the turn after a player swings. 
    During the first hole, players go in order of lineup. After that, player 
    furthest from hole is up. At the start of any other hole, the player with
    honors, who won the last hole, wins *)
val update_turn: t -> t

(** [current_score gm] returns the scorecord object for the game at its current 
    state *)
val current_score: t -> scorecard 

(** UNSURE IF THIS IS NEEDED?
    the type representing the outcome of a hole*)
(* type outcome = Win of t | Lose of t *)

(** [winner_of_hole gm] returns the player who won the hole just played *)
val winner_of_hole: t -> Player.t 

(** [winner_of_game gm] returns the player who won the game of golf *)
val winner_of_game: t -> Player.t