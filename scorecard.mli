(** 
   Representation of the scorecard printed to the terminal.

   This module represents the functions used to print the current scorecard to 
   the terminal for the user. This should occur at the end of every hole
*)

(** [scorecard_printer g c] prints a visual representation of the scorecard
    from game [g] and course [c] to the terminal *)
val scorecard_printer: Game.t ->  Course.t -> unit