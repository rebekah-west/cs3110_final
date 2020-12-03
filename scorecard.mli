(** 
   Representation of the scorecard printed to the terminal.

   This module represents the functions used to print the current scorecard to 
   the terminal for the user. This should occur at the end of every hole
*)

(** [print_loc] prints a visual representation of the scorecard
    to the terminal *)
val scorecard_printer: Game.t ->  Course.t -> unit