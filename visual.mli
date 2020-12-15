(** 
   Representation of the visuals printed to the terminal.

   This module represents the functions used to print orienting visuals to 
   the terminal for the user. This should occur at the beginning of each hole, 
   whenever someone types 'location', and at the beginning and end of each 
   swing.
*)


(** [print_loc] prints a visual of the hole, obstacles, and player 
    location to the terminal *)
val print_loc: (float*float) -> (float*float) -> (float*float*string) list 
  -> unit

(** [congrats ()] prints a congratulatory message when you enter the hole *)
val congrats: unit -> unit