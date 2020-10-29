(** This module contains all data relevant to parsing. The commands within here
    are used throughout the modules for taking player input. *)

(*The type [parsable] represents a user input that we can parse
  to remove minor typing errors from and interpret in the game*)
type parsable = string 

(* [remove_blanks parsbl] removes any white space from a player input,
   a parsable represented by [parsbl] *)
val remove_blanks : parsable -> string

(* [parse parsbl] converts a parsable that has had its white space removed
   into a string that can be interpretted by the game *)
val parse : parsable -> string 