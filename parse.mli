(** This module contains all data relevant to parsing. The commands within here
    are used throughout the modules for taking player input. *)

(** The type [parsable] represents a user input that we can parse
    to remove minor typing errors from and interpret in the game*)
type parsable = string 

(** [remove_blanks parsbl] removes any white space from a player input,
    a parsable represented by [parsbl] *)
val remove_blanks : parsable -> string

(** [parse parsbl] converts a parsable that has had its white space removed
    into a string that can be interpretted by the game *)
val parse : parsable -> string 

(* [string_catcher s message] checks if strnig [s] can be transformed into an int. If
   not, the user is prompted again with [message] *)
val string_catcher : string -> string -> int 

(* [for_int_output message] takes the message that is to be displayed to a 
   player [message] and takes care of things like the help menu and quitting
   in addition to parsing the user input. Used when the output is meant to 
   be an int*)
val for_int_output : string-> int

(* [for_int_output message] takes the message that is to be displayed to a 
   player [message] and takes care of things like the help menu and quitting
   in addition to parsing the user input. Used when the output is meant to 
   be a string*)
val for_string_output : string -> string

(** [pp_string s] pretty-prints string [s]. *)
val pp_string : string -> string

(** [pp_int (k,v)] pretty-prints the tuple [(k,v)]. *)
val pp_tup : float*float -> string

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
val pp_list : ('a -> string) -> 'a list -> string

(** [pp_array arr] pretty-prints the array [arr]. *)
val pp_array : ('a -> string) -> 'a array -> string

